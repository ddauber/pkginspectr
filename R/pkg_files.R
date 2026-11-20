#' List the files and directories of a package
#'
#' Walks a package directory and returns paths *relative to the package root*,
#' so that classification never depends on where the package lives on disk.
#' Mirrors what `R CMD build` would ship: version-control folders, R's
#' built-in build exclusions and (unless `include_ignored = TRUE`) anything
#' matched by `.Rbuildignore` are dropped. Ignored top-level folders are not
#' descended into, so large `.git/`, `docs/` or `revdep/` folders cost nothing.
#'
#' @param pkg_path Path to the package root.
#' @param include_ignored If `TRUE`, `.Rbuildignore` is not applied.
#' @return A list with character vectors `files` and `dirs`.
#' @keywords internal
#' @noRd
list_pkg_files <- function(pkg_path, include_ignored = FALSE) {
  patterns <- build_exclusion_patterns(pkg_path, include_ignored)

  walk <- function(dir, recurse) {
    entries <- tryCatch(
      fs::dir_ls(
        dir,
        recurse = recurse,
        all = TRUE,
        type = c("file", "directory")
      ),
      error = function(e) {
        cli::cli_abort("Failed to list files in {.path {pkg_path}}.", parent = e)
      }
    )
    rel <- as.character(fs::path_rel(entries, pkg_path))
    is_dir <- unname(fs::is_dir(entries))
    keep <- !is_build_excluded(rel, is_dir, patterns)
    list(rel = rel[keep], is_dir = is_dir[keep])
  }

  top <- walk(pkg_path, recurse = FALSE)
  nested <- lapply(
    fs::path(pkg_path, top$rel[top$is_dir]),
    walk,
    recurse = TRUE
  )

  rel <- c(top$rel, unlist(lapply(nested, `[[`, "rel")))
  is_dir <- c(top$is_dir, unlist(lapply(nested, `[[`, "is_dir")))

  list(files = sort(rel[!is_dir]), dirs = sort(rel[is_dir]))
}

#' Regex patterns `R CMD build` uses to drop files
#'
#' Copied from `tools:::get_exclude_patterns()` (R 4.x) so that local folders
#' and CRAN tarballs are inspected on the same footing.
#' @noRd
r_build_exclude_patterns <- c(
  "^\\.Rbuildignore$",
  "(^|/)\\.DS_Store$",
  "^\\.(RData|Rhistory)$",
  "~$",
  "\\.bak$",
  "\\.sw.$",
  "(^|/)\\.#[^/]*$",
  "(^|/)#[^/]*#$",
  "^TITLE$",
  "^data/00Index$",
  "^inst/doc/00Index\\.dcf$",
  "^config\\.(cache|log|status)$",
  "(^|/)autom4te\\.cache$",
  "^src/.*\\.d$",
  "^src/Makedeps$",
  "^src/so_locations$",
  "^inst/doc/Rplots\\.(ps|pdf)$",
  "^(GPATH|GRTAGS|GTAGS)$"
)

#' Directory names `R CMD build` always drops (`tools:::.vc_dir_names`)
#' @noRd
r_build_vc_dirs <- c(
  "CVS", ".svn", ".arch-ids", ".bzr", ".git", ".hg", "_darcs", ".metadata"
)

#' File names `R CMD build` always drops (`tools:::.hidden_file_exclusions`)
#' @noRd
r_build_hidden_files <- c(
  ".Renviron", ".Rprofile", ".Rproj.user", ".Rhistory", ".Rapp.history",
  ".tex", ".log", ".aux", ".pdf", ".png", ".backups", ".cvsignore",
  ".cproject", ".directory", ".dropbox", ".exrc", ".gdb.history",
  ".gitattributes", ".gitignore", ".gitmodules", ".hgignore", ".hgtags",
  ".htaccess", ".latex2html-init", ".project", ".seed", ".settings",
  ".tm_properties"
)

#' Collect exclusion regexes for a package
#' @noRd
build_exclusion_patterns <- function(pkg_path, include_ignored = FALSE) {
  patterns <- r_build_exclude_patterns
  ignore_file <- fs::path(pkg_path, ".Rbuildignore")
  if (!include_ignored && fs::file_exists(ignore_file)) {
    patterns <- c(patterns, readLines(ignore_file, warn = FALSE))
  }
  patterns[nzchar(patterns)]
}

#' Is a relative path (or any of its parent folders) excluded from the build?
#'
#' `R CMD build` deletes an excluded directory together with its contents, so
#' a path is excluded when the path itself or any ancestor matches.
#' @noRd
is_build_excluded <- function(rel, is_dir, patterns) {
  if (length(rel) == 0) {
    return(logical())
  }

  # Ancestors are directories by construction; the final element keeps its type.
  ancestors <- lapply(strsplit(rel, "/", fixed = TRUE), function(parts) {
    vapply(
      seq_along(parts),
      function(i) paste(parts[seq_len(i)], collapse = "/"),
      character(1)
    )
  })

  candidates <- unique(unlist(ancestors))
  hit <- rep(FALSE, length(candidates))
  for (p in patterns) {
    hit <- hit | grepl(p, candidates, perl = TRUE, ignore.case = TRUE)
  }
  base <- basename(candidates)
  hit <- hit | base %in% r_build_hidden_files | startsWith(base, "._")

  excluded <- candidates[hit]
  # Version-control names only count as directories.
  vc <- candidates[base %in% r_build_vc_dirs]
  vc <- vc[vc %in% unlist(lapply(ancestors, utils::head, -1L)) |
    vc %in% rel[is_dir]]

  vapply(
    ancestors,
    function(a) any(a %in% excluded) || any(a %in% vc),
    logical(1)
  )
}

#' Detect R source files (`.R` or `.r`) from relative paths
#' @noRd
is_r_file <- function(rel) {
  grepl("\\.[Rr]$", rel)
}
