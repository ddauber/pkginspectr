#' Resolve package specifications to local package directories
#'
#' Accepts local folders, GitHub repositories and CRAN packages (see the
#' `path` argument of [pkg_lines()] for the syntax) and returns a local
#' directory for each, downloading and caching remote packages as needed.
#'
#' @param path Character vector of package specifications.
#' @param cache `"session"`, `"persistent"` or `"none"`; see [pkg_lines()].
#' @return A character vector of absolute paths to package directories.
#' @keywords internal
#' @noRd
resolve_package_path <- function(
  path,
  cache = c("session", "persistent", "none")
) {
  cache <- match.arg(cache)
  if (is.list(path)) {
    path <- unlist(path)
  }
  if (!is.character(path) || length(path) == 0 || anyNA(path)) {
    cli::cli_abort(
      "{.arg path} must be a character vector of local folders, GitHub repos or CRAN package names."
    )
  }

  paths <- purrr::map_chr(path, function(p) {
    spec <- parse_pkg_spec(p)
    switch(
      spec$source,
      local = as.character(fs::path_abs(spec$path)),
      github = resolve_github(spec, cache),
      cran = resolve_cran(spec, cache)
    )
  })

  fs::path_abs(paths)
}

# Parsing ----------------------------------------------------------------------

#' Parse one package specification
#'
#' Explicit prefixes (`local::`, `github::`, `cran::`) always win. Without a
#' prefix, an existing folder is local, a github.com URL or `owner/repo` is
#' GitHub, and a bare package name is CRAN. Anything that looks like a file
#' path but does not exist is an error, rather than a surprise GitHub request.
#'
#' @param x A single specification string.
#' @return A list with `source` (`"local"`, `"github"` or `"cran"`), `spec`
#'   (the original string) and source-specific fields.
#' @keywords internal
#' @noRd
parse_pkg_spec <- function(x) {
  if (startsWith(x, "local::")) {
    path <- sub("^local::", "", x)
    if (!fs::dir_exists(path)) {
      cli::cli_abort("Could not resolve {.val {x}}: no local folder {.path {path}}.")
    }
    return(list(source = "local", spec = x, path = path))
  }
  if (startsWith(x, "github::")) {
    return(parse_github_spec(sub("^github::", "", x), x))
  }
  if (startsWith(x, "cran::")) {
    return(parse_cran_spec(sub("^cran::", "", x), x))
  }
  if (grepl("^[A-Za-z]+::", x)) {
    cli::cli_abort(c(
      "Unknown source prefix in {.val {x}}.",
      "i" = "Use {.code local::}, {.code github::} or {.code cran::}."
    ))
  }

  if (fs::dir_exists(x)) {
    return(list(source = "local", spec = x, path = x))
  }
  if (grepl("^https?://(www\\.)?github\\.com/", x)) {
    return(parse_github_spec(x, x))
  }
  if (looks_like_local_path(x)) {
    cli::cli_abort(c(
      "Could not resolve path {.val {x}}: no such local folder.",
      "i" = "For GitHub use {.code github::owner/repo}; for CRAN use {.code cran::pkg}."
    ))
  }
  if (grepl("^[^/@]+/[^/@]+(@.+)?$", x)) {
    return(parse_github_spec(x, x))
  }
  if (grepl("^[A-Za-z][A-Za-z0-9.]*[A-Za-z0-9](@.+)?$", x)) {
    return(parse_cran_spec(x, x))
  }

  cli::cli_abort(c(
    "Could not resolve path {.val {x}}: must be a local folder, GitHub repo or CRAN package name.",
    "i" = "Examples: {.val .}, {.val r-lib/cli}, {.val desc}."
  ))
}

#' Strings that are clearly meant as file paths
#'
#' Absolute or relative paths, home-directory paths, Windows paths, and
#' anything with more than one `/` (GitHub subfolders need `github::`).
#' @noRd
looks_like_local_path <- function(x) {
  without_ref <- sub("@.*$", "", x)
  grepl("^(\\.|/|~|[A-Za-z]:)", x) ||
    grepl("\\", x, fixed = TRUE) ||
    lengths(strsplit(without_ref, "/", fixed = TRUE)) > 2
}

#' Parse `owner/repo[/subdir][@ref]` or a github.com URL
#' @noRd
parse_github_spec <- function(x, spec) {
  is_url <- grepl("^https?://", x)
  x <- sub("^https?://(www\\.)?github\\.com/", "", x)
  x <- sub("/+$", "", x)
  x <- sub("\\.git$", "", x)

  ref <- NULL
  if (grepl("@", x, fixed = TRUE)) {
    ref <- sub("^[^@]*@", "", x)
    x <- sub("@.*$", "", x)
  }

  parts <- strsplit(x, "/", fixed = TRUE)[[1]]
  # Browser URLs: github.com/owner/repo/tree/<ref>/<subdir>
  if (is_url && length(parts) >= 4 && parts[3] == "tree") {
    ref <- ref %||% parts[4]
    parts <- parts[-(3:4)]
  }

  valid <- length(parts) >= 2 &&
    all(nzchar(parts)) &&
    grepl("^[A-Za-z0-9-]+$", parts[1]) &&
    grepl("^[A-Za-z0-9._-]+$", parts[2]) &&
    (is.null(ref) || nzchar(ref))
  if (!valid) {
    cli::cli_abort(c(
      "Invalid GitHub repository {.val {spec}}.",
      "i" = "Use {.code owner/repo}, optionally followed by {.code /subdir} and {.code @ref}."
    ))
  }

  list(
    source = "github",
    spec = spec,
    owner = parts[1],
    repo = parts[2],
    subdir = if (length(parts) > 2) paste(parts[-(1:2)], collapse = "/"),
    ref = ref
  )
}

#' Parse `pkg[@version]`
#' @noRd
parse_cran_spec <- function(x, spec) {
  name <- sub("@.*$", "", x)
  version <- if (grepl("@", x, fixed = TRUE)) sub("^[^@]*@", "", x)

  if (!grepl("^[A-Za-z][A-Za-z0-9.]*[A-Za-z0-9]$", name)) {
    cli::cli_abort("Invalid CRAN package name in {.val {spec}}.")
  }
  if (!is.null(version) && !grepl("^[0-9]+([.-][0-9]+)+$", version)) {
    cli::cli_abort(c(
      "Invalid version in {.val {spec}}.",
      "i" = "Use a version number such as {.code cran::desc@1.4.0}."
    ))
  }

  list(source = "cran", spec = spec, name = name, version = version)
}

#' Does a GitHub ref name a fixed version (commit SHA or version tag)?
#'
#' Only fixed versions are kept in the persistent cache; branches move, so
#' caching them across sessions would go stale.
#' @noRd
is_pinned_ref <- function(ref) {
  !is.null(ref) &&
    (grepl("^[0-9a-f]{7,40}$", ref) || grepl("^v?[0-9]+(\\.[0-9]+)+", ref))
}

# GitHub -----------------------------------------------------------------------

#' Download (or reuse) a GitHub repository and return the package directory
#' @noRd
resolve_github <- function(spec, cache) {
  ref <- spec$ref %||% "HEAD"
  repo <- paste0(spec$owner, "/", spec$repo)
  label <- if (is.null(spec$ref)) repo else paste0(repo, "@", ref)

  root <- cache_root(cache, pinned = is_pinned_ref(spec$ref))
  dest <- fs::path(root, "github", spec$owner, spec$repo, safe_dir_name(ref))
  url <- paste0("https://github.com/", repo, "/archive/", ref, ".tar.gz")

  pkg_dir <- fetch_cached(url, dest, paste(label, "from GitHub"))

  if (!is.null(spec$subdir)) {
    pkg_dir <- fs::path(pkg_dir, spec$subdir)
    if (!fs::dir_exists(pkg_dir)) {
      cli::cli_abort("Folder {.path {spec$subdir}} not found in {.val {label}}.")
    }
  }
  as.character(pkg_dir)
}

# CRAN -------------------------------------------------------------------------

#' Download (or reuse) a CRAN package and return the package directory
#'
#' Current releases come from `src/contrib`, older or archived ones from
#' `src/contrib/Archive`.
#' @noRd
resolve_cran <- function(spec, cache) {
  name <- spec$name
  repos <- cran_repos()
  db <- cran_available(repos)
  current <- if (name %in% rownames(db)) db[name, "Version"] else NA_character_
  version <- spec$version %||% current

  if (is.na(version) || !identical(version, current)) {
    archived <- cran_archive_versions(name, repos)
    if (is.na(current) && length(archived) == 0) {
      cli::cli_abort("Package {.pkg {name}} not found on CRAN.")
    }
    if (is.na(version)) {
      version <- as.character(max(package_version(archived)))
      cli::cli_inform(
        "{.pkg {name}} is archived on CRAN; using its last release {version}."
      )
    } else if (!version %in% archived) {
      available <- c(archived, if (!is.na(current)) current)
      recent <- utils::head(
        as.character(sort(package_version(available), decreasing = TRUE)),
        5
      )
      cli::cli_abort(c(
        "Version {version} of {.pkg {name}} not found on CRAN.",
        "i" = "Most recent versions: {.val {recent}}."
      ))
    }
  }

  tarball <- paste0(name, "_", version, ".tar.gz")
  url <- if (identical(version, current)) {
    paste0(repos, "/src/contrib/", tarball)
  } else {
    paste0(repos, "/src/contrib/Archive/", name, "/", tarball)
  }

  root <- cache_root(cache, pinned = TRUE)
  dest <- fs::path(root, "cran", name, version)
  as.character(fetch_cached(url, dest, paste(name, version, "from CRAN")))
}

#' The CRAN mirror to use, falling back to the cloud mirror
#'
#' Plain `Rscript` sessions often have `repos` set to `"@CRAN@"`, which
#' cannot be downloaded from.
#' @noRd
cran_repos <- function() {
  repos <- getOption("repos")
  cran <- if ("CRAN" %in% names(repos)) repos[["CRAN"]] else NA_character_
  if (is.na(cran) || !nzchar(cran) || cran == "@CRAN@") {
    cran <- "https://cloud.r-project.org"
  }
  sub("/+$", "", cran)
}

#' Versions of a package in CRAN's archive (empty if none)
#' @noRd
cran_archive_versions <- function(name, repos) {
  lines <- read_url_lines(paste0(repos, "/src/contrib/Archive/", name, "/"))
  pattern <- paste0(gsub(".", "\\.", name, fixed = TRUE), "_([0-9][0-9.-]*)\\.tar\\.gz")
  matches <- unlist(regmatches(lines, gregexpr(pattern, lines)))
  unique(sub(pattern, "\\1", matches))
}

# Download and cache -----------------------------------------------------------

#' Download a tarball into `dest` unless it is already cached
#'
#' The archive is downloaded and extracted in a staging folder next to
#' `dest` and only moved into place once complete, so an interrupted or
#' failed download never leaves a broken cache entry behind.
#'
#' @param url Tarball URL.
#' @param dest Cache directory that will hold the package root.
#' @param label Human-readable name for messages.
#' @return `dest`.
#' @noRd
fetch_cached <- function(url, dest, label) {
  if (fs::dir_exists(dest)) {
    cli::cli_inform("Using cached {label}.")
    return(dest)
  }

  cli::cli_inform("Downloading {label}...")
  parent <- fs::path_dir(dest)
  fs::dir_create(parent)
  staging <- fs::file_temp("staging-", tmp_dir = parent)
  fs::dir_create(staging)
  on.exit(fs::dir_delete(staging), add = TRUE)

  tarball <- fs::path(staging, "download.tar.gz")
  download_file(url, tarball, label)

  extracted <- fs::path(staging, "extracted")
  status <- utils::untar(tarball, exdir = extracted)
  top <- if (fs::dir_exists(extracted)) {
    fs::dir_ls(extracted, type = "directory")
  } else {
    character()
  }
  if (!identical(status, 0L) || length(top) != 1) {
    cli::cli_abort("Could not extract {label}: unexpected archive contents.")
  }

  fs::file_move(top, dest)
  dest
}

#' Where to cache a download
#'
#' `"persistent"` only applies to pinned versions; anything that can change
#' (a GitHub branch or default branch) uses the session cache instead.
#' @noRd
cache_root <- function(cache, pinned) {
  switch(
    cache,
    none = fs::file_temp("pkginspectr-"),
    session = get_cache_dir("session"),
    persistent = get_cache_dir(if (pinned) "persistent" else "session")
  )
}

#' Make a ref usable as a single directory name
#' @noRd
safe_dir_name <- function(x) {
  gsub("[^A-Za-z0-9._-]", "_", x)
}

# Network wrappers (mocked in tests) -------------------------------------------

#' Download a URL to a file, turning failures into a clear error
#' @noRd
download_file <- function(url, dest, label) {
  result <- tryCatch(
    utils::download.file(url, dest, quiet = TRUE, mode = "wb"),
    error = function(e) e,
    warning = function(w) w
  )
  if (inherits(result, "condition") || !identical(result, 0L)) {
    reason <- if (inherits(result, "condition")) {
      conditionMessage(result)
    } else {
      "download failed"
    }
    cli::cli_abort(c(
      "Could not download {label}.",
      "x" = "{reason}",
      "i" = "URL: {.url {url}}"
    ))
  }
  invisible(dest)
}

#' Source packages currently on CRAN (matrix with a `Version` column)
#' @noRd
cran_available <- function(repos) {
  tryCatch(
    utils::available.packages(
      repos = repos,
      type = "source",
      filters = "duplicates"
    ),
    error = function(e) {
      cli::cli_abort("Could not reach CRAN at {.url {repos}}.", parent = e)
    }
  )
}

#' Lines of a web page, or `character()` if it cannot be read
#' @noRd
read_url_lines <- function(url) {
  tryCatch(
    suppressWarnings(readLines(url, warn = FALSE)),
    error = function(e) character()
  )
}
