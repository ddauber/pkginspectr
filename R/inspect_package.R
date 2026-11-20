#' Inspect packages for structure and file types
#'
#' Examine the file structure of one or more packages and return summary
#' statistics including file counts, whether R files define functions and the
#' package version.
#'
#' Only files that `R CMD build` would ship are counted: version-control
#' folders (e.g. `.git/`), R's built-in build exclusions (e.g. `.Rproj.user/`,
#' `.Rhistory`) and, unless `include_ignored = TRUE`, anything listed in
#' `.Rbuildignore` (e.g. `docs/`, `data-raw/`). This makes results for a local
#' folder comparable with results for a CRAN tarball.
#'
#' @inheritParams get_code_stats
#'
#' @return A `pkginspectr_tbl` with one row per package and columns:
#'   * `package`, `version`
#'   * `num_files`, `num_dirs`: files and directories in the package.
#'   * `num_r_files`: R source files (`.R` or `.r`) anywhere in the package.
#'   * `num_function_r_files`, `num_non_function_r_files`: R files that do /
#'     do not define at least one function (detected by the R parser).
#'   * `num_test_files`: R files under `tests/`.
#'   * `num_vignettes`: `.Rmd`, `.qmd` or `.Rnw` files under `vignettes/`.
#'   * `num_docs`: `.Rd` files under `man/`.
#'   * `num_data_files`: data files under `data/` (`.rda`, `.RData`, `.rds`,
#'     `.csv`, `.tsv`, `.tab`, `.txt`, optionally compressed).
#' @export
#'
#' @examples
#' inspect_package(pkginspectr_example())
#'
#' @examplesIf interactive()
#' # GitHub repositories and CRAN packages are downloaded and cached
#' inspect_package(c("r-lib/cli", "desc"))
inspect_package <- function(path = ".", include_ignored = FALSE) {
  paths <- resolve_package_path(path)

  result <- purrr::map(paths, function(pkg_path) {
    if (!fs::dir_exists(pkg_path)) {
      cli::cli_abort("The path {.path {pkg_path}} does not exist.")
    }

    listing <- list_pkg_files(pkg_path, include_ignored)
    files <- listing$files

    pkg_info <- get_pkg_name_version(pkg_path)
    pkg_name <- pkg_info$name
    pkg_version <- pkg_info$version

    cli::cli_inform("Inspecting {.pkg {pkg_name}}...")

    # All classification uses paths relative to the package root.
    r_files <- files[is_r_file(files)]
    test_files <- r_files[startsWith(r_files, "tests/")]
    vignette_files <- files[grepl(
      "^vignettes/.*\\.(rmd|qmd|rnw)$",
      files,
      ignore.case = TRUE
    )]
    doc_files <- files[grepl("^man/.*\\.Rd$", files, ignore.case = TRUE)]
    data_files <- files[grepl(
      "^data/.*\\.(rda|rdata|rds|csv|tsv|tab|txt)(\\.(gz|bz2|xz))?$",
      files,
      ignore.case = TRUE
    )]

    has_function <- purrr::map_lgl(r_files, function(rel) {
      analyse_r_file(fs::path(pkg_path, rel), label = rel)$has_function
    })

    dplyr::tibble(
      package = pkg_name,
      version = pkg_version,
      num_files = length(files),
      num_dirs = length(listing$dirs),
      num_r_files = length(r_files),
      num_function_r_files = sum(has_function),
      num_non_function_r_files = sum(!has_function),
      num_test_files = length(test_files),
      num_vignettes = length(vignette_files),
      num_docs = length(doc_files),
      num_data_files = length(data_files)
    )
  }) |>
    purrr::list_rbind()

  class(result) <- c("pkginspectr_tbl", class(result))
  result
}
