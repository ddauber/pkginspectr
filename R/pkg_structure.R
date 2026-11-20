#' Count files and folders in packages
#'
#' Examine the file structure of one or more packages and count files,
#' folders, R scripts, tests, vignettes, help pages and data files.
#'
#' Only files that `R CMD build` would ship are counted: version-control
#' folders (e.g. `.git/`), R's built-in build exclusions (e.g. `.Rproj.user/`,
#' `.Rhistory`) and, unless `include_ignored = TRUE`, anything listed in
#' `.Rbuildignore` (e.g. `docs/`, `data-raw/`). This makes results for a local
#' folder comparable with results for a CRAN tarball.
#'
#' @inheritParams pkg_lines
#'
#' @return A `pkginspectr_tbl` with one row per package and columns:
#'   * `package`, `version`
#'   * `num_files`, `num_dirs`: files and folders in the package.
#'   * `num_r_files`: R source files (`.R` or `.r`) in `R/`.
#'   * `num_function_r_files`, `num_non_function_r_files`: files in `R/` that
#'     do / do not define at least one function (detected by the R parser).
#'     These two add up to `num_r_files`.
#'   * `num_test_files`: R files under `tests/`.
#'   * `num_vignettes`: `.Rmd`, `.qmd` or `.Rnw` files under `vignettes/`.
#'   * `num_docs`: `.Rd` help files under `man/`.
#'   * `num_data_files`: data files under `data/` (`.rda`, `.RData`, `.rds`,
#'     `.csv`, `.tsv`, `.tab`, `.txt`, optionally compressed).
#' @seealso [pkg_lines()] for line counts, [pkg_summary()] for both, and
#'   [pkg_plot()] to visualise the result.
#' @export
#'
#' @examples
#' pkg_structure(pkginspectr_example())
#'
#' @examplesIf interactive()
#' # GitHub repositories and CRAN packages are downloaded and cached
#' pkg_structure(c("r-lib/cli", "desc"))
pkg_structure <- function(
  path = ".",
  include_ignored = FALSE,
  quiet = FALSE,
  cache = c("session", "persistent", "none")
) {
  map_packages(
    path,
    function(pkg_path, info) structure_one(pkg_path, info, include_ignored),
    verb = "Inspecting",
    quiet = quiet,
    cache = cache
  )
}

#' File and folder counts for a single package
#'
#' @inheritParams lines_one
#' @return A one-row tibble (see [pkg_structure()]).
#' @keywords internal
#' @noRd
structure_one <- function(pkg_path, info, include_ignored = FALSE) {
  listing <- list_pkg_files(pkg_path, include_ignored)
  files <- listing$files

  # All classification uses paths relative to the package root.
  r_files <- files[is_r_file(files) & startsWith(files, "R/")]
  test_files <- files[is_r_file(files) & startsWith(files, "tests/")]
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

  has_function <- vapply(
    r_files,
    function(rel) {
      analyse_r_file(fs::path(pkg_path, rel), label = rel)$has_function
    },
    logical(1)
  )

  dplyr::tibble(
    package = info$name,
    version = info$version,
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
}
