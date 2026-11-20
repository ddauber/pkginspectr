#' Summarise package structure and lines of code
#'
#' Combine [pkg_structure()] and [pkg_lines()] into one table. Each package is
#' resolved (and, for GitHub or CRAN, downloaded) only once, so this is the
#' quickest way to get everything about a set of packages.
#'
#' @inheritParams pkg_lines
#'
#' @return A `pkginspectr_tbl` with one row per package: `package`, `version`,
#'   then the columns of [pkg_structure()] followed by those of [pkg_lines()].
#' @seealso [pkg_plot()] to visualise the result.
#' @export
#'
#' @examples
#' pkg_summary(pkginspectr_example())
#'
#' @examplesIf interactive()
#' pkg_summary(c("r-lib/cli", "desc")) |>
#'   pkg_plot(type = "metric")
pkg_summary <- function(path = ".", include_ignored = FALSE, quiet = FALSE) {
  map_packages(
    path,
    function(pkg_path, info) {
      structure <- structure_one(pkg_path, info, include_ignored)
      lines <- lines_one(pkg_path, info, include_ignored)
      dplyr::bind_cols(structure, lines[setdiff(names(lines), names(structure))])
    },
    verb = "Summarising",
    quiet = quiet
  )
}
