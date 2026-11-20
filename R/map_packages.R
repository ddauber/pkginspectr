#' Apply a per-package function to one or more packages
#'
#' Shared driver for [pkg_structure()], [pkg_lines()] and [pkg_summary()]:
#' resolves every entry of `path` once (downloading GitHub or CRAN packages if
#' needed), reads each package's name and version once, and row-binds the
#' results into a `pkginspectr_tbl`.
#'
#' @param path Character vector of local paths, GitHub repos or CRAN names.
#' @param fun Function of `(pkg_path, info)` returning a one-row tibble, where
#'   `info` is the list returned by `get_pkg_name_version()`.
#' @param verb Progress message verb, e.g. `"Inspecting"`.
#' @param quiet If `TRUE`, suppress progress messages.
#' @return A `pkginspectr_tbl`.
#' @keywords internal
#' @noRd
map_packages <- function(path, fun, verb, quiet = FALSE) {
  with_quiet(quiet, {
    paths <- resolve_package_path(path)

    result <- purrr::map(paths, function(pkg_path) {
      if (!fs::dir_exists(pkg_path)) {
        cli::cli_abort("The path {.path {pkg_path}} does not exist.")
      }
      info <- get_pkg_name_version(pkg_path)
      cli::cli_inform("{verb} {.pkg {info$name}}...")
      fun(pkg_path, info)
    }) |>
      purrr::list_rbind()

    new_pkginspectr_tbl(result)
  })
}

#' Mark a tibble as a pkginspectr result
#' @noRd
new_pkginspectr_tbl <- function(x) {
  class(x) <- unique(c("pkginspectr_tbl", class(x)))
  x
}

#' Evaluate `expr`, suppressing messages when `quiet` is `TRUE`
#' @noRd
with_quiet <- function(quiet, expr) {
  if (isTRUE(quiet)) suppressMessages(expr) else expr
}
