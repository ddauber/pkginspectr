#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions were renamed in pkginspectr 0.2.0 so that all exported
#' functions share the `pkg_` prefix and are named after what they return:
#'
#' * `inspect_package()` is now [pkg_structure()].
#' * `get_code_stats()` is now [pkg_lines()].
#' * `plot_pkg()` is now [pkg_plot()].
#' * `clear_pkg_cache()` is now [pkg_cache_clear()].
#'
#' The old names still work but warn. `get_code_stats()` keeps returning its
#' original columns; use [pkg_lines()] for the full, non-overlapping
#' breakdown.
#'
#' @param path,include_ignored See [pkg_lines()].
#' @param x,... See [pkg_plot()]. Arguments that `pkg_plot()` does not
#'   accept are ignored with a warning.
#' @param type See [pkg_plot()] or [pkg_cache_clear()].
#' @keywords internal
#' @name pkginspectr-deprecated
NULL

#' @rdname pkginspectr-deprecated
#' @export
inspect_package <- function(path = ".", include_ignored = FALSE) {
  lifecycle::deprecate_warn("0.2.0", "inspect_package()", "pkg_structure()")
  pkg_structure(path, include_ignored = include_ignored)
}

#' @rdname pkginspectr-deprecated
#' @export
get_code_stats <- function(path = ".", include_ignored = FALSE) {
  lifecycle::deprecate_warn("0.2.0", "get_code_stats()", "pkg_lines()")
  result <- pkg_lines(path, include_ignored = include_ignored)
  new_pkginspectr_tbl(result[c(
    "package", "version", "total_lines", "blank_lines", "comment_lines",
    "roxygen_lines", "function_lines", "test_lines"
  )])
}

#' @rdname pkginspectr-deprecated
#' @export
plot_pkg <- function(x, type = c("package", "metric"), ...) {
  lifecycle::deprecate_warn("0.2.0", "plot_pkg()", "pkg_plot()")
  args <- list(...)
  unknown <- setdiff(names(args), names(formals(pkg_plot)))
  if (length(unknown) > 0) {
    cli::cli_warn("Ignoring argument{?s} no longer supported: {.arg {unknown}}.")
    args <- args[setdiff(names(args), unknown)]
  }
  do.call(pkg_plot, c(list(x, type = type), args))
}

#' @rdname pkginspectr-deprecated
#' @export
clear_pkg_cache <- function(type = c("session", "persistent")) {
  lifecycle::deprecate_warn("0.2.0", "clear_pkg_cache()", "pkg_cache_clear()")
  pkg_cache_clear(type)
}
