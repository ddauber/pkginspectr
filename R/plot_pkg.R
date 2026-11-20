#' Plot package statistics in the console
#'
#' Plots data retrieved from [get_code_stats()], [inspect_package()]
#' and returns the results in the console.
#'
#' @param x A tibble from [get_code_stats()], [inspect_package()] or a combined
#'   result.
#' @param type One of `"package"` or `"metric"`. `"package"` shows a
#'   per-package breakdown; `"metric"` compares packages across each metric.
#' @param ... Additional arguments passed to the plotting functions.
#'
#' @return Invisibly returns `x`.
#' @export
#'
#' @examples
#' stats <- get_code_stats(pkginspectr_example())
#'
#' # One chart per package
#' plot_pkg(stats)
#'
#' # One chart per metric, comparing packages
#' plot_pkg(stats, type = "metric")
plot_pkg <- function(x, type = c("package", "metric"), ...) {
  type <- match.arg(type)

  if (identical(type, "package")) {
    return(invisible(plot_pkginspect_by_package(x, ...)))
  }

  invisible(plot_pkginspect_by_metric(x, ...))
}
