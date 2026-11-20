#' Print headers for pkginspectr results
#'
#' pkginspectr results print like any tibble, with an extra first header
#' line that says what they contain, e.g.
#' `# pkginspectr: Lines of code, 3 packages (1 failed)`. The wording matches
#' the titles of [pkg_plot()].
#'
#' @param x A pkginspectr result.
#' @param ... Passed on to the tibble method.
#' @return A named character vector of header lines, see [pillar::tbl_sum()].
#' @keywords internal
#' @name pkginspectr-print
NULL

#' @rdname pkginspectr-print
#' @export
tbl_sum.pkginspectr_tbl <- function(x, ...) {
  c(pkginspectr = result_description(x), NextMethod())
}

#' @rdname pkginspectr-print
#' @export
tbl_sum.pkginspectr_functions <- function(x, ...) {
  n_packages <- length(unique(x$package))
  description <- paste0(
    "Functions, ",
    count_noun(nrow(x), "function"),
    " in ",
    count_noun(n_packages, "package")
  )
  c(pkginspectr = description, NextMethod())
}

#' Describe a package-level result: its kind, size and failures
#' @noRd
result_description <- function(x) {
  n <- nrow(x)

  # pkg_history(): one package across releases, with a date column
  if (all(c("package", "date") %in% names(x)) && length(unique(x$package)) == 1) {
    return(paste0(
      "Package history of ", x$package[[1]], ", ", count_noun(n, "release")
    ))
  }

  kind <- sub("^pkginspectr: ", "", default_plot_title(plot_blocks(x)))
  description <- paste0(kind, ", ", count_noun(n, "package"))

  failed <- count_failed_rows(x)
  if (failed > 0) {
    description <- paste0(description, " (", failed, " failed)")
  }
  description
}

#' Rows whose numeric columns are all `NA` (packages that failed)
#' @noRd
count_failed_rows <- function(x) {
  numeric <- vapply(x, is.numeric, logical(1))
  if (!any(numeric) || nrow(x) == 0) {
    return(0L)
  }
  sum(rowSums(!is.na(as.data.frame(x)[numeric])) == 0)
}

#' "1 package", "3 packages"
#' @noRd
count_noun <- function(n, noun) {
  paste0(n, " ", noun, if (n != 1) "s")
}
