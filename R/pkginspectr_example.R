#' Path to the example package bundled with pkginspectr
#'
#' pkginspectr ships a tiny package, `examplepkg`, with known contents. It lets
#' examples run offline and makes results easy to check by hand: its R files
#' use `<-`, `=` and `\(x)` function definitions, a lower-case `.r` test file,
#' and strings containing `#` and unbalanced braces.
#'
#' @return Path to the root directory of the example package.
#' @export
#'
#' @examples
#' pkginspectr_example()
#'
#' pkg_structure(pkginspectr_example())
pkginspectr_example <- function() {
  system.file("extdata", "examplepkg", package = "pkginspectr", mustWork = TRUE)
}
