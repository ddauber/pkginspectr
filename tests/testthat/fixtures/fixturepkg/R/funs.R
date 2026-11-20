#' Add one
#'
#' @param x A number.
#' @export
add_one <- function(x) {
  # increment
  x + 1
}

square = function(x) x^2

compose <- \(f, g) {
  function(x) f(g(x))
}

msg <- "not { balanced"
