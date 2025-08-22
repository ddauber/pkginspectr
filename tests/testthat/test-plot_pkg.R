test_that("plot_pkg returns input invisibly", {
  x <- dplyr::tibble(package = "pkg1", total_lines = 1)
  class(x) <- c("pkginspectr_tbl", class(x))
  expect_identical(plot_pkg(x, type = "metric"), x)
  expect_identical(plot_pkg(x, type = "package"), x)
})
