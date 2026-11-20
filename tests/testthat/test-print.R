header <- function(x) pillar::tbl_sum(x)[["pkginspectr"]]

test_that("results print a header describing their contents", {
  path <- fixture_path()
  expect_equal(header(pkg_lines(path, quiet = TRUE)), "Lines of code, 1 package")
  expect_equal(header(pkg_structure(path, quiet = TRUE)), "Package structure, 1 package")
  expect_equal(
    header(pkg_summary(c(path, path), quiet = TRUE)),
    "Package summary, 2 packages"
  )
  expect_equal(
    header(pkg_functions(path, quiet = TRUE)),
    "Functions, 3 functions in 1 package"
  )
})

test_that("the header counts failed packages", {
  result <- suppressWarnings(pkg_lines(c(fixture_path(), "./missing"), quiet = TRUE))
  expect_equal(header(result), "Lines of code, 2 packages (1 failed)")
})

test_that("pkg_history results are described as a history", {
  x <- new_pkginspectr_tbl(dplyr::tibble(
    package = "desc",
    version = c("1.0.0", "1.4.3"),
    date = as.Date(c("2016-05-13", "2023-12-10")),
    total_lines = c(10L, 20L)
  ))
  expect_equal(header(x), "Package history of desc, 2 releases")
})

test_that("the header appears above tibble's own header when printing", {
  out <- capture_output(print(pkg_lines(fixture_path(), quiet = TRUE)))
  lines <- strsplit(out, "\n")[[1]]
  expect_match(lines[1], "^# pkginspectr: Lines of code, 1 package$")
  expect_match(lines[2], "^# A tibble:")
})

test_that("derived tables keep a sensible header", {
  x <- pkg_lines(fixture_path(), quiet = TRUE)
  ratios <- dplyr::transmute(x, package, test_to_code = test_lines / r_lines)
  expect_equal(header(ratios), "Package statistics, 1 package")
  expect_equal(header(x[0, ]), "Lines of code, 0 packages")
})
