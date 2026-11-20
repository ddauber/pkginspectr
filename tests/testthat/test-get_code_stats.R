test_that("get_code_stats returns exact counts for the fixture package", {
  result <- suppressMessages(get_code_stats(fixture_path()))

  expect_s3_class(result, "pkginspectr_tbl")
  expect_equal(nrow(result), 1)
  expect_equal(result$package, "examplepkg")
  expect_equal(result$version, "1.2.3")
  expect_equal(result$total_lines, 32)
  expect_equal(result$blank_lines, 5)
  expect_equal(result$comment_lines, 2)
  expect_equal(result$roxygen_lines, 4)
  expect_equal(result$function_lines, 8)
  expect_equal(result$test_lines, 7)
})

test_that("get_code_stats respects .Rbuildignore unless include_ignored = TRUE", {
  pkg <- local_fixture_copy()
  fs::dir_create(fs::path(pkg, "data-raw"))
  writeLines(c("x <- 1", "y <- 2"), fs::path(pkg, "data-raw", "make.R"))
  writeLines("^data-raw$", fs::path(pkg, ".Rbuildignore"))

  ignored <- suppressMessages(get_code_stats(pkg))
  included <- suppressMessages(get_code_stats(pkg, include_ignored = TRUE))
  expect_equal(ignored$total_lines, 32)
  expect_equal(included$total_lines, 34)
})

test_that("get_code_stats errors on unresolvable path", {
  withr::local_options(cli.num_colors = 1)
  fake <- tempfile("does_not_exist_")
  expect_error(
    get_code_stats(fake),
    regexp = "Could not resolve path .* must be a local folder",
    class = "purrr_error_indexed"
  )
})

test_that("get_code_stats warns and returns 0s when no .R files found", {
  pkg_path <- withr::local_tempdir()
  fs::dir_create(fs::path(pkg_path, c("R", "tests", "data-raw")))
  writeLines(c("Package: empty", "Version: 0.0.1"), fs::path(pkg_path, "DESCRIPTION"))
  expect_warning(
    result <- suppressMessages(get_code_stats(pkg_path)),
    "No .R files found"
  )
  expect_s3_class(result, "tbl_df")
  expect_equal(result$version, "0.0.1")
  expect_equal(result$total_lines, 0)
  expect_equal(result$function_lines, 0)
  expect_equal(result$test_lines, 0)
})

# count_lines() ---------------------------------------------------------------

test_that("count_lines detects every way of writing a function", {
  expect_equal(count_lines("f <- function(x) x")$function_lines, 1)
  expect_equal(count_lines(c("f = function(x) {", "x", "}"))$function_lines, 3)
  expect_equal(count_lines(c("f <- \\(x) {", "x", "}"))$function_lines, 3)
  expect_equal(count_lines("lapply(1:3, function(i) i)")$function_lines, 1)
  expect_equal(count_lines("x <- 1")$function_lines, 0)
  expect_false(count_lines("x <- 1")$has_function)
  expect_true(count_lines("lapply(1:3, \\(i) i)")$has_function)
})

test_that("count_lines does not spill past a one-line function", {
  lines <- c("f <- function(x) x", "y <- 1", "z <- 2")
  expect_equal(count_lines(lines)$function_lines, 1)
})

test_that("count_lines counts nested functions once", {
  lines <- c("outer <- function() {", "  inner <- function() 1", "  inner()", "}")
  expect_equal(count_lines(lines)$function_lines, 4)
})

test_that("count_lines is not fooled by braces or # inside strings", {
  lines <- c(
    "f <- function() {",
    "  msg <- \"{ unbalanced\"",
    "}",
    "x <- \"",
    "# inside a string",
    "",
    "#' also inside",
    "\""
  )
  counts <- count_lines(lines)
  expect_equal(counts$function_lines, 3)
  expect_equal(counts$comment, 0)
  expect_equal(counts$roxygen, 0)
  expect_equal(counts$blank, 0)
})

test_that("count_lines separates comments, roxygen and blank lines", {
  lines <- c("#' Title", "", "# note", "x <- 1 # trailing", "   ")
  counts <- count_lines(lines)
  expect_equal(counts$total, 5)
  expect_equal(counts$roxygen, 1)
  expect_equal(counts$comment, 1)
  expect_equal(counts$blank, 2)
})

test_that("count_lines handles empty and comment-only input", {
  expect_equal(count_lines(character())$total, 0)
  counts <- count_lines(c("# a", "#' b"))
  expect_equal(counts$comment, 1)
  expect_equal(counts$roxygen, 1)
})

test_that("count_lines warns and falls back when code does not parse", {
  lines <- c("# broken", "f <- function(x) {", "  x +")
  expect_warning(counts <- count_lines(lines, label = "R/broken.R"), "R/broken.R")
  expect_equal(counts$total, 3)
  expect_equal(counts$comment, 1)
  expect_equal(counts$function_lines, 0)
  expect_true(counts$has_function)
})
