test_that("pkg_lines returns exact counts for the example package", {
  result <- pkg_lines(fixture_path(), quiet = TRUE)

  expect_s3_class(result, "pkginspectr_tbl")
  expect_equal(nrow(result), 1)
  expect_equal(result$package, "examplepkg")
  expect_equal(result$version, "1.2.3")
  expect_equal(result$total_lines, 32)
  expect_equal(result$code_lines, 21)
  expect_equal(result$comment_lines, 2)
  expect_equal(result$roxygen_lines, 4)
  expect_equal(result$blank_lines, 5)
  expect_equal(result$r_lines, 25)
  expect_equal(result$test_lines, 7)
  expect_equal(result$other_lines, 0)
  expect_equal(result$function_lines, 7)
})

test_that("line types and locations each add up to total_lines", {
  pkg <- local_fixture_copy()
  fs::dir_create(fs::path(pkg, "data-raw"))
  writeLines(c("# build data", "x <- 1", ""), fs::path(pkg, "data-raw", "make.R"))

  result <- pkg_lines(pkg, quiet = TRUE)
  with(result, {
    expect_equal(code_lines + comment_lines + roxygen_lines + blank_lines, total_lines)
    expect_equal(r_lines + test_lines + other_lines, total_lines)
    expect_lte(function_lines, code_lines)
  })
  expect_equal(result$other_lines, 3)
})

test_that("pkg_lines respects .Rbuildignore unless include_ignored = TRUE", {
  pkg <- local_fixture_copy()
  fs::dir_create(fs::path(pkg, "data-raw"))
  writeLines(c("x <- 1", "y <- 2"), fs::path(pkg, "data-raw", "make.R"))
  writeLines("^data-raw$", fs::path(pkg, ".Rbuildignore"))

  expect_equal(pkg_lines(pkg, quiet = TRUE)$total_lines, 32)
  expect_equal(pkg_lines(pkg, include_ignored = TRUE, quiet = TRUE)$total_lines, 34)
})

test_that("pkg_lines reports progress unless quiet = TRUE", {
  expect_message(pkg_lines(fixture_path()), "Counting lines in")
  expect_no_message(pkg_lines(fixture_path(), quiet = TRUE))
})

test_that("pkg_lines errors on unresolvable path", {
  withr::local_options(cli.num_colors = 1)
  fake <- tempfile("does_not_exist_")
  expect_error(
    pkg_lines(fake),
    regexp = "Could not resolve path .* no such local folder",
    class = "rlang_error"
  )
})

test_that("pkg_lines warns and returns 0s when no .R files found", {
  pkg_path <- withr::local_tempdir()
  fs::dir_create(fs::path(pkg_path, c("R", "tests", "data-raw")))
  writeLines(c("Package: empty", "Version: 0.0.1"), fs::path(pkg_path, "DESCRIPTION"))
  expect_warning(result <- pkg_lines(pkg_path, quiet = TRUE), "No .R files found")
  expect_equal(result$version, "0.0.1")
  expect_true(all(unlist(result[-(1:2)]) == 0))
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

test_that("function_lines only counts code, not comments or blanks inside", {
  lines <- c("f <- function() {", "  # note", "", "  1", "}")
  counts <- count_lines(lines)
  expect_equal(counts$function_lines, 3)
  expect_equal(counts$code, 3)
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
  expect_equal(counts$code, 8)
  expect_equal(counts$comment, 0)
  expect_equal(counts$roxygen, 0)
  expect_equal(counts$blank, 0)
})

test_that("count_lines separates code, comments, roxygen and blank lines", {
  lines <- c("#' Title", "", "# note", "x <- 1 # trailing", "   ")
  counts <- count_lines(lines)
  expect_equal(counts$total, 5)
  expect_equal(counts$code, 1)
  expect_equal(counts$roxygen, 1)
  expect_equal(counts$comment, 1)
  expect_equal(counts$blank, 2)
})

test_that("count_lines handles empty and comment-only input", {
  expect_equal(count_lines(character())$total, 0)
  counts <- count_lines(c("# a", "#' b"))
  expect_equal(counts$comment, 1)
  expect_equal(counts$roxygen, 1)
  expect_equal(counts$code, 0)
})

test_that("count_lines warns and falls back when code does not parse", {
  lines <- c("# broken", "f <- function(x) {", "  x +")
  expect_warning(counts <- count_lines(lines, label = "R/broken.R"), "R/broken.R")
  expect_equal(counts$total, 3)
  expect_equal(counts$comment, 1)
  expect_equal(counts$code, 2)
  expect_equal(counts$function_lines, 0)
  expect_true(counts$has_function)
})
