test_that("pkg_functions lists the example package's functions exactly", {
  fns <- pkg_functions(fixture_path(), quiet = TRUE)

  expect_s3_class(fns, "pkginspectr_functions")
  expect_named(fns, c(
    "package", "version", "file", "location", "name", "line",
    "code_lines", "n_args", "exported"
  ))
  expect_equal(fns$name, c("add_one", "square", "compose"))
  expect_equal(fns$file, rep("R/funs.R", 3))
  expect_equal(fns$line, c(5, 10, 12))
  expect_equal(fns$code_lines, c(3, 1, 3))
  expect_equal(fns$n_args, c(1, 1, 2))
  expect_equal(fns$exported, c(TRUE, FALSE, FALSE))
  expect_equal(unique(fns$package), "examplepkg")
})

test_that("function code lines agree with pkg_lines", {
  fns <- pkg_functions(fixture_path(), quiet = TRUE)
  lines <- pkg_lines(fixture_path(), quiet = TRUE)
  # every function in the example is top-level and non-nested
  expect_equal(sum(fns$code_lines), lines$function_lines)
})

test_that("find_functions recognises every assignment style", {
  defs <- find_functions(c(
    "a <- function(x, y, ...) NULL",
    "b = \\() 1",
    "c <<- function(x) {",
    "  inner <- function() 2",
    "  x",
    "}",
    "\"d\" <- function(x) x",
    "lapply(1:3, function(i) i)",
    "obj$e <- function() 3",
    "f <- 1"
  ))
  expect_equal(defs$name, c("a", "b", "c", "d"))
  expect_equal(defs$n_args, c(3, 0, 1, 1))
  expect_equal(defs$line, c(1, 2, 3, 7))
  expect_equal(defs$code_lines, c(1, 1, 4, 1))
})

test_that("find_functions skips files that do not parse, with a warning", {
  expect_warning(
    defs <- find_functions(c("f <- function(x) {", "  x +"), label = "R/broken.R"),
    "R/broken.R"
  )
  expect_equal(nrow(defs), 0)
  expect_named(defs, c("name", "line", "code_lines", "n_args"))
})

test_that("exportPattern() and missing NAMESPACE are handled", {
  pkg <- local_fixture_copy()
  writeLines('exportPattern("^[a-z]")', fs::path(pkg, "NAMESPACE"))
  expect_true(all(pkg_functions(pkg, quiet = TRUE)$exported))

  fs::file_delete(fs::path(pkg, "NAMESPACE"))
  expect_true(all(is.na(pkg_functions(pkg, quiet = TRUE)$exported)))
})

test_that("functions outside R/ have a location and no export status", {
  pkg <- local_fixture_copy()
  writeLines("helper <- function() 1", fs::path(pkg, "tests", "testthat", "helper.R"))
  fns <- pkg_functions(pkg, quiet = TRUE)
  helper <- fns[fns$name == "helper", ]
  expect_equal(helper$location, "tests")
  expect_equal(helper$file, "tests/testthat/helper.R")
  expect_true(is.na(helper$exported))
})

test_that("packages without functions give zero rows with all columns", {
  pkg <- withr::local_tempdir()
  fs::dir_create(fs::path(pkg, "R"))
  writeLines(c("Package: nofuns", "Version: 0.1"), fs::path(pkg, "DESCRIPTION"))
  writeLines("x <- 1", fs::path(pkg, "R", "data.R"))

  fns <- pkg_functions(pkg, quiet = TRUE)
  expect_equal(nrow(fns), 0)
  expect_named(fns, names(pkg_functions(fixture_path(), quiet = TRUE)))
  expect_type(fns$exported, "logical")
})

test_that("pkg_functions combines several packages", {
  fns <- pkg_functions(c(fixture_path(), local_fixture_copy()), quiet = TRUE)
  expect_equal(nrow(fns), 6)
})

test_that("pkg_plot refuses function tables", {
  fns <- pkg_functions(fixture_path(), quiet = TRUE)
  expect_error(pkg_plot(fns), "one row per function")
})
