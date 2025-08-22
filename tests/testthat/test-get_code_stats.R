test_that("get_code_stats returns line counts", {
  withr::local_tempdir() |>
    (\(tmp) {
      pkg_path <- file.path(tmp, "pkg1")
      usethis::create_package(pkg_path, open = FALSE)

      fs::dir_create(file.path(pkg_path, "R"))
      writeLines(
        c(
          "#' Title",
          "#' Description",
          "my_func <- function(x) {",
          "  # do something",
          "  x + 1",
          "}"
        ),
        file.path(pkg_path, "R", "my_func.R")
      )

      result <- get_code_stats(pkg_path)

      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 1)
      expect_true("version" %in% names(result))
      expect_true("function_lines" %in% names(result))
      expect_false(is.na(result$version))
    })()
})

test_that("get_code_stats errors on bad path", {
  expect_error(get_code_stats("this/path/does/not/exist"))
})

test_that("estimate_function_lines returns 0 when no functions are found", {
  lines <- c(
    "# This is a comment",
    "x <- 5",
    "y <- x + 1"
  )

  expect_equal(pkginspectr:::estimate_function_lines(lines), 0)
})

test_that("estimate_function_lines counts simple function bodies", {
  lines <- c(
    "f <- function(x) {",
    "  x + 1",
    "}"
  )

  expect_true(pkginspectr:::estimate_function_lines(lines) > 0)
})

test_that("get_code_stats distinguishes roxygen and comments", {
  withr::local_tempdir() |>
    (\(tmp) {
      pkg_path <- file.path(tmp, "pkg1")
      usethis::create_package(pkg_path, open = FALSE)
      fs::dir_create(file.path(pkg_path, "R"))
      writeLines(
        c(
          "#' A title",
          "# regular comment",
          "f <- function(x) x"
        ),
        file.path(pkg_path, "R", "f.R")
      )
      stats <- get_code_stats(pkg_path)
      expect_equal(stats$roxygen_lines, 1)
      expect_equal(stats$comment_lines, 1)
    })()
})

test_that("get_code_stats() errors on unresolvable path", {
  withr::local_options(cli.num_colors = 1) # keep messages plain

  fake <- tempfile("does_not_exist_")
  expect_false(fs::dir_exists(fake))

  # Error originates from resolve_package_path() inside purrr::map_chr()
  expect_error(
    get_code_stats(fake),
    regexp = "Could not resolve path .* must be a local folder",
    class = "purrr_error_indexed" # purrr wraps the inner abort
  )
})
