test_that("old function names still work but are deprecated", {
  withr::local_options(lifecycle_verbosity = "warning")
  path <- fixture_path()

  expect_warning(
    res <- suppressMessages(inspect_package(path)),
    class = "lifecycle_warning_deprecated"
  )
  expect_equal(res, pkg_structure(path, quiet = TRUE))

  expect_warning(
    res <- suppressMessages(get_code_stats(path)),
    class = "lifecycle_warning_deprecated"
  )
  expect_s3_class(res, "pkginspectr_tbl")
  expect_named(res, c(
    "package", "version", "total_lines", "blank_lines", "comment_lines",
    "roxygen_lines", "function_lines", "test_lines"
  ))

  expect_warning(
    suppressMessages(clear_pkg_cache("session")),
    class = "lifecycle_warning_deprecated"
  )
})

test_that("plot_pkg is deprecated and ignores arguments pkg_plot dropped", {
  withr::local_options(lifecycle_verbosity = "warning")
  x <- pkg_lines(fixture_path(), quiet = TRUE)

  expect_warning(
    capture_output(res <- plot_pkg(x, type = "metric")),
    class = "lifecycle_warning_deprecated"
  )
  expect_identical(res, x)

  withr::local_options(lifecycle_verbosity = "quiet")
  expect_warning(
    capture_output(plot_pkg(x, name_spacing = 2, max_bars = 10)),
    "name_spacing"
  )
})
