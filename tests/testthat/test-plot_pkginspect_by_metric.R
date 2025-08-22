test_that("plot_pkginspect_by_metric scales bars by column total", {
  x <- dplyr::tibble(package = c("pkg1", "pkg2"), files = c(1, 3))
  class(x) <- c("pkginspectr_tbl", class(x))
  out <- testthat::capture_output(
    plot_pkginspect_by_metric(x, glyph = "x", start = "", end = "")
  )
  lines <- strsplit(cli::ansi_strip(out), "\n")[[1]]
  bar_pkg1 <- stringr::str_extract(lines[2], "x+")
  bar_pkg2 <- stringr::str_extract(lines[3], "x+")
  expect_lt(nchar(bar_pkg1), nchar(bar_pkg2))
})

test_that("plot_pkginspect_by_metric handles single-row data and wide glyphs", {
  x <- dplyr::tibble(package = "pkg1", tiny = 0.001, big = 1)
  class(x) <- c("pkginspectr_tbl", class(x))
  out <- testthat::capture_output(
    plot_pkginspect_by_metric(
      x,
      glyph = "\U0001F600", # 😀 wide glyph triggers fallback
      start = "",
      end = "",
      max_bars = 10,
      title = "My Title",
      bg_col = "#000000"
    )
  )
  stripped <- cli::ansi_strip(out)
  expect_match(stripped, "My Title")
  lines <- strsplit(stripped, "\n")[[1]]
  pkg_lines <- lines[stringr::str_detect(lines, "pkg1")]
  bar_tiny <- stringr::str_extract(pkg_lines[1], "\u2501+")
  bar_big <- stringr::str_extract(pkg_lines[2], "\u2501+")
  expect_equal(nchar(bar_tiny), 1)
  expect_gt(nchar(bar_big), nchar(bar_tiny))
  expect_false(any(grepl("\U0001F600", pkg_lines, fixed = TRUE)))
})

test_that("plot_pkginspect_by_metric prints stubs when totals are zero", {
  x <- dplyr::tibble(package = c("pkg1", "pkg2"), files = c(0, 0))
  class(x) <- c("pkginspectr_tbl", class(x))
  out <- testthat::capture_output(
    plot_pkginspect_by_metric(
      x,
      glyph = "x",
      start = "[",
      end = "]",
      max_bars = 5
    )
  )
  lines <- strsplit(cli::ansi_strip(out), "\n")[[1]]
  pkg_lines <- lines[stringr::str_detect(lines, "^pkg")]
  bars <- stringr::str_extract(pkg_lines, stringr::fixed("[]"))
  expect_equal(bars, rep("[]", 2))
})

test_that("plot_pkginspect_by_metric warns when no numeric columns", {
  x <- dplyr::tibble(package = "pkg1")
  class(x) <- c("pkginspectr_tbl", class(x))
  expect_warning(
    res <- plot_pkginspect_by_metric(x),
    "No numeric columns to plot."
  )
  expect_identical(res, x)
})
