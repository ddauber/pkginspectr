test_that("plot_pkginspect_by_package shows glyph for positive values", {
  x <- dplyr::tibble(package = "pkg1", total_lines = 10, blank_lines = 1)
  class(x) <- c("pkginspectr_tbl", class(x))
  out <- testthat::capture_output(
    plot_pkginspect_by_package(
      x,
      max_bars = 10,
      glyph = "x",
      start = "",
      end = ""
    )
  )
  lines <- strsplit(cli::ansi_strip(out), "\n")[[1]]
  bar_line <- lines[stringr::str_detect(lines, "blank_lines")]
  bar <- stringr::str_extract(bar_line, "x+")
  expect_gte(nchar(bar), 1)
})

test_that("plot_pkginspect_by_package automatically uses num_files as denominator", {
  x <- dplyr::tibble(package = "pkg1", num_files = 2, foo = 1)
  class(x) <- c("pkginspectr_tbl", class(x))
  out <- testthat::capture_output(
    plot_pkginspect_by_package(
      x,
      denom_col = "auto",
      start = "",
      end = "",
      glyph = "x"
    )
  )
  expect_match(cli::ansi_strip(out), "Denominator: num_files \\(2\\)")
})

test_that("plot_pkginspect_by_package falls back to sum of numeric columns", {
  x <- dplyr::tibble(
    package = c("pkg1", "pkg2"),
    a = c(1, 2),
    b = c(3, 4)
  )
  class(x) <- c("pkginspectr_tbl", class(x))
  out <- testthat::capture_output(
    plot_pkginspect_by_package(
      x,
      denom_col = "auto",
      start = "",
      end = "",
      glyph = "-"
    )
  )
  stripped <- cli::ansi_strip(out)
  expect_match(stripped, "pkginspectr: Code statistics")
  expect_match(stripped, "Denominator: sum\\(numeric\\) \\(4\\)")
  expect_match(stripped, "Denominator: sum\\(numeric\\) \\(6\\)")
})

test_that("plot_pkginspect_by_package handles NA denominator values", {
  x <- dplyr::tibble(package = "pkg1", total_lines = NA_real_, blank_lines = 1)
  class(x) <- c("pkginspectr_tbl", class(x))
  out <- testthat::capture_output(
    plot_pkginspect_by_package(
      x,
      denom_col = "auto",
      start = "[",
      end = "]",
      glyph = "x"
    )
  )
  lines <- strsplit(cli::ansi_strip(out), "\n")[[1]]
  expect_true(any(grepl("Denominator: total_lines \\(0\\)", lines)))
  blank_line <- lines[stringr::str_detect(lines, "blank_lines")]
  stub <- stringr::str_extract(blank_line, "\\[\\]")
  expect_identical(stub, "[]")
})
