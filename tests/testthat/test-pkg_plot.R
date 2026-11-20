# Plot output without colours, split into lines.
plot_lines <- function(...) {
  out <- capture_output(pkg_plot(..., text_col = NULL, bar_col = NULL))
  strsplit(cli::ansi_strip(out), "\n")[[1]]
}

line_for <- function(lines, label) {
  lines[startsWith(lines, paste0(label, " "))]
}

test_that("pkg_plot and plot() return their input invisibly", {
  x <- pkg_lines(fixture_path(), quiet = TRUE)
  capture_output(res <- withVisible(pkg_plot(x)))
  expect_identical(res$value, x)
  expect_false(res$visible)
  capture_output(res <- withVisible(plot(x, type = "metric")))
  expect_identical(res$value, x)
  expect_false(res$visible)
})

test_that("package view shows shares of total_lines and num_files", {
  lines <- plot_lines(pkg_summary(fixture_path(), quiet = TRUE))
  expect_true(any(grepl("Lines (share of total_lines)", lines, fixed = TRUE)))
  expect_true(any(grepl("Files (share of num_files)", lines, fixed = TRUE)))
  expect_match(line_for(lines, "code_lines"), "21\\s+65\\.6%")
  expect_match(line_for(lines, "num_r_files"), "2\\s+22\\.2%")
})

test_that("num_dirs is shown without a share or bar", {
  lines <- plot_lines(pkg_structure(fixture_path(), quiet = TRUE), start = "[", end = "]")
  dirs <- line_for(lines, "num_dirs")
  expect_match(dirs, "^num_dirs\\s+6\\s*$")
})

test_that("metric view scales bars to each metric's largest value", {
  x <- dplyr::tibble(package = c("a", "b"), files = c(1, 3))
  lines <- plot_lines(x, type = "metric", glyph = "x", start = "", end = "", max_bars = 9)
  expect_equal(nchar(stringr::str_extract(line_for(lines, "a"), "x+")), 3)
  expect_equal(nchar(stringr::str_extract(line_for(lines, "b"), "x+")), 9)
})

test_that("tiny positive values get one glyph and zeros show only the caps", {
  x <- dplyr::tibble(package = c("a", "b", "c"), n = c(0, 0.001, 100))
  lines <- plot_lines(x, type = "metric", glyph = "x", start = "[", end = "]", max_bars = 10)
  expect_match(line_for(lines, "a"), "\\[\\]")
  expect_match(line_for(lines, "b"), "\\[x\\]")
  expect_match(line_for(lines, "c"), "\\[x{10}\\]")
})

test_that("wide glyphs are replaced by a single-width one", {
  x <- dplyr::tibble(package = "a", n = 1)
  lines <- plot_lines(x, type = "metric", glyph = "\U0001F600", max_bars = 5)
  expect_false(any(grepl("\U0001F600", lines, fixed = TRUE)))
  expect_true(any(grepl("━", lines, fixed = TRUE)))
})

test_that("default titles describe the data; custom and empty titles work", {
  expect_match(plot_lines(pkg_lines(fixture_path(), quiet = TRUE))[1], "Lines of code")
  expect_match(plot_lines(pkg_structure(fixture_path(), quiet = TRUE))[1], "Package structure")
  expect_match(plot_lines(pkg_summary(fixture_path(), quiet = TRUE))[1], "Package summary")

  x <- dplyr::tibble(package = "a", n = 1)
  expect_match(plot_lines(x)[1], "Package statistics")
  expect_match(plot_lines(x, title = "My Title")[1], "My Title")
  expect_false(any(grepl("pkginspectr:", plot_lines(x, title = ""))))
})

test_that("columns without a known denominator are scaled within the row", {
  x <- dplyr::tibble(package = "a", small = 1, big = 4)
  lines <- plot_lines(x, glyph = "x", start = "", end = "", max_bars = 8)
  expect_equal(nchar(stringr::str_extract(line_for(lines, "small"), "x+")), 2)
  expect_equal(nchar(stringr::str_extract(line_for(lines, "big"), "x+")), 8)
})

test_that("duplicate package names are labelled with their version", {
  x <- dplyr::tibble(package = c("a", "a"), version = c("1.0", "2.0"), n = 1:2)
  lines <- plot_lines(x, type = "metric")
  expect_length(line_for(lines, "a 1.0"), 1)
  expect_length(line_for(lines, "a 2.0"), 1)
})

test_that("bg_col pads every line to the console width", {
  x <- dplyr::tibble(package = "a", n = 1)
  out <- capture_output(pkg_plot(x, bg_col = "black"))
  widths <- cli::ansi_nchar(strsplit(out, "\n")[[1]], type = "width")
  expect_true(all(widths == cli::console_width()))
})

test_that("pkg_plot rejects unsuitable input", {
  expect_error(pkg_plot(1:3), "must be a data frame")
  expect_error(pkg_plot(data.frame(n = 1)), "package")
  expect_error(pkg_plot(data.frame(package = "a")), "no numeric columns")
})

test_that("values are shown with thousands separators and at most 3 decimals", {
  x <- dplyr::tibble(package = c("a", "b"), ratio = c(0.5026087, 0.28), n = c(1234567, 2))
  lines <- plot_lines(x, type = "metric")
  expect_match(lines, "0.503$", all = FALSE)
  expect_match(lines, "0.28$", all = FALSE)
  expect_match(lines, "1,234,567$", all = FALSE)
})
