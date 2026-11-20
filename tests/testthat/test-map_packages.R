test_that("one failing package in a batch becomes an NA row, with a warning", {
  good <- fixture_path()
  expect_warning(
    result <- pkg_summary(c(good, "./missing", good), quiet = TRUE),
    "1 package could not be analysed"
  )

  expect_s3_class(result, "pkginspectr_tbl")
  expect_equal(nrow(result), 3)
  expect_equal(result$package, c("examplepkg", "./missing", "examplepkg"))
  expect_true(is.na(result$version[2]))
  expect_true(all(is.na(unlist(result[2, -1]))))
  # the other rows are unaffected, and column types are kept
  expect_equal(result$total_lines[c(1, 3)], c(32, 32))
  expect_type(result$total_lines, "integer")
})

test_that("the warning lists every failure with its reason", {
  warnings <- capture_warnings(
    pkg_lines(c(fixture_path(), "./a", "./b"), quiet = TRUE)
  )
  expect_length(warnings, 1)
  expect_match(warnings, "2 packages could not be analysed")
  expect_match(warnings, "./a.*no such local folder")
  expect_match(warnings, "./b.*no such local folder")
})

test_that("failed downloads in a batch become NA rows", {
  local_test_cache()
  local_mocked_bindings(download_file = function(url, dest, label) {
    cli::cli_abort("Could not download {label}.")
  })
  expect_warning(
    result <- pkg_structure(c(fixture_path(), "owner/repo"), quiet = TRUE),
    "owner/repo.*Could not download"
  )
  expect_equal(result$package, c("examplepkg", "owner/repo"))
  expect_true(is.na(result$num_files[2]))
})

test_that("a single failing package is still an error", {
  expect_error(pkg_lines("./missing", quiet = TRUE), "no such local folder")
})

test_that("all packages failing is an error listing each reason", {
  expect_error(
    pkg_structure(c("./a", "./b"), quiet = TRUE),
    "None of the packages could be analysed"
  )
})

test_that("pkg_functions leaves failed packages out", {
  expect_warning(
    fns <- pkg_functions(c(fixture_path(), "./missing"), quiet = TRUE),
    "left out"
  )
  expect_equal(nrow(fns), 3)
  expect_s3_class(fns, "pkginspectr_functions")
})

test_that("quiet = TRUE silences progress but not failure warnings", {
  expect_no_message(
    suppressWarnings(pkg_lines(c(fixture_path(), "./missing"), quiet = TRUE))
  )
  expect_warning(pkg_lines(c(fixture_path(), "./missing"), quiet = TRUE))
})

test_that("pkg_history keeps failed releases as labelled NA rows", {
  local_test_cache()
  tarball <- local_tarball("desc")
  local_fake_network(
    tarball,
    cran_db = c(desc = "1.4.3"),
    archive = c("desc_1.2.0.tar.gz" = "2018-05-01")
  )
  # make the archived release fail to download
  local_mocked_bindings(download_file = function(url, dest, label) {
    if (grepl("Archive", url)) cli::cli_abort("Could not download {label}.")
    fs::file_copy(tarball, dest)
  })

  expect_warning(
    history <- pkg_history("desc", quiet = TRUE),
    "cran::desc@1.2.0"
  )
  expect_equal(history$package, c("desc", "desc"))
  expect_equal(history$version, c("1.2.0", "1.4.3"))
  expect_equal(history$date, as.Date(c("2018-05-01", "2023-12-10")))
  expect_true(is.na(history$total_lines[1]))
  expect_false(is.na(history$total_lines[2]))
})

test_that("plots show NA rows without bars", {
  result <- suppressWarnings(pkg_lines(c(fixture_path(), "./missing"), quiet = TRUE))
  out <- cli::ansi_strip(capture_output(
    pkg_plot(result, type = "metric", text_col = NULL, bar_col = NULL, start = "[", end = "]")
  ))
  expect_match(out, "./missing\\s+\\[\\]\\s+NA")
})
