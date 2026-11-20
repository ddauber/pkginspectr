test_that("inspect_package returns exact counts for the fixture package", {
  result <- suppressMessages(inspect_package(fixture_path()))

  expect_s3_class(result, "pkginspectr_tbl")
  expect_equal(result$package, "fixturepkg")
  expect_equal(result$version, "1.2.3")
  expect_equal(result$num_files, 9)
  expect_equal(result$num_dirs, 6)
  expect_equal(result$num_r_files, 4)
  expect_equal(result$num_function_r_files, 1)
  expect_equal(result$num_non_function_r_files, 3)
  expect_equal(result$num_test_files, 2)
  expect_equal(result$num_vignettes, 1)
  expect_equal(result$num_docs, 1)
  expect_equal(result$num_data_files, 1)
})

test_that("inspect_package ignores build-excluded files", {
  pkg <- local_fixture_copy()
  fs::dir_create(fs::path(pkg, c(".git", "docs")))
  fs::file_create(fs::path(pkg, c(".git/HEAD", "docs/index.html", ".Rhistory")))
  writeLines("^docs$", fs::path(pkg, ".Rbuildignore"))

  expect_equal(suppressMessages(inspect_package(pkg))$num_files, 9)
  expect_equal(suppressMessages(inspect_package(pkg))$num_dirs, 6)
  all <- suppressMessages(inspect_package(pkg, include_ignored = TRUE))
  expect_equal(all$num_files, 10)
  expect_equal(all$num_dirs, 7)
})

test_that("inspect_package errors on bad path", {
  expect_error(inspect_package("some/bogus/path"))
})

test_that("inspect_package warns if no DESCRIPTION", {
  path <- withr::local_tempdir()
  fs::dir_create(fs::path(path, "R"))
  expect_warning(result <- suppressMessages(inspect_package(path)))
  expect_true(is.na(result$version))
})

test_that("inspect_package errors if path is a file", {
  tmp_file <- withr::local_tempfile()
  writeLines("This is not a directory", tmp_file)
  expect_error(
    inspect_package(tmp_file),
    "does not exist|Failed to list files|Could not resolve path"
  )
})

test_that("inspect_package warns on malformed DESCRIPTION file", {
  pkg_path <- withr::local_tempdir()
  fs::dir_create(fs::path(pkg_path, "R"))
  writeLines("Package: malformed\nVersion", fs::path(pkg_path, "DESCRIPTION"))
  expect_warning(
    result <- suppressMessages(inspect_package(pkg_path)),
    "Failed to read DESCRIPTION"
  )
  expect_true(is.na(result$version))
})

test_that("inspect_package triggers error on unreadable folder", {
  skip_on_os("windows")
  skip_if(Sys.info()[["effective_user"]] == "root", "cannot test as root")

  pkg_path <- withr::local_tempdir()
  Sys.chmod(pkg_path, mode = "000")
  withr::defer(Sys.chmod(pkg_path, mode = "700"))

  expect_error(inspect_package(pkg_path), "Failed to list files in")
})
