test_that("pkg_structure returns exact counts for the example package", {
  result <- pkg_structure(fixture_path(), quiet = TRUE)

  expect_s3_class(result, "pkginspectr_tbl")
  expect_equal(result$package, "examplepkg")
  expect_equal(result$version, "1.2.3")
  expect_equal(result$num_files, 9)
  expect_equal(result$num_dirs, 6)
  expect_equal(result$num_r_files, 2)
  expect_equal(result$num_function_r_files, 1)
  expect_equal(result$num_non_function_r_files, 1)
  expect_equal(result$num_test_files, 2)
  expect_equal(result$num_vignettes, 1)
  expect_equal(result$num_docs, 1)
  expect_equal(result$num_data_files, 1)
})

test_that("pkg_structure ignores build-excluded files", {
  pkg <- local_fixture_copy()
  fs::dir_create(fs::path(pkg, c(".git", "docs")))
  fs::file_create(fs::path(pkg, c(".git/HEAD", "docs/index.html", ".Rhistory")))
  writeLines("^docs$", fs::path(pkg, ".Rbuildignore"))

  default <- pkg_structure(pkg, quiet = TRUE)
  expect_equal(default$num_files, 9)
  expect_equal(default$num_dirs, 6)
  all <- pkg_structure(pkg, include_ignored = TRUE, quiet = TRUE)
  expect_equal(all$num_files, 10)
  expect_equal(all$num_dirs, 7)
})

test_that("results do not depend on where the package lives on disk", {
  # Parent folders named like package folders used to misclassify every file.
  pkg <- local_fixture_copy(parent = fs::path("tests", "man", "data"))
  expect_true(grepl("/tests/man/data/", pkg, fixed = TRUE))

  expect_equal(
    pkg_structure(pkg, quiet = TRUE)[-1],
    pkg_structure(fixture_path(), quiet = TRUE)[-1]
  )
  expect_equal(
    pkg_lines(pkg, quiet = TRUE)[-1],
    pkg_lines(fixture_path(), quiet = TRUE)[-1]
  )
})

test_that("pkg_structure analyses several packages at once", {
  result <- pkg_structure(c(fixture_path(), local_fixture_copy()), quiet = TRUE)
  expect_equal(nrow(result), 2)
  expect_s3_class(result, "pkginspectr_tbl")
})

test_that("pkg_structure errors on bad path", {
  expect_error(pkg_structure("some/bogus/path", quiet = TRUE))
})

test_that("pkg_structure warns if no DESCRIPTION", {
  path <- withr::local_tempdir()
  fs::dir_create(fs::path(path, "R"))
  expect_warning(result <- pkg_structure(path, quiet = TRUE), "No DESCRIPTION")
  expect_true(is.na(result$version))
})

test_that("pkg_structure errors if path is a file", {
  tmp_file <- withr::local_tempfile()
  writeLines("This is not a directory", tmp_file)
  expect_error(
    pkg_structure(tmp_file, quiet = TRUE),
    "does not exist|Failed to list files|Could not resolve path"
  )
})

test_that("pkg_structure warns on malformed DESCRIPTION file", {
  pkg_path <- withr::local_tempdir()
  fs::dir_create(fs::path(pkg_path, "R"))
  writeLines("Package: malformed\nVersion", fs::path(pkg_path, "DESCRIPTION"))
  expect_warning(
    result <- pkg_structure(pkg_path, quiet = TRUE),
    "Failed to read DESCRIPTION"
  )
  expect_true(is.na(result$version))
})

test_that("pkg_structure errors on unreadable folder", {
  skip_on_os("windows")
  skip_if(Sys.info()[["effective_user"]] == "root", "cannot test as root")

  pkg_path <- withr::local_tempdir()
  Sys.chmod(pkg_path, mode = "000")
  withr::defer(Sys.chmod(pkg_path, mode = "700"))

  expect_error(
    suppressWarnings(pkg_structure(pkg_path, quiet = TRUE)),
    "Failed to list files in"
  )
})

test_that("pkginspectr_example() points at the bundled package", {
  expect_true(fs::file_exists(fs::path(pkginspectr_example(), "DESCRIPTION")))
})
