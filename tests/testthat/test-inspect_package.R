test_that("inspect_package returns expected structure", {
  withr::local_tempdir() |>
    (\(tmp) {
      pkg_path <- file.path(tmp, "pkg1")
      usethis::create_package(
        pkg_path,
        open = FALSE,
        fields = list(Title = "Test Package")
      )

      result <- inspect_package(pkg_path)

      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 1)
      expect_true("package" %in% names(result))
      expect_true("version" %in% names(result))
      expect_true("num_r_files" %in% names(result))
    })()
})

test_that("inspect_package errors on bad path", {
  expect_error(inspect_package("some/bogus/path"))
})

test_that("inspect_package warns if no DESCRIPTION", {
  path <- withr::local_tempdir()
  fs::dir_create(fs::path(path, "R"))
  expect_warning(result <- inspect_package(path))
  expect_true(is.na(result$version))
})

test_that("inspect_package errors if fs::dir_ls() fails", {
  tmp_file <- withr::local_tempfile()
  writeLines("This is not a directory", tmp_file)

  # Confirm it's a file
  expect_true(fs::file_exists(tmp_file))
  expect_false(fs::dir_exists(tmp_file))

  expect_error(
    inspect_package(tmp_file),
    "does not exist|Failed to list files|Could not resolve path"
  )
})

test_that("inspect_package warns on malformed DESCRIPTION file", {
  pkg_path <- withr::local_tempdir()
  fs::dir_create(fs::path(pkg_path, "R"))
  writeLines("Package: malformed\nVersion", fs::path(pkg_path, "DESCRIPTION")) # incomplete field

  expect_warning(
    result <- inspect_package(pkg_path),
    "Failed to read DESCRIPTION"
  )
  expect_true(is.na(result$version))
})

test_that("inspect_package triggers error on unreadable folder", {
  skip_if(
    Sys.info()[["sysname"]] == "Windows",
    "chmod test unsupported on Windows"
  )
  skip_if(Sys.info()[["effective_user"]] == "root", "cannot test as root")

  pkg_path <- withr::local_tempdir()
  fs::dir_create(pkg_path)
  Sys.chmod(pkg_path, mode = "000")

  expect_error(
    inspect_package(pkg_path),
    "Failed to list files in"
  )

  Sys.chmod(pkg_path, mode = "700")
})

test_that("get_code_stats warns and returns 0s when no .R files found", {
  pkg_path <- withr::local_tempdir()
  fs::dir_create(fs::path(pkg_path, "R")) # Create R folder but leave it empty
  fs::dir_create(fs::path(pkg_path, "tests"))
  fs::dir_create(fs::path(pkg_path, "data-raw"))
  expect_warning(
    result <- get_code_stats(pkg_path),
    "No .R files found"
  )

  expect_s3_class(result, "tbl_df")
  expect_true("version" %in% names(result))
  expect_true(is.na(result$version))
  expect_equal(result$total_lines, 0)
  expect_equal(result$function_lines, 0)
  expect_equal(result$test_lines, 0)
})
