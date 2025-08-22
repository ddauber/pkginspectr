test_that("resolve_package_path caches GitHub packages with correct root", {
  skip_if_offline(host = "github.com")
  clear_pkg_cache("session")
  path1 <- resolve_package_path("ddauber/tidycomp", cache = "session")
  expect_true(fs::file_exists(fs::path(path1, "DESCRIPTION")))
  path2 <- resolve_package_path("ddauber/tidycomp", cache = "session")
  expect_true(fs::file_exists(fs::path(path2, "DESCRIPTION")))
  expect_identical(path1, path2)
})

test_that("resolve_package_path returns absolute paths for local directories", {
  withr::local_tempdir() |>
    (\(tmp) {
      pkg <- fs::path(tmp, "pkg")
      fs::dir_create(pkg)
      resolved <- resolve_package_path(pkg)
      expect_equal(resolved, fs::path_abs(pkg))
    })()
})

test_that("resolve_package_path caches CRAN packages with correct root", {
  skip_if_offline(host = "cran.r-project.org")
  clear_pkg_cache("session")
  path1 <- tryCatch(
    resolve_package_path("desc", cache = "session"),
    error = function(e) skip("unable to access CRAN")
  )
  expect_true(fs::file_exists(fs::path(path1, "DESCRIPTION")))
  path2 <- resolve_package_path("desc", cache = "session")
  expect_true(fs::file_exists(fs::path(path2, "DESCRIPTION")))
  expect_identical(path1, path2)
})

test_that("resolve_package_path errors on unknown specification", {
  skip_if_offline(host = "cran.r-project.org")
  expect_error(resolve_package_path("pkginspectrNonexistentPkg"))
})
