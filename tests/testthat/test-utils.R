test_that("get_cache_dir respects cache modes and test overrides", {
  root <- local_test_cache()
  expect_equal(get_cache_dir("session"), fs::path(root, "session"))
  expect_equal(get_cache_dir("persistent"), fs::path(root, "persistent"))
  expect_null(get_cache_dir("none"))
})

test_that("pkg_cache_clear removes the cache and returns its path", {
  local_test_cache()
  dir <- get_cache_dir("session")
  fs::dir_create(dir)
  fs::file_create(fs::path(dir, "dummy"))

  expect_message(cleared <- pkg_cache_clear("session"), "Cleared")
  expect_equal(cleared, dir)
  expect_false(fs::dir_exists(dir))
})

test_that("pkg_cache_clear reports when there is no cache", {
  local_test_cache()
  expect_message(cleared <- pkg_cache_clear(), "No cache found")
  expect_null(cleared)
})
