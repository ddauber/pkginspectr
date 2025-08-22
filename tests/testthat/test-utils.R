# Tests for internal utility helpers

test_that("is_github_spec identifies GitHub shorthand", {
  expect_true(pkginspectr:::is_github_spec("user/repo"))
  expect_false(pkginspectr:::is_github_spec("https://github.com/user/repo"))
})

test_that("is_github_url detects full GitHub URLs", {
  expect_true(pkginspectr:::is_github_url("https://github.com/user/repo"))
  expect_false(pkginspectr:::is_github_url("user/repo"))
})

test_that("get_cache_dir respects cache modes", {
  session_path <- pkginspectr:::get_cache_dir("session")
  expect_true(grepl("pkginspectr", session_path))
  none_path <- pkginspectr:::get_cache_dir("none")
  expect_null(none_path)
})

test_that("clear_pkg_cache removes session cache", {
  dir <- pkginspectr:::get_cache_dir("session")
  fs::dir_create(dir)
  file.create(fs::path(dir, "dummy"))
  expect_true(fs::file_exists(fs::path(dir, "dummy")))
  clear_pkg_cache("session")
  expect_false(fs::dir_exists(dir))
})

test_that("clear_pkg_cache is silent when no cache exists", {
  dir <- pkginspectr:::get_cache_dir("session")
  if (fs::dir_exists(dir)) fs::dir_delete(dir)
  expect_no_error(clear_pkg_cache("session"))
})
