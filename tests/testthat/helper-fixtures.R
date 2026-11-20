# Path to the in-repo fixture package. It lives under `tests/`, so every test
# using it in place also guards against classifying files by absolute path.
fixture_path <- function() {
  testthat::test_path("fixtures", "fixturepkg")
}

# Writable copy of the fixture, removed when `env` exits.
local_fixture_copy <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  fs::dir_copy(fixture_path(), fs::path(dir, "fixturepkg"))
  fs::path(dir, "fixturepkg")
}
