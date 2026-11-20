# The example package bundled in inst/extdata has hand-counted contents, so
# tests can assert exact numbers.
fixture_path <- function() {
  pkginspectr_example()
}

# Writable copy of the example package inside `parent`, removed when `env`
# exits. `parent` lets tests place it under folders such as `tests/`.
local_fixture_copy <- function(parent = NULL, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  if (!is.null(parent)) {
    dir <- fs::path(dir, parent)
    fs::dir_create(dir)
  }
  fs::dir_copy(fixture_path(), fs::path(dir, "examplepkg"))
  fs::path(dir, "examplepkg")
}
