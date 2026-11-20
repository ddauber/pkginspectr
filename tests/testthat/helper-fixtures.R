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

# Point the session and persistent caches at a throwaway folder, so tests
# never read or clear a user's real cache.
local_test_cache <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  withr::local_options(
    pkginspectr.session_cache = fs::path(root, "session"),
    pkginspectr.persistent_cache = fs::path(root, "persistent"),
    .local_envir = env
  )
  root
}

# A .tar.gz shaped like a GitHub or CRAN download: one top-level folder that
# holds the example package (optionally inside `subdir`).
local_tarball <- function(top = "examplepkg-main", subdir = NULL, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  target <- if (is.null(subdir)) fs::path(dir, top) else fs::path(dir, top, subdir)
  fs::dir_create(fs::path_dir(target))
  fs::dir_copy(fixture_path(), target)
  tarball <- fs::path(dir, "pkg.tar.gz")
  withr::with_dir(dir, utils::tar(tarball, top, compression = "gzip", tar = "internal"))
  tarball
}

# Replace the network: downloads copy `tarball` and are recorded in
# `$urls`; `cran_db` lists current CRAN versions; `archive` lists the
# file names in CRAN's archive folder.
local_fake_network <- function(
  tarball,
  cran_db = c(desc = "1.4.3"),
  archive = character(),
  env = parent.frame()
) {
  log <- new.env()
  log$urls <- character()
  db <- matrix(
    cran_db,
    ncol = 1,
    dimnames = list(names(cran_db), "Version")
  )
  testthat::local_mocked_bindings(
    download_file = function(url, dest, label) {
      log$urls <- c(log$urls, url)
      fs::file_copy(tarball, dest)
      invisible(dest)
    },
    cran_available = function(repos) db,
    read_url_lines = function(url) {
      paste0('<a href="', archive, '">', archive, "</a>")
    },
    .env = env
  )
  log
}
