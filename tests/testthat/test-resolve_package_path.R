# Parsing ----------------------------------------------------------------------

test_that("existing local folders resolve to absolute paths", {
  pkg <- local_fixture_copy()
  expect_equal(resolve_package_path(pkg), fs::path_abs(pkg))
  expect_equal(resolve_package_path(paste0("local::", pkg)), fs::path_abs(pkg))
})

test_that("GitHub specifications are parsed", {
  spec <- parse_pkg_spec("r-lib/cli")
  expect_equal(spec[c("source", "owner", "repo")], list(source = "github", owner = "r-lib", repo = "cli"))
  expect_null(spec$ref)
  expect_null(spec$subdir)

  spec <- parse_pkg_spec("github::apache/arrow/r@apache-arrow-15.0.0")
  expect_equal(spec$subdir, "r")
  expect_equal(spec$ref, "apache-arrow-15.0.0")

  expect_equal(parse_pkg_spec("r-lib/cli@main")$ref, "main")
  expect_equal(parse_pkg_spec("https://github.com/r-lib/cli.git")$repo, "cli")

  spec <- parse_pkg_spec("https://github.com/apache/arrow/tree/main/r")
  expect_equal(spec$ref, "main")
  expect_equal(spec$subdir, "r")
})

test_that("CRAN specifications are parsed", {
  expect_equal(parse_pkg_spec("desc")[c("source", "name")], list(source = "cran", name = "desc"))
  expect_null(parse_pkg_spec("desc")$version)
  expect_equal(parse_pkg_spec("cran::desc@1.4.0")$version, "1.4.0")
  expect_equal(parse_pkg_spec("data.table@1.14.8")$version, "1.14.8")
})

test_that("non-existent paths are errors, not GitHub requests", {
  withr::local_options(cli.num_colors = 1)
  expect_error(parse_pkg_spec("./mypkg"), "no such local folder")
  expect_error(parse_pkg_spec("/does/not/exist"), "no such local folder")
  expect_error(parse_pkg_spec("~/does-not-exist-pkginspectr"), "no such local folder")
  expect_error(parse_pkg_spec("pkgs/sub/mypkg"), "no such local folder")
  expect_error(parse_pkg_spec("local::nope"), "no local folder")
})

test_that("invalid specifications give helpful errors", {
  withr::local_options(cli.num_colors = 1)
  expect_error(parse_pkg_spec("svn::foo"), "Unknown source prefix")
  expect_error(parse_pkg_spec("github::justone"), "Invalid GitHub repository")
  expect_error(parse_pkg_spec("cran::desc@latest"), "Invalid version")
  expect_error(parse_pkg_spec("not a package!"), "Could not resolve")
  expect_error(resolve_package_path(character()), "character vector")
  expect_error(resolve_package_path(NA_character_), "character vector")
})

test_that("only commit SHAs and version tags count as pinned refs", {
  expect_true(is_pinned_ref("v1.2.0"))
  expect_true(is_pinned_ref("1.2.0"))
  expect_true(is_pinned_ref("257fd05"))
  expect_false(is_pinned_ref("main"))
  expect_false(is_pinned_ref("feature/v2"))
  expect_false(is_pinned_ref(NULL))
})

# GitHub -----------------------------------------------------------------------

test_that("GitHub repos download the default branch once per session", {
  local_test_cache()
  net <- local_fake_network(local_tarball("tidycomp-257fd05"))

  path1 <- suppressMessages(resolve_package_path("ddauber/tidycomp"))
  expect_message(path2 <- resolve_package_path("ddauber/tidycomp"), "Using cached")

  expect_equal(net$urls, "https://github.com/ddauber/tidycomp/archive/HEAD.tar.gz")
  expect_identical(path1, path2)
  expect_true(fs::file_exists(fs::path(path1, "DESCRIPTION")))
})

test_that("GitHub refs and subfolders are honoured", {
  local_test_cache()
  net <- local_fake_network(local_tarball("arrow-15", subdir = "r"))

  path <- suppressMessages(resolve_package_path("github::apache/arrow/r@v15.0.0"))
  expect_equal(net$urls, "https://github.com/apache/arrow/archive/v15.0.0.tar.gz")
  expect_equal(fs::path_file(path), "r")
  expect_true(fs::file_exists(fs::path(path, "DESCRIPTION")))

  expect_error(
    suppressMessages(resolve_package_path("github::apache/arrow/python@v15.0.0")),
    "not found"
  )
})

test_that("same-named repos from different owners do not collide", {
  local_test_cache()
  net <- local_fake_network(local_tarball())
  a <- suppressMessages(resolve_package_path("alice/utils"))
  b <- suppressMessages(resolve_package_path("bob/utils"))
  expect_false(a == b)
  expect_length(net$urls, 2)
})

test_that("the persistent cache only stores pinned versions", {
  root <- local_test_cache()
  local_fake_network(local_tarball())

  moving <- suppressMessages(resolve_package_path("owner/repo", cache = "persistent"))
  branch <- suppressMessages(resolve_package_path("owner/repo@dev", cache = "persistent"))
  pinned <- suppressMessages(resolve_package_path("owner/repo@v1.0.0", cache = "persistent"))

  expect_true(startsWith(moving, fs::path(root, "session")))
  expect_true(startsWith(branch, fs::path(root, "session")))
  expect_true(startsWith(pinned, fs::path(root, "persistent")))
})

test_that("cache = 'none' downloads every time", {
  local_test_cache()
  net <- local_fake_network(local_tarball())
  suppressMessages(resolve_package_path("owner/repo", cache = "none"))
  suppressMessages(resolve_package_path("owner/repo", cache = "none"))
  expect_length(net$urls, 2)
})

test_that("a failed download leaves no broken cache entry", {
  local_test_cache()
  local_mocked_bindings(download_file = function(url, dest, label) {
    writeLines("half a file", dest)
    cli::cli_abort("Could not download {label}.")
  })
  expect_error(suppressMessages(resolve_package_path("owner/repo")), "Could not download")
  expect_false(fs::dir_exists(fs::path(get_cache_dir("session"), "github", "owner", "repo", "HEAD")))
  # nothing left behind in the staging area either
  expect_length(fs::dir_ls(fs::path(get_cache_dir("session"), "github", "owner", "repo")), 0)

  net <- local_fake_network(local_tarball())
  path <- suppressMessages(resolve_package_path("owner/repo"))
  expect_true(fs::file_exists(fs::path(path, "DESCRIPTION")))
})

test_that("archives with an unexpected layout are rejected", {
  local_test_cache()
  dir <- withr::local_tempdir()
  fs::dir_create(fs::path(dir, c("one", "two")))
  tarball <- fs::path(dir, "bad.tar.gz")
  withr::with_dir(dir, utils::tar(tarball, c("one", "two"), compression = "gzip", tar = "internal"))
  local_fake_network(tarball)

  expect_error(suppressMessages(resolve_package_path("owner/repo")), "unexpected archive")
})

# CRAN -------------------------------------------------------------------------

test_that("current CRAN releases come from src/contrib", {
  local_test_cache()
  net <- local_fake_network(local_tarball("desc"), cran_db = c(desc = "1.4.3"))
  path <- suppressMessages(resolve_package_path("desc"))
  expect_equal(net$urls, "https://cloud.r-project.org/src/contrib/desc_1.4.3.tar.gz")
  expect_true(fs::file_exists(fs::path(path, "DESCRIPTION")))
})

test_that("older CRAN versions come from the archive", {
  local_test_cache()
  net <- local_fake_network(
    local_tarball("desc"),
    cran_db = c(desc = "1.4.3"),
    archive = c("desc_1.4.0.tar.gz", "desc_1.4.1.tar.gz")
  )
  suppressMessages(resolve_package_path("cran::desc@1.4.0"))
  expect_equal(net$urls, "https://cloud.r-project.org/src/contrib/Archive/desc/desc_1.4.0.tar.gz")

  expect_error(
    suppressMessages(resolve_package_path("cran::desc@0.9.0")),
    "not found on CRAN"
  )
})

test_that("archived CRAN packages use their last release", {
  local_test_cache()
  net <- local_fake_network(
    local_tarball("oldpkg"),
    cran_db = c(desc = "1.4.3"),
    archive = c("oldpkg_0.1.0.tar.gz", "oldpkg_0.10.0.tar.gz", "oldpkg_0.9.0.tar.gz")
  )
  messages <- capture_messages(resolve_package_path("oldpkg"))
  expect_match(messages, "archived on CRAN", all = FALSE)
  expect_match(net$urls, "Archive/oldpkg/oldpkg_0.10.0.tar.gz", fixed = TRUE)
})

test_that("unknown CRAN packages are reported", {
  local_test_cache()
  local_fake_network(local_tarball(), cran_db = c(desc = "1.4.3"))
  expect_error(resolve_package_path("pkginspectrNonexistentPkg"), "not found on CRAN")
})

test_that("CRAN downloads work without a configured mirror", {
  withr::local_options(repos = c(CRAN = "@CRAN@"))
  expect_equal(cran_repos(), "https://cloud.r-project.org")
  withr::local_options(repos = c(CRAN = "https://cran.example.org/"))
  expect_equal(cran_repos(), "https://cran.example.org")
})

# Real network (skipped on CRAN and offline) -----------------------------------

test_that("real GitHub and CRAN downloads work", {
  skip_on_cran()
  skip_if_offline(host = "github.com")
  local_test_cache()

  gh <- suppressMessages(resolve_package_path("ddauber/tidycomp"))
  expect_true(fs::file_exists(fs::path(gh, "DESCRIPTION")))

  cran <- suppressMessages(resolve_package_path("cran::desc@1.4.0"))
  expect_equal(desc::desc_get_version(cran), package_version("1.4.0"))
})
