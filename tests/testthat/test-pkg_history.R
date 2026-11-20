# A fake CRAN where "desc" has three archived releases plus a current one.
local_fake_desc <- function(env = parent.frame()) {
  local_test_cache(env = env)
  local_fake_network(
    local_tarball("desc", env = env),
    cran_db = c(desc = "1.4.3"),
    archive = c(
      "desc_1.10.0.tar.gz" = "2022-06-01",
      "desc_1.2.0.tar.gz" = "2018-05-01",
      "desc_1.9.0.tar.gz" = "2021-09-28"
    ),
    published = "2023-12-10",
    env = env
  )
}

# pkg_versions() ---------------------------------------------------------------

test_that("pkg_versions lists archived and current releases, oldest first", {
  local_fake_desc()
  releases <- pkg_versions("desc")

  expect_named(releases, c("package", "version", "date", "current"))
  # sorted by version number, not alphabetically (1.10.0 after 1.9.0)
  expect_equal(releases$version, c("1.2.0", "1.4.3", "1.9.0", "1.10.0"))
  expect_equal(
    releases$date,
    as.Date(c("2018-05-01", "2023-12-10", "2021-09-28", "2022-06-01"))
  )
  expect_equal(releases$current, c(FALSE, TRUE, FALSE, FALSE))
  expect_equal(unique(releases$package), "desc")
})

test_that("pkg_versions handles archived packages and unknown packages", {
  local_test_cache()
  local_fake_network(
    local_tarball(),
    cran_db = c(desc = "1.4.3"),
    archive = c("oldpkg_0.1.0.tar.gz", "oldpkg_0.2.0.tar.gz")
  )
  old <- pkg_versions("cran::oldpkg")
  expect_equal(old$version, c("0.1.0", "0.2.0"))
  expect_false(any(old$current))

  expect_error(pkg_versions("nopkg"), "not found on CRAN")
})

test_that("pkg_versions validates its input", {
  expect_error(pkg_versions(c("a", "b")), "single CRAN package name")
  expect_error(pkg_versions(NA_character_), "single CRAN package name")
  expect_error(pkg_versions("desc@1.4.0"), "without a version")
  expect_error(pkg_versions("not a name"), "Invalid CRAN package name")
})

test_that("archive pages without dates give NA dates, not an error", {
  local_test_cache()
  local_mocked_bindings(read_url_lines = function(url) {
    c('<a href="desc_1.0.0.tar.gz">desc_1.0.0.tar.gz</a>', "no tarball here")
  })
  listing <- cran_archive_listing("desc", "https://cran.example.org")
  expect_equal(listing$version, "1.0.0")
  expect_true(is.na(listing$date))
})

# pkg_history() ----------------------------------------------------------------

test_that("pkg_history analyses releases spread across the history", {
  net <- local_fake_desc()
  history <- pkg_history("desc", n = 2, quiet = TRUE)

  expect_s3_class(history, "pkginspectr_tbl")
  expect_equal(nrow(history), 2)
  expect_equal(names(history)[1:3], c("package", "version", "date"))
  # first and latest release, in order
  expect_equal(history$date, as.Date(c("2018-05-01", "2022-06-01")))
  expect_equal(net$urls, c(
    "https://cloud.r-project.org/src/contrib/Archive/desc/desc_1.2.0.tar.gz",
    "https://cloud.r-project.org/src/contrib/Archive/desc/desc_1.10.0.tar.gz"
  ))
  expect_true(all(c("num_files", "total_lines") %in% names(history)))
})

test_that("pkg_history uses all releases when n is large", {
  local_fake_desc()
  expect_equal(nrow(pkg_history("desc", n = 50, quiet = TRUE)), 4)
})

test_that("pkg_history analyses the versions you name, oldest first", {
  net <- local_fake_desc()
  history <- pkg_history("desc", versions = c("1.4.3", "1.2.0"), quiet = TRUE)
  expect_equal(history$date, as.Date(c("2018-05-01", "2023-12-10")))
  expect_match(net$urls[2], "src/contrib/desc_1.4.3.tar.gz", fixed = TRUE)

  expect_error(
    pkg_history("desc", versions = c("1.2.0", "9.9.9"), quiet = TRUE),
    "9.9.9"
  )
})

test_that("pkg_history validates n", {
  local_fake_desc()
  expect_error(pkg_history("desc", n = 0), "whole number")
  expect_error(pkg_history("desc", n = 2.5), "whole number")
  expect_error(pkg_history("desc", n = c(1, 2)), "whole number")
})

test_that("pick_spread always keeps the latest release", {
  expect_equal(pick_spread(10, 1), 10)
  expect_equal(pick_spread(10, 2), c(1, 10))
  expect_equal(pick_spread(10, 4), c(1, 4, 7, 10))
  expect_equal(pick_spread(3, 5), 1:3)
})

test_that("real CRAN release history can be listed", {
  skip_on_cran()
  skip_if_offline(host = "cloud.r-project.org")
  releases <- pkg_versions("desc")
  expect_true("1.4.0" %in% releases$version)
  expect_equal(releases$date[releases$version == "1.4.0"], as.Date("2021-09-28"))
  expect_equal(sum(releases$current), 1)
})
