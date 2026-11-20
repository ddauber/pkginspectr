test_that("list_pkg_files returns package-relative paths", {
  listing <- list_pkg_files(fixture_path())
  expect_setequal(listing$files, c(
    "DESCRIPTION", "NAMESPACE", "R/data.R", "R/funs.R",
    "data/example.csv", "man/add_one.Rd", "tests/testthat.R",
    "tests/testthat/test-funs.r", "vignettes/intro.Rmd"
  ))
  expect_setequal(listing$dirs, c(
    "R", "data", "man", "tests", "tests/testthat", "vignettes"
  ))
})

test_that("list_pkg_files drops what R CMD build drops", {
  pkg <- local_fixture_copy()
  fs::dir_create(fs::path(pkg, c(".git/objects", ".Rproj.user", "docs")))
  fs::file_create(fs::path(pkg, c(
    ".git/HEAD", ".git/objects/ab", ".Rproj.user/state", ".Rhistory",
    "R/.DS_Store", "docs/index.html", ".Rbuildignore", ".lintr"
  )))
  writeLines(c("^docs$", "", "^\\.lintr$"), fs::path(pkg, ".Rbuildignore"))

  listing <- list_pkg_files(pkg)
  expect_false(any(grepl("^(\\.git|\\.Rproj\\.user|docs)(/|$)", listing$files)))
  expect_false(any(grepl("^(\\.git|\\.Rproj\\.user|docs)(/|$)", listing$dirs)))
  expect_false(any(c(".Rhistory", "R/.DS_Store", ".Rbuildignore", ".lintr") %in%
    listing$files))
  expect_length(listing$files, 9)

  # include_ignored only lifts .Rbuildignore, never VCS or R's built-ins
  all <- list_pkg_files(pkg, include_ignored = TRUE)
  expect_true(all(c("docs/index.html", ".lintr") %in% all$files))
  expect_false(any(grepl("^\\.git(/|$)", all$files)))
  expect_false(".Rhistory" %in% all$files)
})

test_that("is_build_excluded applies patterns to parent folders, ignoring case", {
  rel <- c("data-raw", "data-raw/make.R", "Data-Raw/x.R", "R/data-raw.R")
  is_dir <- c(TRUE, FALSE, FALSE, FALSE)
  expect_equal(
    is_build_excluded(rel, is_dir, "^data-raw$"),
    c(TRUE, TRUE, TRUE, FALSE)
  )
})

test_that("version-control names are only excluded as directories", {
  expect_equal(
    is_build_excluded(c("CVS", "R/CVS/a.R"), c(FALSE, FALSE), character()),
    c(FALSE, TRUE)
  )
})

test_that("is_r_file accepts upper- and lower-case extensions", {
  expect_equal(is_r_file(c("a.R", "b.r", "c.Rmd", "d.Rd")), c(TRUE, TRUE, FALSE, FALSE))
})
