test_that("pkg_summary combines structure and lines in one row per package", {
  summary <- pkg_summary(fixture_path(), quiet = TRUE)
  structure <- pkg_structure(fixture_path(), quiet = TRUE)
  lines <- pkg_lines(fixture_path(), quiet = TRUE)

  expect_s3_class(summary, "pkginspectr_tbl")
  expect_equal(nrow(summary), 1)
  expect_named(summary, union(names(structure), names(lines)))
  expect_equal(summary[names(structure)], structure, ignore_attr = TRUE)
  expect_equal(summary[names(lines)], lines, ignore_attr = TRUE)
})

test_that("pkg_summary reads each DESCRIPTION once", {
  path <- withr::local_tempdir()
  fs::dir_create(fs::path(path, "R"))
  writeLines("f <- function() 1", fs::path(path, "R", "f.R"))
  warnings <- character()
  withCallingHandlers(
    pkg_summary(path, quiet = TRUE),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(grep("No DESCRIPTION", warnings), 1)
})
