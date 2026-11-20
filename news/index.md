# Changelog

## pkginspectr (development version)

- curl is now listed in Suggests: the network tests use it to check for
  an internet connection, and failed on CI without it.

## pkginspectr 0.3.0

- New
  [`pkg_functions()`](https://ddauber.github.io/pkginspectr/reference/pkg_functions.md)
  returns one row per named function: its file, location, start line,
  code lines, number of arguments and whether it is exported (via
  `export()` or `exportPattern()`). Useful for finding the largest
  functions or measuring how much of a package is internal.

- New
  [`pkg_versions()`](https://ddauber.github.io/pkginspectr/reference/pkg_versions.md)
  lists every CRAN release of a package with its date, and
  [`pkg_history()`](https://ddauber.github.io/pkginspectr/reference/pkg_history.md)
  runs
  [`pkg_summary()`](https://ddauber.github.io/pkginspectr/reference/pkg_summary.md)
  on several releases (spread evenly across the history by default, or
  the versions you name) to show how a package grew over time.

- When several packages are analysed, one that fails (a typo, a failed
  download, an unreadable folder) no longer aborts the rest. It is
  reported in a single warning and kept as a row of `NA`s, so
  [`pkg_history()`](https://ddauber.github.io/pkginspectr/reference/pkg_history.md)
  still returns every other release. A single package, or all packages,
  failing is still an error.

- Results print a header saying what they contain, e.g.
  `# pkginspectr: Lines of code, 3 packages (1 failed)`, matching the
  titles used by
  [`pkg_plot()`](https://ddauber.github.io/pkginspectr/reference/pkg_plot.md).

- New “Derived metrics” section in
  [`vignette("pkginspectr")`](https://ddauber.github.io/pkginspectr/articles/pkginspectr.md)
  with copy-and-paste recipes for ratios such as test-to-code lines,
  comment density, share of code inside functions and help pages per
  export.

- [`pkg_plot()`](https://ddauber.github.io/pkginspectr/reference/pkg_plot.md)
  shows non-integer values with at most three decimals, so derived
  metrics such as ratios stay readable.

## pkginspectr 0.2.0

### Lifecycle changes

- All exported functions now share the `pkg_` prefix and are named after
  what they return. The old names still work but are deprecated:

  - [`inspect_package()`](https://ddauber.github.io/pkginspectr/reference/pkginspectr-deprecated.md)
    →
    [`pkg_structure()`](https://ddauber.github.io/pkginspectr/reference/pkg_structure.md)
  - [`get_code_stats()`](https://ddauber.github.io/pkginspectr/reference/pkginspectr-deprecated.md)
    →
    [`pkg_lines()`](https://ddauber.github.io/pkginspectr/reference/pkg_lines.md)
  - [`plot_pkg()`](https://ddauber.github.io/pkginspectr/reference/pkginspectr-deprecated.md)
    →
    [`pkg_plot()`](https://ddauber.github.io/pkginspectr/reference/pkg_plot.md)
  - [`clear_pkg_cache()`](https://ddauber.github.io/pkginspectr/reference/pkginspectr-deprecated.md)
    →
    [`pkg_cache_clear()`](https://ddauber.github.io/pkginspectr/reference/pkg_cache_clear.md)

### Changes to results

- [`pkg_lines()`](https://ddauber.github.io/pkginspectr/reference/pkg_lines.md)
  returns non-overlapping columns. `code_lines`, `comment_lines`,
  `roxygen_lines` and `blank_lines` add up to `total_lines`, and so do
  `r_lines`, `test_lines` and `other_lines`. `function_lines` now counts
  only code lines inside functions. The deprecated
  [`get_code_stats()`](https://ddauber.github.io/pkginspectr/reference/pkginspectr-deprecated.md)
  keeps its original columns.

- [`pkg_structure()`](https://ddauber.github.io/pkginspectr/reference/pkg_structure.md):
  `num_r_files`, `num_function_r_files` and `num_non_function_r_files`
  now count only scripts in `R/` (previously test and `data-raw/`
  scripts were included).

- Lines are now classified with R’s parser
  ([`utils::getParseData()`](https://rdrr.io/r/utils/getParseData.html))
  instead of regular expressions and brace counting. Functions assigned
  with `=`, `\(x)` lambdas and anonymous functions are counted; one-line
  functions no longer absorb the following line; `{`, `}` and `#` inside
  strings are no longer miscounted. Files that fail to parse produce a
  warning.

- Only files that `R CMD build` would ship are counted: `.Rbuildignore`,
  version-control folders and R’s built-in exclusions are respected, so
  local folders and CRAN tarballs are comparable. Use the new
  `include_ignored = TRUE` argument to count `.Rbuildignore`d files
  again.

- Lower-case `.r` files, `.Rnw` vignettes and
  `.rds`/`.csv`/`.tsv`/`.tab`/`.txt` data files are now recognised.

### New features

- Clearer package sources in `path`:

  - Explicit prefixes `local::`, `github::` and `cran::`. Unprefixed
    input still works, but a path that does not exist (e.g. `"./mypkg"`)
    is now an error instead of a GitHub request.
  - GitHub refs (`"r-lib/cli@v3.6.0"`), subfolders
    (`"github::owner/repo/subdir"`) and browser URLs
    (`github.com/owner/repo/tree/<ref>/<subdir>`).
  - Older CRAN releases (`"cran::desc@1.4.0"`) and archived CRAN
    packages.

- New `cache` argument (`"session"`, `"persistent"` or `"none"`). The
  persistent cache keeps fixed versions (CRAN releases, GitHub commit
  SHAs and version tags) across R sessions; GitHub branches are always
  fetched fresh.

- [`pkg_summary()`](https://ddauber.github.io/pkginspectr/reference/pkg_summary.md)
  returns
  [`pkg_structure()`](https://ddauber.github.io/pkginspectr/reference/pkg_structure.md)
  and
  [`pkg_lines()`](https://ddauber.github.io/pkginspectr/reference/pkg_lines.md)
  in one table, resolving and downloading each package only once.

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) works on any
  pkginspectr result.

- [`pkg_plot()`](https://ddauber.github.io/pkginspectr/reference/pkg_plot.md)
  documents all its styling arguments (`text_col`, `bar_col`, `bg_col`,
  `glyph`, `start`, `end`, `max_bars`, `title`), and both views accept
  the same set. Metric bars are scaled to each metric’s largest value,
  so packages are easy to compare. The package view groups metrics into
  “Files” (shares of `num_files`) and “Lines” (shares of `total_lines`),
  and shows `num_dirs` without a misleading percentage. Default titles
  describe the data.

- [`pkg_structure()`](https://ddauber.github.io/pkginspectr/reference/pkg_structure.md),
  [`pkg_lines()`](https://ddauber.github.io/pkginspectr/reference/pkg_lines.md)
  and
  [`pkg_summary()`](https://ddauber.github.io/pkginspectr/reference/pkg_summary.md)
  gain `quiet = TRUE` to suppress progress messages.

- New
  [`pkginspectr_example()`](https://ddauber.github.io/pkginspectr/reference/pkginspectr_example.md)
  returns the path to a small example package bundled with pkginspectr,
  so examples run offline.

- New “Get started” vignette:
  [`vignette("pkginspectr")`](https://ddauber.github.io/pkginspectr/articles/pkginspectr.md).

### Bug fixes

- GitHub downloads no longer use the GitHub API, so they are no longer
  limited to 60 requests per hour.

- CRAN downloads work when no CRAN mirror is set (e.g. under `Rscript`),
  and no longer miss packages that need a newer R version.

- Cached downloads are keyed by owner, repository and ref, so same-named
  repos from different owners no longer collide, and a failed download
  no longer leaves a broken cache entry behind.

- Files are classified by their path relative to the package root.
  Previously a package stored under a folder named `tests/`, `man/` or
  `data/` had all of its files misclassified.

- [`pkg_cache_clear()`](https://ddauber.github.io/pkginspectr/reference/pkg_cache_clear.md)
  returns the cleared path (or `NULL`) invisibly, as documented, and its
  message no longer lists both cache types when called without
  arguments.

- GitHub and CRAN lookups no longer fail when the suggested glue package
  is not installed; glue is no longer used.

### Other

- pkginspectr now requires R \>= 4.1.0 and purrr \>= 1.0.0, and no
  longer depends on tidyr or httr.

- Examples no longer use `\dontrun{}` and no longer clear the persistent
  cache. Added GitHub Actions workflows for `R CMD check` and test
  coverage.

## pkginspectr 0.1.0

- Added
  [`inspect_package()`](https://ddauber.github.io/pkginspectr/reference/pkginspectr-deprecated.md)
  which returns key statistics about an R package. In particular:

  - `package`: package name  
  - `version`: version number
  - `num_files`: number of files in the package
  - `num_dirs`: number of directories in the package
  - `num_r_files`: number of R files in the package
  - `num_function_r_files`: number of R files containing functions
  - `num_non_function_r_files`: number of R files not containing
    functions
  - `num_test_files`: number of test files in the package
  - `num_vignettes`: number of vignettes in the package
  - `num_docs`: number of documentation files in the package
  - `num_data_files`: number of data files in the package

- Added
  [`get_code_stats()`](https://ddauber.github.io/pkginspectr/reference/pkginspectr-deprecated.md)
  which returns code statistics for an R package. In particular:

  - `package`: package name
  - `version`: version number
  - `total_lines`: total number of lines of code
  - `blank_lines`: total number of blank lines
  - `comment_lines`: total number of comment lines
  - `roxygen_lines`: total number of roxygen lines
  - `function_lines`: total number of lines within functions
  - `test_lines`: total number of lines within test files

- Added
  [`plot_pkg()`](https://ddauber.github.io/pkginspectr/reference/pkginspectr-deprecated.md)
  which plots output from
  [`inspect_package()`](https://ddauber.github.io/pkginspectr/reference/pkginspectr-deprecated.md)
  or
  [`get_code_stats()`](https://ddauber.github.io/pkginspectr/reference/pkginspectr-deprecated.md)
  in the console. Users can modify colours and shapes of the plot to
  make it uniquely theirs. It offers two modes `type = "package"` and
  `type = "metric"`:

  - `type = "package"` plots package statistics for multiple packages
    grouped by package name.

  - `type = "metric"` plots package statistics grouped by metric.

- `pkginspectr` retrieves package information from

  - local packages
  - CRAN, and
  - GitHub repositories.

- Updated `README.md` with examples of how to use the package.

- Added tests for all functions using `testthat`.

- Create informative `README.md`.

## pkginspectr 0.0.0.9000

- Initial commit.
