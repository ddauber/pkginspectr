# Count files and folders in packages

Examine the file structure of one or more packages and count files,
folders, R scripts, tests, vignettes, help pages and data files.

## Usage

``` r
pkg_structure(
  path = ".",
  include_ignored = FALSE,
  quiet = FALSE,
  cache = c("session", "persistent", "none")
)
```

## Arguments

- path:

  A character vector of packages. Each element is one of:

  - A local folder, such as `"."` or `"~/code/mypkg"`. Use
    `"local::path"` to be explicit.

  - A GitHub repository, such as `"r-lib/cli"` or a `github.com` URL.
    Add `@ref` for a branch, tag or commit (`"r-lib/cli@v3.6.0"`). For a
    package in a subfolder, use the `github::` prefix:
    `"github::owner/repo/subdir"`.

  - A CRAN package, such as `"desc"`. Add `@version` for an older
    release (`"cran::desc@1.4.0"`). Archived packages are supported.

  Without a prefix, existing local folders take precedence, then
  `"owner/repo"` is treated as GitHub and a bare name as CRAN. A path
  that does not exist (e.g. `"./mypkg"`) is an error, not a GitHub
  request.

  With several packages, one that cannot be analysed (a typo, a failed
  download) does not stop the others: it is reported in a warning and
  returned as a row of `NA`s whose `package` is the specification as
  given
  ([`pkg_functions()`](https://ddauber.github.io/pkginspectr/reference/pkg_functions.md)
  leaves it out instead). A single package that fails, or all packages
  failing, is an error.

- include_ignored:

  If `TRUE`, also analyse files matched by the package's
  `.Rbuildignore`. Version-control folders and R's built-in build
  exclusions are always skipped.

- quiet:

  If `TRUE`, suppress progress messages.

- cache:

  Where to keep downloaded GitHub and CRAN packages:

  - `"session"` (default): a temporary folder, removed when R closes.

  - `"persistent"`: a user cache folder kept across R sessions. Only
    fixed versions are stored there: CRAN releases and GitHub refs that
    look like a commit SHA or a version tag (`"owner/repo@v1.2.0"`).
    Requests that can change over time (no ref, or a branch) always
    fetch the latest code and use the session cache instead.

  - `"none"`: always download afresh.

  Use
  [`pkg_cache_clear()`](https://ddauber.github.io/pkginspectr/reference/pkg_cache_clear.md)
  to empty a cache.

## Value

A `pkginspectr_tbl` with one row per package and columns:

- `package`, `version`

- `num_files`, `num_dirs`: files and folders in the package.

- `num_r_files`: R source files (`.R` or `.r`) in `R/`.

- `num_function_r_files`, `num_non_function_r_files`: files in `R/` that
  do / do not define at least one function (detected by the R parser).
  These two add up to `num_r_files`.

- `num_test_files`: R files under `tests/`.

- `num_vignettes`: `.Rmd`, `.qmd` or `.Rnw` files under `vignettes/`.

- `num_docs`: `.Rd` help files under `man/`.

- `num_data_files`: data files under `data/` (`.rda`, `.RData`, `.rds`,
  `.csv`, `.tsv`, `.tab`, `.txt`, optionally compressed).

## Details

Only files that `R CMD build` would ship are counted: version-control
folders (e.g. `.git/`), R's built-in build exclusions (e.g.
`.Rproj.user/`, `.Rhistory`) and, unless `include_ignored = TRUE`,
anything listed in `.Rbuildignore` (e.g. `docs/`, `data-raw/`). This
makes results for a local folder comparable with results for a CRAN
tarball.

## See also

[`pkg_lines()`](https://ddauber.github.io/pkginspectr/reference/pkg_lines.md)
for line counts,
[`pkg_summary()`](https://ddauber.github.io/pkginspectr/reference/pkg_summary.md)
for both, and
[`pkg_plot()`](https://ddauber.github.io/pkginspectr/reference/pkg_plot.md)
to visualise the result.

## Examples

``` r
pkg_structure(pkginspectr_example())
#> Inspecting examplepkg...
#> # pkginspectr: Package structure, 1 package
#> # A tibble:    1 × 11
#>   package    version num_files num_dirs num_r_files num_function_r_files
#>   <chr>      <chr>       <int>    <int>       <int>                <int>
#> 1 examplepkg 1.2.3           9        6           2                    1
#> # ℹ 5 more variables: num_non_function_r_files <int>, num_test_files <int>,
#> #   num_vignettes <int>, num_docs <int>, num_data_files <int>

if (FALSE) { # interactive()
# GitHub repositories and CRAN packages are downloaded and cached
pkg_structure(c("r-lib/cli", "desc"))
}
```
