# Summarise package structure and lines of code

Combine
[`pkg_structure()`](https://ddauber.github.io/pkginspectr/reference/pkg_structure.md)
and
[`pkg_lines()`](https://ddauber.github.io/pkginspectr/reference/pkg_lines.md)
into one table. Each package is resolved (and, for GitHub or CRAN,
downloaded) only once, so this is the quickest way to get everything
about a set of packages.

## Usage

``` r
pkg_summary(
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

A `pkginspectr_tbl` with one row per package: `package`, `version`, then
the columns of
[`pkg_structure()`](https://ddauber.github.io/pkginspectr/reference/pkg_structure.md)
followed by those of
[`pkg_lines()`](https://ddauber.github.io/pkginspectr/reference/pkg_lines.md).

## See also

[`pkg_plot()`](https://ddauber.github.io/pkginspectr/reference/pkg_plot.md)
to visualise the result.

## Examples

``` r
pkg_summary(pkginspectr_example())
#> Summarising examplepkg...
#> # A tibble: 1 × 20
#>   package    version num_files num_dirs num_r_files num_function_r_files
#>   <chr>      <chr>       <int>    <int>       <int>                <int>
#> 1 examplepkg 1.2.3           9        6           2                    1
#> # ℹ 14 more variables: num_non_function_r_files <int>, num_test_files <int>,
#> #   num_vignettes <int>, num_docs <int>, num_data_files <int>,
#> #   total_lines <int>, code_lines <int>, comment_lines <int>,
#> #   roxygen_lines <int>, blank_lines <int>, r_lines <int>, test_lines <int>,
#> #   other_lines <int>, function_lines <int>

if (FALSE) { # interactive()
pkg_summary(c("r-lib/cli", "desc")) |>
  pkg_plot(type = "metric")
}
```
