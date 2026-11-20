# Follow a package across its CRAN releases

Run
[`pkg_summary()`](https://ddauber.github.io/pkginspectr/reference/pkg_summary.md)
on several CRAN releases of one package to see how it grew over time. By
default `n` releases are picked evenly across the package's history,
always including the first and the latest; or name the versions you
want.

## Usage

``` r
pkg_history(
  package,
  versions = NULL,
  n = 5,
  include_ignored = FALSE,
  quiet = FALSE,
  cache = c("session", "persistent", "none")
)
```

## Arguments

- package:

  Name of a CRAN package, such as `"dplyr"`.

- versions:

  Optional character vector of versions to analyse, e.g.
  `c("0.8.0", "1.0.0")`. See
  [`pkg_versions()`](https://ddauber.github.io/pkginspectr/reference/pkg_versions.md)
  for what is available. If given, `n` is ignored.

- n:

  Number of releases to pick when `versions` is not given. If the
  package has `n` releases or fewer, all are used.

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

A `pkginspectr_tbl` like
[`pkg_summary()`](https://ddauber.github.io/pkginspectr/reference/pkg_summary.md),
with one row per release, sorted by version number (oldest first), and
an extra `date` column after `version`. Plot it with
`pkg_plot(type = "metric")` to compare releases side by side.

## Details

Each release is downloaded from CRAN. Old releases never change, so
`cache = "persistent"` is a good choice if you will revisit them.

## See also

[`pkg_versions()`](https://ddauber.github.io/pkginspectr/reference/pkg_versions.md)
to list the releases.

## Examples

``` r
if (FALSE) { # interactive()
# Five releases spread across desc's history
history <- pkg_history("desc")
history
pkg_plot(history, type = "metric")

# Specific releases, kept across R sessions
pkg_history("desc", versions = c("1.2.0", "1.4.0"), cache = "persistent")
}
```
