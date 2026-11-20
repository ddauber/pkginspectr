# List the CRAN releases of a package

Return every version of a package published on CRAN, with its release
date, sorted by version number (oldest first). Use it to choose versions
for
[`pkg_history()`](https://ddauber.github.io/pkginspectr/reference/pkg_history.md)
or for `"cran::pkg@version"` specifications.

## Usage

``` r
pkg_versions(package)
```

## Arguments

- package:

  Name of a CRAN package, such as `"dplyr"`. A `"cran::"` prefix is
  accepted.

## Value

A tibble with one row per release and columns `package`, `version`,
`date` (a `Date`) and `current` (`TRUE` for the release currently on
CRAN). Archived packages have no current release.

## Details

Dates are those recorded by CRAN: the file date in CRAN's archive for
older releases, and the `Date/Publication` field for the current one. A
date is `NA` if CRAN does not report it.

## See also

[`pkg_history()`](https://ddauber.github.io/pkginspectr/reference/pkg_history.md)
to analyse several releases.

## Examples

``` r
if (FALSE) { # interactive()
pkg_versions("desc")
}
```
