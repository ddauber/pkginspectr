# Print headers for pkginspectr results

pkginspectr results print like any tibble, with an extra first header
line that says what they contain, e.g.
`# pkginspectr: Lines of code, 3 packages (1 failed)`. The wording
matches the titles of
[`pkg_plot()`](https://ddauber.github.io/pkginspectr/reference/pkg_plot.md).

## Usage

``` r
# S3 method for class 'pkginspectr_tbl'
tbl_sum(x, ...)

# S3 method for class 'pkginspectr_functions'
tbl_sum(x, ...)
```

## Arguments

- x:

  A pkginspectr result.

- ...:

  Passed on to the tibble method.

## Value

A named character vector of header lines, see
[`pillar::tbl_sum()`](https://pillar.r-lib.org/reference/tbl_sum.html).
