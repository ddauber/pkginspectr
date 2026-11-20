
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pkginspectr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/ddauber/pkginspectr/graph/badge.svg)](https://app.codecov.io/gh/ddauber/pkginspectr)
[![R-CMD-check](https://github.com/ddauber/pkginspectr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ddauber/pkginspectr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`pkginspectr` lets you peek inside R packages. It inspects local folders
as well as packages downloaded from GitHub or CRAN and returns tidy
tibbles that are easy to work with.

Key functions include:

- `inspect_package()` – summarises file structure, function files, tests
  and docs.
- `get_code_stats()` – counts lines of code, comments, roxygen and
  tests.
- `plot_pkg()` – draws simple but customisable bar charts of the results
  in the console.

Counts are designed to be trustworthy: lines are classified with R’s own
parser, and only files that `R CMD build` would ship are counted
(`.Rbuildignore`, `.git/` and friends are skipped), so a local folder
and a CRAN release of the same package give comparable results.

## Installation

You can install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("ddauber/pkginspectr")
```

## Inspect a package

Want a quick overview of what’s inside an R package? `inspect_package()`
counts files, R scripts, tests, documentation and data. Here we use the
small example package that ships with `pkginspectr`:

``` r
library(pkginspectr)

inspect_package(pkginspectr_example())
#> Inspecting examplepkg...
#> # A tibble: 1 × 11
#>   package    version num_files num_dirs num_r_files num_function_r_files
#>   <chr>      <chr>       <int>    <int>       <int>                <int>
#> 1 examplepkg 1.2.3           9        6           4                    1
#> # ℹ 5 more variables: num_non_function_r_files <int>, num_test_files <int>,
#> #   num_vignettes <int>, num_docs <int>, num_data_files <int>
```

`path` can be a local folder, a GitHub repository (`"owner/repo"`) or
the name of a CRAN package. Downloads are cached for the rest of the R
session.

## Compute code statistics

Go beyond file counts and see how much code is really there.
`get_code_stats()` analyses line counts across `R/`, `tests/` and
`data-raw/`.

``` r
get_code_stats(pkginspectr_example())
#> Analysing code for examplepkg...
#> # A tibble: 1 × 8
#>   package    version total_lines blank_lines comment_lines roxygen_lines
#>   <chr>      <chr>         <int>       <int>         <int>         <int>
#> 1 examplepkg 1.2.3            32           5             2             4
#> # ℹ 2 more variables: function_lines <int>, test_lines <int>
```

Files listed in `.Rbuildignore` are skipped by default; use
`include_ignored = TRUE` to count them too.

## Visualise in the console

No need for heavy graphics — `plot_pkg()` renders Unicode bar charts
right in your terminal.

``` r
get_code_stats(pkginspectr_example()) |>
  plot_pkg()
#> Analysing code for examplepkg...
#> ── pkginspectr: Code statistics ────────────────────────────────────────────────
#> Denominator: total_lines (32)
#> 
#> total_lines                    32 100.0%  ▏██████████████████████████████████████████████████▕
#> function_lines                  8  25.0%  ▏████████████▕
#> test_lines                      7  21.9%  ▏███████████▕
#> blank_lines                     5  15.6%  ▏████████▕
#> roxygen_lines                   4  12.5%  ▏██████▕
#> comment_lines                   2   6.2%  ▏███▕
```

You can customise the look to meet your needs: bars are just glyphs and
ANSI colours, so you can tweak:

- colours (`text_col`, `bar_col`, `bg_col`),
- bar shape (`glyph`, `start`, `end`),
- scale/fit (`max_bars`), or
- add a custom title (`title`).

## Compare multiple packages

Compare packages side by side — perfect for exploring codebases or
teaching.

``` r
inspect_package(c(
  "ddauber/pkginspectr",
  "ddauber/tidycomp",
  "ddauber/r4np"
)) |>
  plot_pkg(type = "metric")
#> num_files
#> pkginspectr   ▏████▕ 24
#> tidycomp      ▏██████████▕ 61
#> r4np          ▏███████████▕ 71
#> 
#> num_dirs
#> pkginspectr   ▏████▕  4
#> tidycomp      ▏█████▕  5
#> r4np          ▏████████████████▕ 17
#> 
#> num_r_files
#> pkginspectr   ▏████████▕ 15
#> tidycomp      ▏█████████████▕ 26
#> r4np          ▏████▕  9
#> 
#> num_function_r_files
#> pkginspectr   ▏████████▕ 10
#> tidycomp      ▏██████████████▕ 18
#> r4np          ▏███▕  4
#> 
#> num_non_function_r_files
#> pkginspectr   ▏███████▕  5
#> tidycomp      ▏███████████▕  8
#> r4np          ▏███████▕  5
#> 
#> num_test_files
#> pkginspectr   ▏███████▕  8
#> tidycomp      ▏██████████████▕ 15
#> r4np          ▏████▕  4
#> 
#> num_vignettes
#> pkginspectr   ▏▕  0
#> tidycomp      ▏▕  0
#> r4np          ▏▕  0
#> 
#> num_docs
#> pkginspectr   ▏██▕  4
#> tidycomp      ▏████████████▕ 29
#> r4np          ▏███████████▕ 25
#> 
#> num_data_files
#> pkginspectr   ▏▕  0
#> tidycomp      ▏▕  0
#> r4np          ▏█████████████████████████▕ 21
```

Here `type = "metric"` switches from a per‑package breakdown to a
metric‑centric view (e.g. compare `num_r_files`, `num_test_files` or
`num_docs` across packages).

This package is meant to be a small and fun package with some useful
functionality. Enjoy exploring your own R packages (as much as others’
packages)!
