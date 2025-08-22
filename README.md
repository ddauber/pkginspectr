
# pkginspectr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

`pkginspectr` lets you peek inside R packages. It inspects local folders
as well as packages downloaded from GitHub or CRAN and returns tidy
tibbles that are easy to work with.

Key functions include:

- `inspect_package()` – summarises file structure, function files, tests
  and docs.
- `get_code_stats()` – counts lines of code, comments and tests.
- `plot_pkginspect()` – draws simple but customisable bar charts of the
  results in the console.

## Installation

You can install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("ddauber/pkginspectr")
```

## Inspect a package

Want a quick overview of what’s inside an R package? `pkginspectr` lets
you count files, functions, and other components in seconds.

``` r
library(pkginspectr)

inspect_package("ddauber/pkginspectr")
```

    ## Detected GitHub repo: "ddauber/pkginspectr"
    ## Detected default branch: "main"
    ## Inspecting pkginspectr...

    ## # A tibble: 1 × 11
    ##   package     version num_files num_dirs num_r_files num_function_r_files
    ##   <chr>       <chr>       <int>    <int>       <int>                <int>
    ## 1 pkginspectr 0.0.1          27        4          15                    8
    ## # ℹ 5 more variables: num_non_function_r_files <int>, num_test_files <int>,
    ## #   num_vignettes <int>, num_docs <int>, num_data_files <int>

## Compute code statistics

Go beyond file counts and see how much code is really there.
`get_code_stats()` analyses line counts across the package.

``` r
get_code_stats("ddauber/pkginspectr")
```

    ## Detected GitHub repo: "ddauber/pkginspectr"
    ## Detected default branch: "main"
    ## Using cached GitHub repo: "pkginspectr_main"
    ## Analysing code for pkginspectr...

    ## # A tibble: 1 × 8
    ##   package     version total_lines blank_lines comment_lines roxygen_lines
    ##   <chr>       <chr>         <int>       <int>         <int>         <int>
    ## 1 pkginspectr 0.0.1          1280         138            46           223
    ## # ℹ 2 more variables: function_lines <dbl>, test_lines <dbl>

## Visualise in the console

No need for heavy graphics — `plot_pkg()` renders Unicode bar charts
right in your terminal.

``` r
inspect_package("ddauber/pkginspectr") |>
  plot_pkg()
```

    ## Detected GitHub repo: "ddauber/pkginspectr"
    ## Detected default branch: "main"
    ## Using cached GitHub repo: "pkginspectr_main"
    ## Inspecting pkginspectr...

    ## ── pkginspectr: Code statistics ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    ## Denominator: num_files (27)
    ## 
    ## num_files                      27 100.0%  ▏██████████████████████████████████████████████████▕
    ## num_r_files                    15  55.6%  ▏████████████████████████████▕
    ## num_function_r_files            8  29.6%  ▏███████████████▕
    ## num_test_files                  8  29.6%  ▏███████████████▕
    ## num_non_function_r_files        7  25.9%  ▏█████████████▕
    ## num_dirs                        4  14.8%  ▏███████▕
    ## num_docs                        4  14.8%  ▏███████▕
    ## num_vignettes                   0   0.0%  ▏▕
    ## num_data_files                  0   0.0%  ▏▕

You can customise the look to meet your needs: bars are just glyphs and
ANSI colours, so you can tweak: - colours (e.g. `text_col`, `bar_col`,
etc.), - bar shape (`glyph`, `start`, `end`), - scale/fit (`max_bars`),
or - add a custom title (`title`)

## Inspect multiple packages at once

Compare packages side by side — perfect for exploring codebases or
teaching.

``` r
inspect_package(c(
  "ddauber/pkginspectr",
  "ddauber/tidycomp",
  "ddauber/r4np"
)) |>
  plot_pkg(type = "metric")
```

    ## Detected GitHub repo: "ddauber/pkginspectr"
    ## Detected default branch: "main"
    ## Using cached GitHub repo: "pkginspectr_main"
    ## Detected GitHub repo: "ddauber/tidycomp"
    ## Detected default branch: "main"
    ## Detected GitHub repo: "ddauber/r4np"
    ## Detected default branch: "master"
    ## Inspecting pkginspectr...
    ## Inspecting tidycomp...
    ## Inspecting r4np...

    ## num_files
    ## pkginspectr   ▏███▕  27
    ## tidycomp      ▏████████▕  65
    ## r4np          ▏█████████████▕ 105
    ## 
    ## num_dirs
    ## pkginspectr   ▏███▕   4
    ## tidycomp      ▏████▕   5
    ## r4np          ▏██████████████████▕  21
    ## 
    ## num_r_files
    ## pkginspectr   ▏███████▕  15
    ## tidycomp      ▏████████████▕  26
    ## r4np          ▏███████▕  15
    ## 
    ## num_function_r_files
    ## pkginspectr   ▏███████▕   8
    ## tidycomp      ▏█████████████▕  15
    ## r4np          ▏█████▕   6
    ## 
    ## num_non_function_r_files
    ## pkginspectr   ▏██████▕   7
    ## tidycomp      ▏██████████▕  11
    ## r4np          ▏████████▕   9
    ## 
    ## num_test_files
    ## pkginspectr   ▏███████▕   8
    ## tidycomp      ▏██████████████▕  15
    ## r4np          ▏████▕   4
    ## 
    ## num_vignettes
    ## pkginspectr   ▏▕   0
    ## tidycomp      ▏▕   0
    ## r4np          ▏▕   0
    ## 
    ## num_docs
    ## pkginspectr   ▏██▕   4
    ## tidycomp      ▏████████████▕  29
    ## r4np          ▏███████████▕  25
    ## 
    ## num_data_files
    ## pkginspectr   ▏▕   0
    ## tidycomp      ▏▕   0
    ## r4np          ▏█████████████████████████▕  21

Here `type = "metric"` switches from a per‑package breakdown to a
metric‑centric view (e.g., compare `num_r_files`, `num_test_files`,
`docs`, etc. across packages).

This package is meant to be a small and fun package with some useful
functionality. Enjoy exploring your own R packages (as much as others’
packages)!
