# pkginspectr

`pkginspectr` lets you peek inside R packages. It inspects local folders
as well as packages downloaded from GitHub or CRAN and returns tidy
tibbles that are easy to work with.

Key functions include:

- [`pkg_structure()`](https://ddauber.github.io/pkginspectr/reference/pkg_structure.md)
  – counts files, folders, R scripts, tests, docs and data.
- [`pkg_lines()`](https://ddauber.github.io/pkginspectr/reference/pkg_lines.md)
  – counts lines of code, comments, roxygen and blank lines, in `R/` and
  in tests.
- [`pkg_summary()`](https://ddauber.github.io/pkginspectr/reference/pkg_summary.md)
  – both of the above in one table.
- [`pkg_functions()`](https://ddauber.github.io/pkginspectr/reference/pkg_functions.md)
  – one row per function, with its size, arguments and whether it is
  exported.
- [`pkg_history()`](https://ddauber.github.io/pkginspectr/reference/pkg_history.md)
  – follows a CRAN package across its releases (see
  [`pkg_versions()`](https://ddauber.github.io/pkginspectr/reference/pkg_versions.md)).
- [`pkg_plot()`](https://ddauber.github.io/pkginspectr/reference/pkg_plot.md)
  (or just [`plot()`](https://rdrr.io/r/graphics/plot.default.html)) –
  draws simple but customisable bar charts of the results in the
  console.

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

Want a quick overview of what’s inside an R package?
[`pkg_structure()`](https://ddauber.github.io/pkginspectr/reference/pkg_structure.md)
counts files, R scripts, tests, documentation and data. Here we use the
small example package that ships with `pkginspectr`:

``` r

library(pkginspectr)

pkg_structure(pkginspectr_example())
#> Inspecting examplepkg...
#> # pkginspectr: Package structure, 1 package
#> # A tibble:    1 × 11
#>   package    version num_files num_dirs num_r_files num_function_r_files
#>   <chr>      <chr>       <int>    <int>       <int>                <int>
#> 1 examplepkg 1.2.3           9        6           2                    1
#> # ℹ 5 more variables: num_non_function_r_files <int>, num_test_files <int>,
#> #   num_vignettes <int>, num_docs <int>, num_data_files <int>
```

`path` can be a local folder, a GitHub repository (`"owner/repo"`,
optionally `"owner/repo@v1.0.0"`) or a CRAN package (`"desc"`, or an
older release such as `"cran::desc@1.4.0"`). Downloads are cached for
the rest of the R session, or across sessions with
`cache = "persistent"`.

## Compute code statistics

Go beyond file counts and see how much code is really there.
[`pkg_lines()`](https://ddauber.github.io/pkginspectr/reference/pkg_lines.md)
breaks the lines in `R/`, `tests/` and `data-raw/` down by type (code,
comment, roxygen, blank) and by location; each breakdown adds up to
`total_lines`.

``` r

pkg_lines(pkginspectr_example())
#> Counting lines in examplepkg...
#> # pkginspectr: Lines of code, 1 package
#> # A tibble:    1 × 11
#>   package version total_lines code_lines comment_lines roxygen_lines blank_lines
#>   <chr>   <chr>         <int>      <int>         <int>         <int>       <int>
#> 1 exampl… 1.2.3            32         21             2             4           5
#> # ℹ 4 more variables: r_lines <int>, test_lines <int>, other_lines <int>,
#> #   function_lines <int>
```

Files listed in `.Rbuildignore` are skipped by default; use
`include_ignored = TRUE` to count them too. Need both tables?
[`pkg_summary()`](https://ddauber.github.io/pkginspectr/reference/pkg_summary.md)
returns them together and downloads each package only once.

## Visualise in the console

No need for heavy graphics —
[`pkg_plot()`](https://ddauber.github.io/pkginspectr/reference/pkg_plot.md),
or simply [`plot()`](https://rdrr.io/r/graphics/plot.default.html),
renders Unicode bar charts right in your terminal. Each metric is shown
as a share of its total:

``` r

pkg_lines(pkginspectr_example()) |>
  plot()
#> Counting lines in examplepkg...
#> ── pkginspectr: Lines of code ──────────────────────────────────────────────────
#> 
#> ── examplepkg 1.2.3 ────────────────────────────────────────────────────────────
#> Lines (share of total_lines)
#> total_lines     32  100.0%  ▏████████████████████████████████████████▕
#> code_lines      21   65.6%  ▏██████████████████████████▕
#> comment_lines    2    6.2%  ▏██▕
#> roxygen_lines    4   12.5%  ▏█████▕
#> blank_lines      5   15.6%  ▏██████▕
#> r_lines         25   78.1%  ▏███████████████████████████████▕
#> test_lines       7   21.9%  ▏█████████▕
#> other_lines      0    0.0%  ▏▕
#> function_lines   7   21.9%  ▏█████████▕
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

pkg_structure(c(
  "ddauber/pkginspectr",
  "ddauber/tidycomp",
  "ddauber/r4np"
)) |>
  pkg_plot(type = "metric")
#> ── pkginspectr: Package structure ──────────────────────────────────────────────
#> 
#> num_files
#> pkginspectr  ▏█████████████████▕ 48
#> tidycomp     ▏█████████████████████▕ 61
#> r4np         ▏█████████████████████████▕ 71
#> 
#> num_dirs
#> pkginspectr  ▏██████████████████████▕ 15
#> tidycomp     ▏███████▕  5
#> r4np         ▏█████████████████████████▕ 17
#> 
#> num_r_files
#> pkginspectr  ▏█████████████████████████▕ 11
#> tidycomp     ▏█████████████████████████▕ 11
#> r4np         ▏███████████▕  5
#> 
#> num_function_r_files
#> pkginspectr  ▏███████████████████████▕ 10
#> tidycomp     ▏█████████████████████████▕ 11
#> r4np         ▏█████████▕  4
#> 
#> num_non_function_r_files
#> pkginspectr  ▏█████████████████████████▕  1
#> tidycomp     ▏▕  0
#> r4np         ▏█████████████████████████▕  1
#> 
#> num_test_files
#> pkginspectr  ▏█████████████████▕ 10
#> tidycomp     ▏█████████████████████████▕ 15
#> r4np         ▏███████▕  4
#> 
#> num_vignettes
#> pkginspectr  ▏█████████████████████████▕  1
#> tidycomp     ▏▕  0
#> r4np         ▏▕  0
#> 
#> num_docs
#> pkginspectr  ▏███████▕  8
#> tidycomp     ▏█████████████████████████▕ 29
#> r4np         ▏██████████████████████▕ 25
#> 
#> num_data_files
#> pkginspectr  ▏▕  0
#> tidycomp     ▏▕  0
#> r4np         ▏█████████████████████████▕ 21
```

Here `type = "metric"` switches from a per‑package breakdown to a
metric‑centric view: one panel per metric, with bars scaled to the
largest package.

See
[`vignette("pkginspectr")`](https://ddauber.github.io/pkginspectr/articles/pkginspectr.md)
for a full tour.

This package is meant to be a small and fun package with some useful
functionality. Enjoy exploring your own R packages (as much as others’
packages)!
