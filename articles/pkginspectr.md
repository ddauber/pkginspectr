# Get started with pkginspectr

pkginspectr answers simple questions about R packages: how many files,
tests and help pages does a package have, and how many of its lines are
code, comments or documentation? It works on local folders, GitHub
repositories and CRAN packages, returns tidy tibbles, and draws bar
charts right in the console.

``` r

library(pkginspectr)
```

## A package to play with

pkginspectr ships a tiny example package so you can try everything
offline.
[`pkginspectr_example()`](https://ddauber.github.io/pkginspectr/reference/pkginspectr_example.md)
returns its path:

``` r

example <- pkginspectr_example()
```

Every function below takes a `path`. It can be:

- a local folder, such as `"."` for the package you are working on,
- a GitHub repository, such as `"r-lib/cli"`, or
- the name of a CRAN package, such as `"desc"`.

Remote packages can be pinned to a version, and prefixes make the source
explicit:

| `path`                     | Meaning                                  |
|----------------------------|------------------------------------------|
| `"r-lib/cli@v3.6.0"`       | GitHub tag, branch or commit             |
| `"github::apache/arrow/r"` | A package in a subfolder of a repository |
| `"cran::desc@1.4.0"`       | An older CRAN release                    |
| `"local::pkgs/mypkg"`      | A local folder, never looked up online   |

GitHub and CRAN packages are downloaded once and cached for the rest of
the R session. With `cache = "persistent"`, fixed versions (CRAN
releases, GitHub tags and commits) are kept across sessions too.
[`pkg_cache_clear()`](https://ddauber.github.io/pkginspectr/reference/pkg_cache_clear.md)
empties the cache.

## Files and folders: `pkg_structure()`

``` r

pkg_structure(example)
#> Inspecting examplepkg...
#> # A tibble: 1 × 11
#>   package    version num_files num_dirs num_r_files num_function_r_files
#>   <chr>      <chr>       <int>    <int>       <int>                <int>
#> 1 examplepkg 1.2.3           9        6           2                    1
#> # ℹ 5 more variables: num_non_function_r_files <int>, num_test_files <int>,
#> #   num_vignettes <int>, num_docs <int>, num_data_files <int>
```

`num_r_files` counts the scripts in `R/`, split into files that define
at least one function and files that do not. `num_test_files` counts R
files under `tests/`.

## Lines of code: `pkg_lines()`

``` r

lines <- pkg_lines(example)
#> Counting lines in examplepkg...
lines
#> # A tibble: 1 × 11
#>   package version total_lines code_lines comment_lines roxygen_lines blank_lines
#>   <chr>   <chr>         <int>      <int>         <int>         <int>       <int>
#> 1 exampl… 1.2.3            32         21             2             4           5
#> # ℹ 4 more variables: r_lines <int>, test_lines <int>, other_lines <int>,
#> #   function_lines <int>
```

The columns break `total_lines` down in two ways, and each adds up to
the total:

- **by type**: `code_lines`, `comment_lines`, `roxygen_lines` and
  `blank_lines`;
- **by location**: `r_lines` (in `R/`), `test_lines` (in `tests/`) and
  `other_lines` (e.g. `data-raw/`).

``` r

with(lines, code_lines + comment_lines + roxygen_lines + blank_lines)
#> [1] 32
with(lines, r_lines + test_lines + other_lines)
#> [1] 32
```

`function_lines` counts the code lines inside function definitions, so
it is part of `code_lines`.

Lines are classified with R’s own parser rather than pattern matching. A
`#` inside a string is not a comment, a `{` inside a string does not
open a block, and functions are found however they are written: with
`<-`, with `=`, as `\(x)` lambdas, or as anonymous functions passed to
other calls.

## Everything at once: `pkg_summary()`

[`pkg_summary()`](https://ddauber.github.io/pkginspectr/reference/pkg_summary.md)
combines both tables. For GitHub or CRAN packages it downloads each
package only once, so it is the quickest way to get everything:

``` r

pkg_summary(example)
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
```

## What gets counted

pkginspectr counts the files that `R CMD build` would put into the
package tarball. It skips version-control folders such as `.git/`, R’s
standard exclusions such as `.Rproj.user/` and `.Rhistory`, and anything
listed in the package’s `.Rbuildignore`, which often includes `docs/`
and `data-raw/`.

This matters when comparing packages: a local checkout and the CRAN
release of the same package give comparable numbers. To count
`.Rbuildignore`d files as well, use `include_ignored = TRUE`:

``` r

pkg_lines(".", include_ignored = TRUE)
```

## Plotting in the console

[`pkg_plot()`](https://ddauber.github.io/pkginspectr/reference/pkg_plot.md),
or simply [`plot()`](https://rdrr.io/r/graphics/plot.default.html),
draws bar charts in the console. The default view shows one panel per
package, with each metric as a share of its total:

``` r

plot(lines)
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

With several packages, `type = "metric"` shows one panel per metric
instead, with bars scaled to the largest value:

``` r

pkg_summary(c("r-lib/cli", "r-lib/fs", "desc")) |>
  pkg_plot(type = "metric")
```

Bars are plain characters, so you can restyle them:

``` r

pkg_plot(
  lines,
  bar_col = "darkorange",
  glyph = "=",
  start = "[",
  end = "]",
  max_bars = 20,
  title = "examplepkg at a glance"
)
#> ── examplepkg at a glance ──────────────────────────────────────────────────────
#> 
#> ── examplepkg 1.2.3 ────────────────────────────────────────────────────────────
#> Lines (share of total_lines)
#> total_lines     32  100.0%  [====================]
#> code_lines      21   65.6%  [=============]
#> comment_lines    2    6.2%  [=]
#> roxygen_lines    4   12.5%  [==]
#> blank_lines      5   15.6%  [===]
#> r_lines         25   78.1%  [================]
#> test_lines       7   21.9%  [====]
#> other_lines      0    0.0%  []
#> function_lines   7   21.9%  [====]
```

## Quiet mode

All functions report progress as they go. Use `quiet = TRUE` to turn the
messages off, for example in scripts or reports:

``` r

pkg_lines(example, quiet = TRUE)
#> # A tibble: 1 × 11
#>   package version total_lines code_lines comment_lines roxygen_lines blank_lines
#>   <chr>   <chr>         <int>      <int>         <int>         <int>       <int>
#> 1 exampl… 1.2.3            32         21             2             4           5
#> # ℹ 4 more variables: r_lines <int>, test_lines <int>, other_lines <int>,
#> #   function_lines <int>
```
