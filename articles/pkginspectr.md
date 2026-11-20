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
#> # pkginspectr: Package structure, 1 package
#> # A tibble:    1 × 11
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
#> # pkginspectr: Lines of code, 1 package
#> # A tibble:    1 × 11
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
#> # pkginspectr: Package summary, 1 package
#> # A tibble:    1 × 20
#>   package    version num_files num_dirs num_r_files num_function_r_files
#>   <chr>      <chr>       <int>    <int>       <int>                <int>
#> 1 examplepkg 1.2.3           9        6           2                    1
#> # ℹ 14 more variables: num_non_function_r_files <int>, num_test_files <int>,
#> #   num_vignettes <int>, num_docs <int>, num_data_files <int>,
#> #   total_lines <int>, code_lines <int>, comment_lines <int>,
#> #   roxygen_lines <int>, blank_lines <int>, r_lines <int>, test_lines <int>,
#> #   other_lines <int>, function_lines <int>
```

## Individual functions: `pkg_functions()`

[`pkg_functions()`](https://ddauber.github.io/pkginspectr/reference/pkg_functions.md)
goes one level deeper and returns one row per named function, with its
size, number of arguments and whether the package exports it:

``` r

fns <- pkg_functions(example)
#> Listing functions in examplepkg...
fns
#> # pkginspectr: Functions, 3 functions in 1 package
#> # A tibble:    3 × 9
#>   package    version file     location name     line code_lines n_args exported
#>   <chr>      <chr>   <chr>    <chr>    <chr>   <int>      <int>  <int> <lgl>   
#> 1 examplepkg 1.2.3   R/funs.R R        add_one     5          3      1 TRUE    
#> 2 examplepkg 1.2.3   R/funs.R R        square     10          1      1 FALSE   
#> 3 examplepkg 1.2.3   R/funs.R R        compose    12          3      2 FALSE
```

That makes questions such as “which functions are longest?” or “how much
of the package is internal?” one-liners:

``` r

fns[order(fns$code_lines, decreasing = TRUE), c("name", "code_lines")]
#> Warning: Unknown or uninitialised column: `package`.
#> # pkginspectr: Functions, 3 functions in 0 packages
#> # A tibble:    3 × 2
#>   name    code_lines
#>   <chr>        <int>
#> 1 add_one          3
#> 2 compose          3
#> 3 square           1
table(exported = fns$exported)
#> exported
#> FALSE  TRUE 
#>     2     1
```

## Following a package over time

[`pkg_versions()`](https://ddauber.github.io/pkginspectr/reference/pkg_versions.md)
lists every CRAN release of a package with its date, and
[`pkg_history()`](https://ddauber.github.io/pkginspectr/reference/pkg_history.md)
runs
[`pkg_summary()`](https://ddauber.github.io/pkginspectr/reference/pkg_summary.md)
on several of them. By default it picks five releases spread across the
package’s history, always including the first and the latest:

``` r

pkg_versions("dplyr")

history <- pkg_history("dplyr")
history |>
  dplyr::select(package, version, date, num_r_files, code_lines)
```

Plot it with `type = "metric"` to see each metric grow release by
release. Rows are labelled with their version:

``` r

pkg_plot(history, type = "metric")
```

Use `versions` to pick releases yourself, and `cache = "persistent"` to
keep them for next time, since old releases never change:

``` r

pkg_history("dplyr", versions = c("0.8.0", "1.0.0", "1.1.0"), cache = "persistent")
```

## Derived metrics

pkginspectr returns raw counts on purpose: they are unambiguous and add
up. Ratios are often more useful, especially when comparing packages of
very different sizes, and each is a single line of dplyr. Copy the ones
you need.

``` r

library(dplyr, warn.conflicts = FALSE)

summary <- pkg_summary(example, quiet = TRUE)
```

### Package-level ratios

``` r

summary |>
  transmute(
    package,
    # Lines in tests/ for every line in R/
    test_to_code = test_lines / r_lines,
    # Share of all lines that are comments or roxygen documentation
    comment_density = (comment_lines + roxygen_lines) / total_lines,
    # Share of code that sits inside function definitions
    function_share = function_lines / code_lines,
    # Average size of a script in R/
    lines_per_r_file = r_lines / num_r_files
  )
#> # pkginspectr: Package statistics, 1 package
#> # A tibble:    1 × 5
#>   package    test_to_code comment_density function_share lines_per_r_file
#>   <chr>             <dbl>           <dbl>          <dbl>            <dbl>
#> 1 examplepkg         0.28           0.188          0.333             12.5
```

`r_lines` includes the roxygen comments in `R/`, so `test_to_code` is a
conservative measure: heavily documented packages score a little lower.

### Function-level summaries

[`pkg_functions()`](https://ddauber.github.io/pkginspectr/reference/pkg_functions.md)
lends itself to per-package summaries of function size and API surface:

``` r

functions <- pkg_functions(example, quiet = TRUE) |>
  filter(location == "R") |>
  group_by(package) |>
  summarise(
    n_functions = n(),
    n_exported = sum(exported, na.rm = TRUE),
    share_internal = mean(!exported, na.rm = TRUE),
    median_lines = median(code_lines),
    longest = max(code_lines),
    .groups = "drop"
  )
functions
#> # A tibble: 1 × 6
#>   package    n_functions n_exported share_internal median_lines longest
#>   <chr>            <int>      <int>          <dbl>        <int>   <int>
#> 1 examplepkg           3          1          0.667            3       3
```

### Combining tables

Joining the two gives ratios that need both, such as help pages per
exported function. `num_docs` counts every `.Rd` file, including
package-level and internal topics, so treat it as a rough indicator:

``` r

summary |>
  select(package, num_docs) |>
  left_join(functions, by = "package") |>
  transmute(package, num_docs, n_exported, docs_per_export = num_docs / n_exported)
#> # pkginspectr: Package structure, 1 package
#> # A tibble:    1 × 4
#>   package    num_docs n_exported docs_per_export
#>   <chr>         <int>      <int>           <dbl>
#> 1 examplepkg        1          1               1
```

### Plotting derived metrics

[`pkg_plot()`](https://ddauber.github.io/pkginspectr/reference/pkg_plot.md)
works on any data frame with a `package` column, so derived metrics can
be plotted too. Use `type = "metric"`, which scales each metric on its
own, because ratios on different scales do not belong on one bar chart:

``` r

pkg_summary(c("r-lib/cli", "r-lib/fs", "desc"), quiet = TRUE) |>
  transmute(
    package,
    test_to_code = test_lines / r_lines,
    comment_density = (comment_lines + roxygen_lines) / total_lines
  ) |>
  pkg_plot(type = "metric", title = "Testing and documentation")
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
#> # pkginspectr: Lines of code, 1 package
#> # A tibble:    1 × 11
#>   package version total_lines code_lines comment_lines roxygen_lines blank_lines
#>   <chr>   <chr>         <int>      <int>         <int>         <int>       <int>
#> 1 exampl… 1.2.3            32         21             2             4           5
#> # ℹ 4 more variables: r_lines <int>, test_lines <int>, other_lines <int>,
#> #   function_lines <int>
```
