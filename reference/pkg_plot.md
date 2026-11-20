# Plot package statistics in the console

Draw Unicode bar charts of results from
[`pkg_structure()`](https://ddauber.github.io/pkginspectr/reference/pkg_structure.md),
[`pkg_lines()`](https://ddauber.github.io/pkginspectr/reference/pkg_lines.md)
or
[`pkg_summary()`](https://ddauber.github.io/pkginspectr/reference/pkg_summary.md)
directly in the console. Calling
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) on any of these
results does the same.

## Usage

``` r
pkg_plot(
  x,
  type = c("package", "metric"),
  text_col = "#4670D4",
  bar_col = "#4670D4",
  bg_col = NULL,
  glyph = "█",
  start = "▏",
  end = "▕",
  max_bars = NULL,
  title = NULL
)

# S3 method for class 'pkginspectr_tbl'
plot(x, ...)
```

## Arguments

- x:

  A result of
  [`pkg_structure()`](https://ddauber.github.io/pkginspectr/reference/pkg_structure.md),
  [`pkg_lines()`](https://ddauber.github.io/pkginspectr/reference/pkg_lines.md)
  or
  [`pkg_summary()`](https://ddauber.github.io/pkginspectr/reference/pkg_summary.md).
  Any data frame with a `package` column and numeric columns also works.

- type:

  `"package"` for one panel per package, or `"metric"` for one panel per
  metric comparing packages.

- text_col, bar_col:

  Colours for text and bars: a hex code such as `"#4670D4"` or a colour
  name. `NULL` uses the terminal's default colour.

- bg_col:

  Optional background colour for the whole chart. `NULL` (the default)
  leaves the terminal background unchanged.

- glyph:

  Character used to fill the bars. Must be a single-width character;
  wide characters such as emoji are replaced by a heavy horizontal line.

- start, end:

  Characters drawn at the start and end of each bar. Use `""` for no
  caps.

- max_bars:

  Maximum bar width in characters. Defaults to 40 for `type = "package"`
  and 25 for `type = "metric"`. Bars shrink to fit narrow consoles.

- title:

  Title shown above the chart. `NULL` (the default) chooses one based on
  the data; `""` shows no title.

- ...:

  Arguments passed on to `pkg_plot()`.

## Value

`x`, invisibly.

## Details

There are two views:

- `type = "package"` draws one panel per package. Bars show each
  metric's share of the package total: `num_files` for file counts and
  `total_lines` for line counts. `num_dirs` counts folders rather than
  files, so it is shown without a bar.

- `type = "metric"` draws one panel per metric, comparing packages. Bars
  are scaled to the largest value of each metric, so the biggest package
  always gets a full bar.

## Examples

``` r
lines <- pkg_lines(pkginspectr_example())
#> Counting lines in examplepkg...

# One panel per package
pkg_plot(lines)
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
#> 

# The same, via plot()
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
#> 

# One panel per metric, for comparing packages
pkg_plot(lines, type = "metric")
#> ── pkginspectr: Lines of code ──────────────────────────────────────────────────
#> 
#> total_lines
#> examplepkg  ▏█████████████████████████▕ 32
#> 
#> code_lines
#> examplepkg  ▏█████████████████████████▕ 21
#> 
#> comment_lines
#> examplepkg  ▏█████████████████████████▕  2
#> 
#> roxygen_lines
#> examplepkg  ▏█████████████████████████▕  4
#> 
#> blank_lines
#> examplepkg  ▏█████████████████████████▕  5
#> 
#> r_lines
#> examplepkg  ▏█████████████████████████▕ 25
#> 
#> test_lines
#> examplepkg  ▏█████████████████████████▕  7
#> 
#> other_lines
#> examplepkg  ▏▕  0
#> 
#> function_lines
#> examplepkg  ▏█████████████████████████▕  7
#> 

# Restyle the bars
pkg_plot(lines, bar_col = "darkorange", glyph = "=", start = "[", end = "]")
#> ── pkginspectr: Lines of code ──────────────────────────────────────────────────
#> 
#> ── examplepkg 1.2.3 ────────────────────────────────────────────────────────────
#> Lines (share of total_lines)
#> total_lines     32  100.0%  [========================================]
#> code_lines      21   65.6%  [==========================]
#> comment_lines    2    6.2%  [==]
#> roxygen_lines    4   12.5%  [=====]
#> blank_lines      5   15.6%  [======]
#> r_lines         25   78.1%  [===============================]
#> test_lines       7   21.9%  [=========]
#> other_lines      0    0.0%  []
#> function_lines   7   21.9%  [=========]
#> 
```
