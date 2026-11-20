# Deprecated functions

**\[deprecated\]**

These functions were renamed in pkginspectr 0.2.0 so that all exported
functions share the `pkg_` prefix and are named after what they return:

- `inspect_package()` is now
  [`pkg_structure()`](https://ddauber.github.io/pkginspectr/reference/pkg_structure.md).

- `get_code_stats()` is now
  [`pkg_lines()`](https://ddauber.github.io/pkginspectr/reference/pkg_lines.md).

- `plot_pkg()` is now
  [`pkg_plot()`](https://ddauber.github.io/pkginspectr/reference/pkg_plot.md).

- `clear_pkg_cache()` is now
  [`pkg_cache_clear()`](https://ddauber.github.io/pkginspectr/reference/pkg_cache_clear.md).

The old names still work but warn. `get_code_stats()` keeps returning
its original columns; use
[`pkg_lines()`](https://ddauber.github.io/pkginspectr/reference/pkg_lines.md)
for the full, non-overlapping breakdown.

## Usage

``` r
inspect_package(path = ".", include_ignored = FALSE)

get_code_stats(path = ".", include_ignored = FALSE)

plot_pkg(x, type = c("package", "metric"), ...)

clear_pkg_cache(type = c("session", "persistent"))
```

## Arguments

- path, include_ignored:

  See
  [`pkg_lines()`](https://ddauber.github.io/pkginspectr/reference/pkg_lines.md).

- x, ...:

  See
  [`pkg_plot()`](https://ddauber.github.io/pkginspectr/reference/pkg_plot.md).
  Arguments that
  [`pkg_plot()`](https://ddauber.github.io/pkginspectr/reference/pkg_plot.md)
  does not accept are ignored with a warning.

- type:

  See
  [`pkg_plot()`](https://ddauber.github.io/pkginspectr/reference/pkg_plot.md)
  or
  [`pkg_cache_clear()`](https://ddauber.github.io/pkginspectr/reference/pkg_cache_clear.md).
