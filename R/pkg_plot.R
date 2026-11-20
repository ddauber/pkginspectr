#' Plot package statistics in the console
#'
#' Draw Unicode bar charts of results from [pkg_structure()], [pkg_lines()]
#' or [pkg_summary()] directly in the console. Calling `plot()` on any of
#' these results does the same.
#'
#' There are two views:
#'
#' * `type = "package"` draws one panel per package. Bars show each metric's
#'   share of the package total: `num_files` for file counts and
#'   `total_lines` for line counts. `num_dirs` counts folders rather than
#'   files, so it is shown without a bar.
#' * `type = "metric"` draws one panel per metric, comparing packages. Bars
#'   are scaled to the largest value of each metric, so the biggest package
#'   always gets a full bar.
#'
#' @param x A result of [pkg_structure()], [pkg_lines()] or [pkg_summary()].
#'   Any data frame with a `package` column and numeric columns also works.
#' @param type `"package"` for one panel per package, or `"metric"` for one
#'   panel per metric comparing packages.
#' @param text_col,bar_col Colours for text and bars: a hex code such as
#'   `"#4670D4"` or a colour name. `NULL` uses the terminal's default colour.
#' @param bg_col Optional background colour for the whole chart. `NULL` (the
#'   default) leaves the terminal background unchanged.
#' @param glyph Character used to fill the bars. Must be a single-width
#'   character; wide characters such as emoji are replaced by a heavy
#'   horizontal line.
#' @param start,end Characters drawn at the start and end of each bar. Use
#'   `""` for no caps.
#' @param max_bars Maximum bar width in characters. Defaults to 40 for
#'   `type = "package"` and 25 for `type = "metric"`. Bars shrink to fit
#'   narrow consoles.
#' @param title Title shown above the chart. `NULL` (the default) chooses one
#'   based on the data; `""` shows no title.
#' @param ... Arguments passed on to `pkg_plot()`.
#'
#' @return `x`, invisibly.
#' @export
#'
#' @examples
#' lines <- pkg_lines(pkginspectr_example())
#'
#' # One panel per package
#' pkg_plot(lines)
#'
#' # The same, via plot()
#' plot(lines)
#'
#' # One panel per metric, for comparing packages
#' pkg_plot(lines, type = "metric")
#'
#' # Restyle the bars
#' pkg_plot(lines, bar_col = "darkorange", glyph = "=", start = "[", end = "]")
pkg_plot <- function(
  x,
  type = c("package", "metric"),
  text_col = "#4670D4",
  bar_col = "#4670D4",
  bg_col = NULL,
  glyph = "\u2588",
  start = "\u258F",
  end = "\u2595",
  max_bars = NULL,
  title = NULL
) {
  type <- match.arg(type)
  check_plot_data(x)

  style <- plot_style(text_col, bar_col, bg_col)
  bar <- list(glyph = check_glyph(glyph), start = start, end = end)
  max_bars <- max_bars %||% if (type == "package") 40L else 25L
  blocks <- plot_blocks(x)

  title <- title %||% default_plot_title(blocks)
  if (nzchar(title)) {
    style$line(style$text(cli::rule(left = title)))
    style$line("")
  }

  if (type == "package") {
    render_by_package(x, blocks, style, bar, max_bars)
  } else {
    render_by_metric(x, blocks, style, bar, max_bars)
  }

  invisible(x)
}

#' @rdname pkg_plot
#' @export
plot.pkginspectr_tbl <- function(x, ...) {
  pkg_plot(x, ...)
}

# Rendering --------------------------------------------------------------------

#' One panel per package; bars are shares of the block's denominator
#' @noRd
render_by_package <- function(x, blocks, style, bar, max_bars) {
  cols <- unlist(lapply(blocks, `[[`, "cols"), use.names = FALSE)
  label_w <- max(cli::ansi_nchar(cols, type = "width"))
  value_w <- max(nchar(fmt_num(unlist(x[cols]))))
  pct_w <- 6L
  bar_w <- bar_width(max_bars, label_w + value_w + pct_w + 6L, bar)

  for (i in seq_len(nrow(x))) {
    style$line(style$text(cli::rule(left = package_header(x, i))))

    for (block in blocks) {
      row_values <- unlist(x[i, block$cols])
      if (is.null(block$denom)) {
        scale <- safe_max(row_values)
        style$line(style$text(cli::style_bold(block$label)))
      } else {
        scale <- x[[block$denom]][i]
        style$line(style$text(cli::style_bold(
          paste0(block$label, " (share of ", block$denom, ")")
        )))
      }

      for (col in block$cols) {
        value <- x[[col]][i]
        shows_share <- !is.null(block$denom) && !col %in% block$no_share
        pct <- if (shows_share && is_positive(scale) && !is.na(value)) {
          sprintf("%5.1f%%", 100 * value / scale)
        } else {
          strrep(" ", pct_w)
        }
        bar_str <- if (col %in% block$no_share) {
          ""
        } else {
          make_bar(value, scale, bar_w, bar)
        }

        style$line(paste0(
          style$text(paste0(
            cli::ansi_align(col, label_w),
            "  ",
            cli::ansi_align(fmt_num(value), value_w, align = "right"),
            "  ",
            pct,
            "  "
          )),
          style$bar(bar_str)
        ))
      }
      style$line("")
    }
  }
}

#' One panel per metric; bars are scaled to the metric's largest value
#' @noRd
render_by_metric <- function(x, blocks, style, bar, max_bars) {
  cols <- unlist(lapply(blocks, `[[`, "cols"), use.names = FALSE)
  labels <- vapply(seq_len(nrow(x)), package_label, character(1), x = x)
  name_w <- max(cli::ansi_nchar(labels, type = "width"))
  value_w <- max(nchar(fmt_num(unlist(x[cols]))))
  bar_w <- bar_width(max_bars, name_w + value_w + 3L, bar)

  for (col in cols) {
    style$line(style$text(cli::style_bold(col)))
    scale <- safe_max(x[[col]])

    for (i in seq_len(nrow(x))) {
      value <- x[[col]][i]
      style$line(paste0(
        style$text(paste0(cli::ansi_align(labels[i], name_w), "  ")),
        style$bar(make_bar(value, scale, bar_w, bar)),
        style$text(paste0(
          " ",
          cli::ansi_align(fmt_num(value), value_w, align = "right")
        ))
      ))
    }
    style$line("")
  }
}

# Helpers ----------------------------------------------------------------------

#' Group numeric columns into blocks that share a denominator
#'
#' File counts (`num_*`) are shares of `num_files`, line counts (`*_lines`)
#' are shares of `total_lines`; anything else is plotted relative to the
#' largest value in its block.
#' @noRd
plot_blocks <- function(x) {
  num <- names(x)[vapply(x, is.numeric, logical(1))]
  file_cols <- num[startsWith(num, "num_")]
  line_cols <- num[endsWith(num, "_lines")]
  other_cols <- setdiff(num, c(file_cols, line_cols))

  blocks <- list(
    files = list(
      label = "Files",
      cols = file_cols,
      denom = if ("num_files" %in% file_cols) "num_files",
      no_share = "num_dirs"
    ),
    lines = list(
      label = "Lines",
      cols = line_cols,
      denom = if ("total_lines" %in% line_cols) "total_lines",
      no_share = character()
    ),
    other = list(
      label = "Other",
      cols = other_cols,
      denom = NULL,
      no_share = character()
    )
  )
  Filter(function(b) length(b$cols) > 0, blocks)
}

#' Title describing what kind of result is being plotted
#' @noRd
default_plot_title <- function(blocks) {
  kinds <- names(blocks)
  what <- if (all(c("files", "lines") %in% kinds)) {
    "Package summary"
  } else if ("files" %in% kinds) {
    "Package structure"
  } else if ("lines" %in% kinds) {
    "Lines of code"
  } else {
    "Package statistics"
  }
  paste0("pkginspectr: ", what)
}

#' Abort unless `x` has a `package` column and at least one numeric column
#' @noRd
check_plot_data <- function(x, call = rlang::caller_env()) {
  if (!is.data.frame(x)) {
    cli::cli_abort(
      "{.arg x} must be a data frame, not {.obj_type_friendly {x}}.",
      call = call
    )
  }
  if (inherits(x, "pkginspectr_functions")) {
    cli::cli_abort(
      c(
        "{.arg x} has one row per function, not one per package.",
        "i" = "Summarise it first, or use {.fn pkg_lines} for package totals."
      ),
      call = call
    )
  }
  if (!"package" %in% names(x)) {
    cli::cli_abort("{.arg x} must have a {.field package} column.", call = call)
  }
  if (!any(vapply(x, is.numeric, logical(1)))) {
    cli::cli_abort("{.arg x} has no numeric columns to plot.", call = call)
  }
  invisible(x)
}

#' Text, bar and line-printing functions for the chosen colours
#' @noRd
plot_style <- function(text_col, bar_col, bg_col) {
  text <- if (is.null(text_col)) identity else cli::make_ansi_style(text_col)
  bar <- if (is.null(bar_col)) identity else cli::make_ansi_style(bar_col)

  line <- if (is.null(bg_col)) {
    function(s) cli::cat_line(s)
  } else {
    bg <- cli::make_ansi_style(bg_col, bg = TRUE)
    function(s) {
      pad <- max(0L, cli::console_width() - cli::ansi_nchar(s, type = "width"))
      cli::cat_line(bg(paste0(s, strrep(" ", pad))))
    }
  }

  list(text = text, bar = bar, line = line)
}

#' Replace a glyph that is not exactly one column wide
#' @noRd
check_glyph <- function(glyph) {
  if (cli::ansi_nchar(glyph, type = "width") != 1L) "\u2501" else glyph
}

#' Number of glyphs available for a bar, given the other columns' width
#' @noRd
bar_width <- function(max_bars, reserved, bar) {
  caps <- cli::ansi_nchar(bar$start, type = "width") +
    cli::ansi_nchar(bar$end, type = "width")
  available <- cli::console_width() - reserved - caps
  as.integer(min(max_bars, max(10L, available)))
}

#' Draw one bar: `value / scale` of `width` glyphs between the caps
#'
#' Any positive value gets at least one glyph, so small but non-zero values
#' stay visible; zero or missing values show only the caps.
#' @noRd
make_bar <- function(value, scale, width, bar) {
  n <- 0L
  if (!is.na(value) && is_positive(scale)) {
    n <- as.integer(round(value / scale * width))
    if (value > 0 && n < 1L) n <- 1L
    n <- max(0L, min(width, n))
  }
  paste0(bar$start, strrep(bar$glyph, n), bar$end)
}

#' Package name, plus its version when names alone are ambiguous
#' @noRd
package_label <- function(x, i) {
  name <- as.character(x$package[i])
  version <- if ("version" %in% names(x)) x$version[i] else NA
  if (anyDuplicated(x$package) && !is.na(version)) {
    paste(name, version)
  } else {
    name
  }
}

#' Package name and version for a panel header
#' @noRd
package_header <- function(x, i) {
  version <- if ("version" %in% names(x)) x$version[i] else NA
  if (is.na(version)) {
    as.character(x$package[i])
  } else {
    paste(x$package[i], version)
  }
}

#' Format values for display: thousands separators, at most 3 decimals
#' @noRd
fmt_num <- function(x) {
  out <- vapply(
    x,
    function(v) format(round(v, 3), big.mark = ",", scientific = FALSE, trim = TRUE),
    character(1)
  )
  out[is.na(x)] <- "NA"
  out
}

safe_max <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else max(x)
}

is_positive <- function(x) {
  length(x) == 1 && !is.na(x) && x > 0
}
