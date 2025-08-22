#' Plot package statistics by metric
#'
#' Display unicode bar charts for objects returned by [inspect_package()] or
#' [get_code_stats()]. Bars are scaled to each value's
#' proportion of the column total.
#'
#' @param x A tibble from [get_code_stats()] or [inspect_package()].
#' @param text_col Colour for package names and values.
#' @param bar_col Colour for the bars.
#' @param start Character marking the start of a bar.
#' @param end Character marking the end of a bar.
#' @param glyph Character used to fill the bar.
#' @param max_bars Maximum number of glyphs used for the bar fill. Bars
#'   auto-shrink to fit the current console width but never exceed `max_bars`.
#' @param name_spacing Extra spaces between the longest package name and the
#'   start of the bars.
#' @param bg_col Optional plot-wide background colour. Each printed line is
#'   padded and rendered with this background when supplied.
#' @param title Plot title. If `NULL` or empty, no title is printed.
#'
#' @return `x` invisibly.
#' @noRd
#' @keywords internal
plot_pkginspect_by_metric <- function(
  x,
  text_col = "#4670D4",
  bar_col = "#4670D4",
  start = "\u258F",
  end = "\u2595",
  glyph = "\u2588",
  max_bars = 25L,
  name_spacing = 3L,
  bg_col = NULL,
  title = NULL
) {
  # ---- styles (background-safe) ----
  text_style_base <- cli::make_ansi_style(text_col)
  bar_style_base <- cli::make_ansi_style(bar_col)

  if (!is.null(bg_col)) {
    bg <- cli::make_ansi_style(bg_col, bg = TRUE)
    text_style <- function(s) text_style_base(bg(s))
    bar_style <- function(s) bar_style_base(bg(s))
    header_style <- function(s) text_style_base(bg(s))
    bg_sep <- bg(" ")
    pad_with_bg <- function(s) {
      w <- cli::console_width()
      used <- cli::ansi_nchar(s, type = "width")
      paste0(s, bg(strrep(" ", max(0L, w - used))))
    }
  } else {
    text_style <- text_style_base
    bar_style <- bar_style_base
    header_style <- text_style_base
    bg_sep <- " "
    pad_with_bg <- identity
  }

  # ---- numeric columns ----
  num_cols <- names(x)[vapply(x, is.numeric, logical(1))]
  num_cols <- setdiff(num_cols, "package")
  if (!length(num_cols)) {
    cli::cli_warn("No numeric columns to plot.")
    return(invisible(x))
  }

  # ---- widths & auto-resize (rendered widths) ----
  console_w <- cli::console_width()
  pkg_width <- max(cli::ansi_nchar(x$package, type = "width"), na.rm = TRUE)
  value_width <- max(
    nchar(format(unlist(dplyr::select(x, dplyr::all_of(num_cols))))),
    na.rm = TRUE
  )

  # account for caps in width calc (since we now always show them)
  caps_w <- cli::ansi_nchar(start, type = "width") +
    cli::ansi_nchar(end, type = "width")
  reserved <- (pkg_width + name_spacing) + 1L + value_width + 1L + caps_w
  available <- max(10L, console_w - reserved)

  # body glyph visual width (force single-width fallback if needed)
  glyph_w <- cli::ansi_nchar(glyph, type = "width")
  if (!glyph_w %in% c(0L, 1L)) {
    glyph <- "\u2501" # ━
    glyph_w <- 1L
  }

  bar_width <- min(max_bars, as.integer(floor(available / max(1L, glyph_w))))

  # ---- optional title ----
  if (!is.null(title) && nzchar(title)) {
    cli::cat_line(pad_with_bg(header_style(cli::rule(left = title))))
    cli::cat_line(pad_with_bg(""))
  }

  # ---- render (metric-first) ----
  for (col in num_cols) {
    header <- header_style(cli::style_bold(col))
    cli::cat_line(pad_with_bg(header))

    values <- x[[col]]

    # multi-row: scale by column total; single-row: by sum across numeric cols
    total_val <- if (nrow(x) == 1L) {
      sum(dplyr::select(x, dplyr::all_of(num_cols)), na.rm = TRUE)
    } else {
      sum(values, na.rm = TRUE)
    }
    if (!is.finite(total_val) || total_val <= 0) {
      total_val <- 1
    }

    for (i in seq_len(nrow(x))) {
      pkg <- x$package[i]
      val <- values[i]
      frac <- if (is.finite(val)) as.numeric(val) / total_val else 0

      # raw body length (in glyphs)
      n_raw <- as.integer(round(frac * bar_width))

      # ≥1 body glyph for any positive value; allow 0 for zeros
      n_body <- if (is.finite(val) && val > 0 && n_raw < 1L) 1L else n_raw
      n_body <- max(0L, min(bar_width, n_body))

      # ALWAYS show caps; for zero values this renders just start+end (the stub)
      bar_body <- if (n_body > 0L) strrep(glyph, n_body) else ""
      bar_full <- paste0(start, bar_body, end)

      val_fmt <- format(val, width = value_width, big.mark = ",")
      name_padded <- sprintf("%-*s", pkg_width + name_spacing, pkg)

      line <- paste0(
        text_style(name_padded),
        bar_style(bar_full),
        bg_sep,
        text_style(val_fmt)
      )
      cli::cat_line(pad_with_bg(line))
    }

    cli::cat_line(pad_with_bg(""))
  }

  invisible(x)
}
