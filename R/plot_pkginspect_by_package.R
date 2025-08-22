#' Visualise code statistics for each package
#'
#' Print a compact console chart for each row of `x` showing counts,
#' percentages and bars. Any positive value is guaranteed at least one glyph.
#'
#' @param x A tibble from [get_code_stats()] or [inspect_package()]; one row per
#'   package.
#' @param denom_col Denominator column name or "auto".
#' @param text_col ANSI colour for text (hex or named); `NULL` for default.
#' @param bar_col ANSI colour for bars (hex or named); `NULL` for default.
#' @param start Left bar cap.
#' @param end Right bar cap.
#' @param glyph Bar body glyph.
#' @param max_bars Maximum bar width in glyphs.
#' @param bg_col Optional background fill colour for the whole line; `NULL` for none.
#' @param title Optional global title. If `NULL`, shows
#'   "pkginspectr: Code statistics" and a per-package subheader when needed.
#' @param spacer_lines Blank lines between packages.
#'
#' @return `x` invisibly.
#' @keywords internal
#' @noRd
plot_pkginspect_by_package <- function(
  x,
  denom_col = "auto",
  text_col = "#4670D4",
  bar_col = "#4670D4",
  start = "\u258F",
  end = "\u2595",
  glyph = "\u2588",
  max_bars = 50L,
  bg_col = NULL,
  title = NULL,
  spacer_lines = 1L
) {
  stopifnot(nrow(x) >= 1L)

  # styles
  txt_style <- if (!is.null(text_col)) {
    cli::make_ansi_style(text_col)
  } else {
    identity
  }
  bar_style <- if (!is.null(bar_col)) {
    cli::make_ansi_style(bar_col)
  } else {
    identity
  }
  bg_style <- if (!is.null(bg_col)) {
    cli::make_ansi_style(bg_col, bg = TRUE)
  } else {
    identity
  }

  # global title once when multiple packages, else single uses header inside
  if (!is.null(title)) {
    cli::cat_line(cli::rule(left = title))
  } else if (nrow(x) > 1L) {
    cli::cat_line(cli::rule(left = "pkginspectr: Code statistics"))
  }

  # helper: render one row (mirrors single-package renderer)
  render_one <- function(row_df, header_label = NULL) {
    # pick denominator column automatically if requested
    dcol <- denom_col
    if (identical(dcol, "auto")) {
      if ("total_lines" %in% names(row_df)) {
        dcol <- "total_lines"
      } else if ("num_files" %in% names(row_df)) {
        dcol <- "num_files"
      } else {
        dcol <- NULL
      }
    }

    # denominator value
    if (!is.null(dcol) && dcol %in% names(row_df)) {
      dval <- row_df[[dcol]][1]
    } else {
      dval <- sum(dplyr::select(row_df, dplyr::where(is.numeric)), na.rm = TRUE)
      dcol <- "sum(numeric)"
    }
    if (is.null(dval) || is.na(dval) || !is.finite(dval)) {
      dval <- 0
    }

    # per-row header
    if (nrow(x) == 1L) {
      cli::cat_line(cli::rule(left = "pkginspectr: Code statistics"))
    } else {
      if (is.null(header_label)) {
        header_label <- "Package"
      }
      cli::cat_line(cli::rule(left = header_label))
    }
    cli::cat_line(txt_style(paste0(
      "Denominator: ",
      dcol,
      " (",
      round(dval),
      ")"
    )))
    cli::cat_line("")

    # numeric metrics, long form
    long <- tidyr::pivot_longer(
      row_df,
      cols = dplyr::where(is.numeric),
      names_to = "metric",
      values_to = "value"
    ) |>
      dplyr::arrange(dplyr::desc(value))

    min_nonzero_glyphs <- 1L # ensure at least one body glyph for val > 0

    for (i in seq_len(nrow(long))) {
      lbl <- long$metric[i]
      val <- long$value[i]
      frac <- if (dval > 0) as.numeric(val) / dval else NA_real_
      n_raw <- if (is.na(frac)) 0L else as.integer(round(frac * max_bars))

      # guarantee a visible body for any positive value
      if (
        !is.na(frac) && is.finite(val) && val > 0 && n_raw < min_nonzero_glyphs
      ) {
        n_glyphs <- min_nonzero_glyphs
      } else {
        n_glyphs <- n_raw
      }
      n_glyphs <- max(0L, min(max_bars, n_glyphs))

      # ALWAYS show caps — even for zeros (caps-only stub)
      bar_body <- if (n_glyphs > 0L) strrep(glyph, n_glyphs) else ""
      bar_str <- paste0(start, bar_body, end)

      perc_str <- if (!is.na(frac)) sprintf("%5.1f%%", 100 * frac) else "     "

      line <- sprintf(
        "%-24s %8s %6s  %s",
        lbl,
        round(val),
        perc_str,
        bar_style(bar_str)
      )

      cli::cat_line(bg_style(txt_style(line)))
    }
  }

  # iterate rows
  for (i in seq_len(nrow(x))) {
    row <- x[i, , drop = FALSE]
    pkg_label <- if ("package" %in% names(row)) {
      paste0(row$package[1])
    } else {
      paste0("Package ", i, "/", nrow(x))
    }
    render_one(row, header_label = pkg_label)

    if (i < nrow(x) && spacer_lines > 0L) {
      cli::cat_line(strrep("\n", spacer_lines))
    }
  }

  invisible(x)
}

# define global variables to avoid R CMD check warnings
utils::globalVariables("value")
