#' Compute code-level statistics for packages
#'
#' Analyse the R source files (`.R` and `.r`) in `R/`, `tests/` and `data-raw/`
#' of one or more packages and report line counts by type: total, blank,
#' comments, roxygen comments, function definitions and lines in test files.
#' The package name and version are also returned.
#'
#' Lines are classified with R's own parser ([utils::getParseData()]), so
#' strings that contain `#` or unbalanced braces are counted correctly, and
#' functions are detected regardless of how they are written (`<-`, `=`,
#' `\(x)` lambdas, or anonymous functions inside calls). Files that fail to
#' parse fall back to a line-based heuristic and their function lines are not
#' counted; a warning names each such file.
#'
#' Only files that `R CMD build` would ship are analysed, so results for a
#' local folder are comparable with results for a CRAN tarball. In particular,
#' anything listed in `.Rbuildignore` (often `data-raw/`) is skipped unless
#' `include_ignored = TRUE`.
#'
#' @param path A character vector of local package paths, GitHub repositories
#'   (`"owner/repo"`) or CRAN package names.
#' @param include_ignored If `TRUE`, also analyse files matched by the
#'   package's `.Rbuildignore`. Version-control folders and R's built-in
#'   build exclusions are always skipped.
#'
#' @return A `pkginspectr_tbl` with one row per package and columns:
#'   * `package`, `version`
#'   * `total_lines`: all lines in the analysed files.
#'   * `blank_lines`: empty or whitespace-only lines (outside strings).
#'   * `comment_lines`: lines containing only a non-roxygen comment.
#'   * `roxygen_lines`: lines containing only a roxygen (`#'`) comment.
#'   * `function_lines`: lines inside function definitions, including their
#'     header line and any comments or blank lines within them. Nested
#'     functions are counted once.
#'   * `test_lines`: total lines of files under `tests/`.
#'
#'   `blank_lines`, `comment_lines` and `roxygen_lines` do not overlap;
#'   `function_lines` and `test_lines` overlap with them.
#' @export
#'
#' @examples
#' \dontrun{
#' get_code_stats("r-lib/cli")
#' }
get_code_stats <- function(path = ".", include_ignored = FALSE) {
  paths <- resolve_package_path(path)

  result <- purrr::map_dfr(paths, function(pkg_path) {
    if (!fs::dir_exists(pkg_path)) {
      cli::cli_abort("The path {.path {pkg_path}} does not exist.")
    }

    pkg_info <- get_pkg_name_version(pkg_path)
    pkg_name <- pkg_info$name
    pkg_version <- pkg_info$version

    cli::cli_inform("Analysing code for {.pkg {pkg_name}}...")

    files <- list_pkg_files(pkg_path, include_ignored)$files
    r_files <- files[is_r_file(files) & grepl("^(R|tests|data-raw)/", files)]

    if (length(r_files) == 0) {
      cli::cli_warn("No .R files found in {.pkg {pkg_name}}.")
      return(dplyr::tibble(
        package = pkg_name,
        version = pkg_version,
        total_lines = 0,
        blank_lines = 0,
        comment_lines = 0,
        roxygen_lines = 0,
        function_lines = 0,
        test_lines = 0
      ))
    }

    stats <- purrr::map(r_files, function(rel) {
      counts <- analyse_r_file(fs::path(pkg_path, rel), label = rel)
      dplyr::tibble(
        total_lines = counts$total,
        blank_lines = counts$blank,
        comment_lines = counts$comment,
        roxygen_lines = counts$roxygen,
        function_lines = counts$function_lines,
        test_lines = if (startsWith(rel, "tests/")) counts$total else 0L
      )
    })

    dplyr::bind_rows(stats) |>
      dplyr::summarise(
        dplyr::across(dplyr::everything(), ~ sum(.x, na.rm = TRUE))
      ) |>
      dplyr::mutate(package = pkg_name, version = pkg_version, .before = 1)
  })

  class(result) <- c("pkginspectr_tbl", class(result))
  result
}

#' Read and classify the lines of one R file
#'
#' @param file Path to the file.
#' @param label Name used in warnings (usually the package-relative path).
#' @return See `count_lines()`.
#' @keywords internal
#' @noRd
analyse_r_file <- function(file, label = file) {
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  count_lines(lines, label = label)
}

#' Classify lines of R code using the parser
#'
#' @param lines Character vector of source lines.
#' @param label Name used in the warning if parsing fails.
#' @return A list with integer counts `total`, `blank`, `comment`, `roxygen`,
#'   `function_lines` and logical `has_function`.
#' @keywords internal
#' @noRd
count_lines <- function(lines, label = "<text>") {
  n <- length(lines)
  if (n == 0) {
    return(list(
      total = 0L, blank = 0L, comment = 0L, roxygen = 0L,
      function_lines = 0L, has_function = FALSE
    ))
  }

  is_blank <- grepl("^\\s*$", lines)
  is_roxygen_text <- grepl("^\\s*#'", lines)
  pd <- parse_lines(lines, label)

  if (is.null(pd)) {
    # Parse failure: classify by line prefix; function lines unknown.
    is_comment_text <- grepl("^\\s*#", lines)
    return(list(
      total = n,
      blank = sum(is_blank),
      comment = sum(is_comment_text & !is_roxygen_text),
      roxygen = sum(is_roxygen_text),
      function_lines = 0L,
      has_function = any(grepl("\\bfunction\\s*\\(|\\\\\\s*\\(", lines))
    ))
  }

  terminals <- pd[pd$terminal, , drop = FALSE]
  code_tok <- terminals[terminals$token != "COMMENT", , drop = FALSE]
  comment_tok <- terminals[terminals$token == "COMMENT", , drop = FALSE]

  # A token can span lines (multi-line strings), so expand its range.
  code_lines <- span_lines(code_tok$line1, code_tok$line2)
  comment_only <- setdiff(unique(comment_tok$line1), code_lines)

  fun_tok <- terminals[terminals$token %in% c("FUNCTION", "'\\\\'"), ,
    drop = FALSE
  ]
  fun_expr <- pd[pd$id %in% fun_tok$parent, , drop = FALSE]
  fun_lines <- span_lines(fun_expr$line1, fun_expr$line2)

  list(
    total = n,
    blank = sum(is_blank[setdiff(seq_len(n), code_lines)]),
    comment = sum(!is_roxygen_text[comment_only]),
    roxygen = sum(is_roxygen_text[comment_only]),
    function_lines = length(fun_lines),
    has_function = nrow(fun_tok) > 0
  )
}

#' Parse lines and return parse data, or `NULL` with a warning on failure
#' @noRd
parse_lines <- function(lines, label) {
  tryCatch(
    {
      exprs <- parse(text = lines, keep.source = TRUE)
      utils::getParseData(exprs, includeText = FALSE)
    },
    error = function(e) {
      reason <- conditionMessage(e)
      cli::cli_warn(c(
        "Could not parse {.file {label}}; its function lines are not counted.",
        "x" = "{reason}"
      ))
      NULL
    }
  )
}

#' Unique line numbers covered by a set of `line1`-`line2` ranges
#' @noRd
span_lines <- function(line1, line2) {
  if (length(line1) == 0) {
    return(integer())
  }
  unique(unlist(Map(seq.int, line1, line2), use.names = FALSE))
}
