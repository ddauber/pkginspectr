#' Count lines of R code in packages
#'
#' Analyse the R source files (`.R` and `.r`) in `R/`, `tests/` and `data-raw/`
#' of one or more packages and count their lines, broken down two ways: by
#' *type* (code, comment, roxygen, blank) and by *location* (`R/`, `tests/`,
#' elsewhere).
#'
#' Lines are classified with R's own parser ([utils::getParseData()]), so
#' strings that contain `#` or unbalanced braces are counted correctly, and
#' functions are detected however they are written (`<-`, `=`, `\(x)`
#' lambdas, or anonymous functions inside calls). Files that fail to parse
#' fall back to a line-based heuristic and their function lines are not
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
#' @param quiet If `TRUE`, suppress progress messages.
#'
#' @return A `pkginspectr_tbl` with one row per package and columns:
#'   * `package`, `version`
#'   * `total_lines`: all lines in the analysed files.
#'
#'   By type (these four add up to `total_lines`):
#'   * `code_lines`: lines containing code, including code followed by a
#'     comment and lines inside multi-line strings.
#'   * `comment_lines`: lines containing only a regular (`#`) comment.
#'   * `roxygen_lines`: lines containing only a roxygen (`#'`) comment.
#'   * `blank_lines`: empty or whitespace-only lines.
#'
#'   By location (these three add up to `total_lines`):
#'   * `r_lines`: lines in `R/`.
#'   * `test_lines`: lines in `tests/`.
#'   * `other_lines`: lines elsewhere (e.g. `data-raw/`).
#'
#'   And:
#'   * `function_lines`: code lines inside function definitions (a subset of
#'     `code_lines`). Nested functions are counted once.
#' @seealso [pkg_structure()] for file counts, [pkg_summary()] for both, and
#'   [pkg_plot()] to visualise the result.
#' @export
#'
#' @examples
#' pkg_lines(pkginspectr_example())
#'
#' @examplesIf interactive()
#' # GitHub repositories and CRAN packages are downloaded and cached
#' pkg_lines(c("r-lib/cli", "desc"))
pkg_lines <- function(path = ".", include_ignored = FALSE, quiet = FALSE) {
  map_packages(
    path,
    function(pkg_path, info) lines_one(pkg_path, info, include_ignored),
    verb = "Counting lines in",
    quiet = quiet
  )
}

#' Line counts for a single package
#'
#' @param pkg_path Path to the package root.
#' @param info List with `name` and `version`.
#' @param include_ignored Passed to `list_pkg_files()`.
#' @return A one-row tibble (see [pkg_lines()]).
#' @keywords internal
#' @noRd
lines_one <- function(pkg_path, info, include_ignored = FALSE) {
  files <- list_pkg_files(pkg_path, include_ignored)$files
  r_files <- files[is_r_file(files) & grepl("^(R|tests|data-raw)/", files)]

  if (length(r_files) == 0) {
    cli::cli_warn("No .R files found in {.pkg {info$name}}.")
  }

  counts <- purrr::map(r_files, function(rel) {
    analyse_r_file(fs::path(pkg_path, rel), label = rel)
  })
  total <- vapply(counts, `[[`, integer(1), "total")
  location <- ifelse(
    startsWith(r_files, "R/"),
    "r",
    ifelse(startsWith(r_files, "tests/"), "test", "other")
  )
  sum_of <- function(field) {
    sum(vapply(counts, `[[`, integer(1), field))
  }

  dplyr::tibble(
    package = info$name,
    version = info$version,
    total_lines = sum(total),
    code_lines = sum_of("code"),
    comment_lines = sum_of("comment"),
    roxygen_lines = sum_of("roxygen"),
    blank_lines = sum_of("blank"),
    r_lines = sum(total[location == "r"]),
    test_lines = sum(total[location == "test"]),
    other_lines = sum(total[location == "other"]),
    function_lines = sum_of("function_lines")
  )
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
#' Every line is exactly one of code, comment, roxygen or blank, so those four
#' counts add up to `total`.
#'
#' @param lines Character vector of source lines.
#' @param label Name used in the warning if parsing fails.
#' @return A list with integer counts `total`, `code`, `comment`, `roxygen`,
#'   `blank`, `function_lines` and logical `has_function`.
#' @keywords internal
#' @noRd
count_lines <- function(lines, label = "<text>") {
  n <- length(lines)
  if (n == 0) {
    return(list(
      total = 0L, code = 0L, comment = 0L, roxygen = 0L, blank = 0L,
      function_lines = 0L, has_function = FALSE
    ))
  }

  is_blank <- grepl("^\\s*$", lines)
  is_roxygen_text <- grepl("^\\s*#'", lines)
  pd <- parse_lines(lines, label)

  if (is.null(pd)) {
    # Parse failure: classify by line prefix; function lines unknown.
    is_comment_text <- grepl("^\\s*#", lines)
    blank <- sum(is_blank)
    comment <- sum(is_comment_text & !is_roxygen_text)
    roxygen <- sum(is_roxygen_text)
    return(list(
      total = n,
      code = n - blank - comment - roxygen,
      comment = comment,
      roxygen = roxygen,
      blank = blank,
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
  fun_lines <- intersect(span_lines(fun_expr$line1, fun_expr$line2), code_lines)

  comment <- sum(!is_roxygen_text[comment_only])
  roxygen <- sum(is_roxygen_text[comment_only])
  blank <- sum(is_blank[setdiff(seq_len(n), code_lines)])

  list(
    total = n,
    code = n - comment - roxygen - blank,
    comment = comment,
    roxygen = roxygen,
    blank = blank,
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
