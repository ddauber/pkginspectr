#' List the functions defined in packages
#'
#' Return one row per named function defined in the R source files of one or
#' more packages, with its size, number of arguments and whether it is
#' exported. Use it to find the largest functions, or to see how much of a
#' package is internal.
#'
#' A function is listed when it is assigned to a name at the top level of a
#' file, with `<-`, `=` or `<<-`, written as `function(...)` or `\(...)`.
#' Functions defined inside other functions, inside lists or R6 classes, or
#' with `setMethod()` are not listed. Files that fail to parse are skipped
#' with a warning.
#'
#' The same files are analysed as in [pkg_lines()]: R source files in `R/`,
#' `tests/` and `data-raw/` that `R CMD build` would ship.
#'
#' @inheritParams pkg_lines
#'
#' @return A tibble of class `pkginspectr_functions` with one row per
#'   function and columns:
#'   * `package`, `version`
#'   * `file`: path relative to the package root, e.g. `"R/utils.R"`.
#'   * `location`: `"R"`, `"tests"` or `"other"` (e.g. `data-raw/`).
#'   * `name`: the function's name.
#'   * `line`: the line where the definition starts.
#'   * `code_lines`: code lines in the definition, excluding comments and
#'     blank lines (as in [pkg_lines()]).
#'   * `n_args`: number of arguments, counting `...` as one.
#'   * `exported`: whether the package's `NAMESPACE` exports the function,
#'     via `export()` or `exportPattern()`. `NA` for functions outside `R/`
#'     or when the package has no readable `NAMESPACE`. S3 methods registered
#'     with `S3method()` are not exported in this sense.
#'
#'   Rows are ordered by package, file and line. A package without functions
#'   contributes no rows.
#' @seealso [pkg_lines()] for package-level line counts.
#' @export
#'
#' @examples
#' fns <- pkg_functions(pkginspectr_example())
#' fns
#'
#' # The largest functions
#' fns[order(fns$code_lines, decreasing = TRUE), c("name", "code_lines")]
#'
#' # Exported vs internal functions
#' table(fns$exported)
#'
#' @examplesIf interactive()
#' # The ten longest functions in cli
#' pkg_functions("r-lib/cli", quiet = TRUE) |>
#'   dplyr::slice_max(code_lines, n = 10)
pkg_functions <- function(
  path = ".",
  include_ignored = FALSE,
  quiet = FALSE,
  cache = c("session", "persistent", "none")
) {
  map_packages(
    path,
    function(pkg_path, info) functions_one(pkg_path, info, include_ignored),
    verb = "Listing functions in",
    quiet = quiet,
    cache = cache,
    class = "pkginspectr_functions",
    failed_rows = FALSE
  )
}

#' Function table for a single package
#'
#' @inheritParams lines_one
#' @return A tibble (see [pkg_functions()]), possibly with zero rows.
#' @keywords internal
#' @noRd
functions_one <- function(pkg_path, info, include_ignored = FALSE) {
  files <- list_pkg_files(pkg_path, include_ignored)$files
  r_files <- files[is_r_file(files) & grepl("^(R|tests|data-raw)/", files)]

  defs <- purrr::map(r_files, function(rel) {
    lines <- readLines(fs::path(pkg_path, rel), warn = FALSE, encoding = "UTF-8")
    out <- find_functions(lines, label = rel)
    if (nrow(out) > 0) {
      out$file <- rel
    }
    out
  }) |>
    purrr::list_rbind()

  if (is.null(defs) || nrow(defs) == 0) {
    defs <- empty_function_defs()
    defs$file <- character()
  }

  location <- ifelse(
    startsWith(defs$file, "R/"),
    "R",
    ifelse(startsWith(defs$file, "tests/"), "tests", "other")
  )
  exported <- is_exported(defs$name, namespace_info(pkg_path))
  exported[location != "R"] <- NA

  dplyr::tibble(
    package = rep(info$name, nrow(defs)),
    version = rep(info$version, nrow(defs)),
    file = defs$file,
    location = location,
    name = defs$name,
    line = defs$line,
    code_lines = defs$code_lines,
    n_args = defs$n_args,
    exported = exported
  )
}

#' Find top-level named function definitions in source lines
#'
#' @param lines Character vector of source lines.
#' @param label Name used in the warning if parsing fails.
#' @return A tibble with `name`, `line`, `code_lines` and `n_args`.
#' @keywords internal
#' @noRd
find_functions <- function(lines, label = "<text>") {
  exprs <- tryCatch(
    parse(text = lines, keep.source = TRUE),
    error = function(e) {
      reason <- conditionMessage(e)
      cli::cli_warn(c(
        "Could not parse {.file {label}}; its functions are not listed.",
        "x" = "{reason}"
      ))
      NULL
    }
  )
  if (length(exprs) == 0) {
    return(empty_function_defs())
  }

  code_lines <- code_line_numbers(utils::getParseData(exprs, includeText = FALSE))
  srcrefs <- attr(exprs, "srcref")

  defs <- lapply(seq_along(exprs), function(i) {
    def <- as_function_definition(exprs[[i]])
    if (is.null(def)) {
      return(NULL)
    }
    first <- srcrefs[[i]][[1]]
    last <- srcrefs[[i]][[3]]
    dplyr::tibble(
      name = def$name,
      line = first,
      code_lines = sum(code_lines >= first & code_lines <= last),
      n_args = def$n_args
    )
  })

  purrr::list_rbind(c(list(empty_function_defs()), defs))
}

#' If `expr` assigns a function to a name, return its name and arity
#' @noRd
as_function_definition <- function(expr) {
  if (!is.call(expr) || length(expr) != 3) {
    return(NULL)
  }
  op <- as.character(expr[[1]])
  if (!op %in% c("<-", "=", "<<-")) {
    return(NULL)
  }
  target <- expr[[2]]
  value <- expr[[3]]
  is_function <- is.call(value) && identical(value[[1]], as.name("function"))
  is_name <- is.symbol(target) || (is.character(target) && length(target) == 1)
  if (!is_function || !is_name) {
    return(NULL)
  }
  list(name = as.character(target), n_args = length(value[[2]]))
}

#' Zero-row function table with the right column types
#' @noRd
empty_function_defs <- function() {
  dplyr::tibble(
    name = character(),
    line = integer(),
    code_lines = integer(),
    n_args = integer()
  )
}

#' Parsed `NAMESPACE` of a package, or `NULL` if missing or unreadable
#' @noRd
namespace_info <- function(pkg_path) {
  if (!fs::file_exists(fs::path(pkg_path, "NAMESPACE"))) {
    return(NULL)
  }
  tryCatch(
    parseNamespaceFile(fs::path_file(pkg_path), fs::path_dir(pkg_path)),
    error = function(e) NULL
  )
}

#' Which names a parsed `NAMESPACE` exports (`NA` if there is none)
#' @noRd
is_exported <- function(names, ns) {
  if (is.null(ns)) {
    return(rep(NA, length(names)))
  }
  exported <- names %in% ns$exports
  for (pattern in ns$exportPatterns) {
    exported <- exported | grepl(pattern, names)
  }
  exported
}
