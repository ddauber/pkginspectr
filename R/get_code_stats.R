#' Compute code-level statistics for packages
#'
#' Analyse all `.R` files in one or more packages and report line counts by
#' type: total, blank, comments, roxygen comments, function bodies and lines in
#' test files. The package name and version are also returned.
#'
#' @param path A character vector or list of local package paths.
#'
#' @return A `pkginspectr_tbl` with one row per package containing summary line
#'   counts.
#' @export
#'
#' @examples
#' \dontrun{
#' # Analyse the installed "stats" package
#' get_code_stats(system.file(package = "stats"))
#' }
get_code_stats <- function(path = ".") {
  paths <- resolve_package_path(path)

  result <- purrr::map_dfr(paths, function(pkg_path) {
    if (!fs::dir_exists(pkg_path)) {
      cli::cli_abort("The path {.path {pkg_path}} does not exist.")
    }

    pkg_info <- get_pkg_name_version(pkg_path)
    pkg_name <- pkg_info$name
    pkg_version <- pkg_info$version

    cli::cli_inform("Analysing code for {.pkg {pkg_name}}...")

    r_dirs <- c("R", "tests", "data-raw") |>
      purrr::map(~ fs::path(pkg_path, .x)) |>
      purrr::keep(fs::dir_exists)

    r_files <- purrr::map(
      r_dirs,
      ~ fs::dir_ls(.x, regexp = "\\.R$", recurse = TRUE)
    ) |>
      unlist(use.names = FALSE) |>
      unique()

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

    stats <- purrr::map(r_files, function(file) {
      lines <- readLines(file, warn = FALSE)
      total <- length(lines)
      blank <- sum(stringr::str_detect(lines, "^\\s*$"))
      roxygen <- sum(stringr::str_detect(lines, "^\\s*#'"))
      comment <- sum(
        stringr::str_detect(lines, "^\\s*#") &
          !stringr::str_detect(lines, "^\\s*#'")
      )
      func_lines <- estimate_function_lines(lines)
      is_test <- stringr::str_detect(file, "/tests/")

      dplyr::tibble(
        total_lines = total,
        blank_lines = blank,
        comment_lines = comment,
        roxygen_lines = roxygen,
        function_lines = func_lines,
        test_lines = ifelse(is_test, total, 0)
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

#' Estimate function block line count from a character vector
#'
#' Rough approximation of the number of lines across all function bodies.
#'
#' @param lines A character vector of code lines
#' @return Integer, total estimated function body lines
#' @keywords internal
#' @noRd
estimate_function_lines <- function(lines) {
  func_start <- stringr::str_which(lines, "<-\\s*function\\s*\\(")
  if (length(func_start) == 0) {
    return(0)
  }

  total_lines <- 0
  open_braces <- 0
  inside_function <- FALSE

  for (i in seq_along(lines)) {
    line <- lines[i]
    if (i %in% func_start) {
      inside_function <- TRUE
      open_braces <- open_braces +
        stringr::str_count(line, "\\{") -
        stringr::str_count(line, "\\}")
      total_lines <- total_lines + 1
      next
    }

    if (inside_function) {
      open_braces <- open_braces +
        stringr::str_count(line, "\\{") -
        stringr::str_count(line, "\\}")
      total_lines <- total_lines + 1
      if (open_braces <= 0) {
        inside_function <- FALSE
      }
    }
  }

  total_lines
}
