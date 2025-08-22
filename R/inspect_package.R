#' Inspect packages for structure and file types
#'
#' Examine the file structure of one or more local packages and return summary
#' statistics including file counts, whether files contain functions and the
#' package version.
#'
#' @param path A character vector or list of local package paths.
#'
#' @return A `pkginspectr_tbl` with one row per package containing structural
#'   summaries.
#' @export
#'
#' @examples
#' \dontrun{
#' # Inspect the installed "stats" package
#' inspect_package(system.file(package = "stats"))
#' }
inspect_package <- function(path = ".") {
  paths <- resolve_package_path(path)

  result <- purrr::map_dfr(paths, function(pkg_path) {
    if (!fs::dir_exists(pkg_path)) {
      cli::cli_abort("The path {.path {pkg_path}} does not exist.")
    }

    all_files <- tryCatch(
      fs::dir_ls(pkg_path, recurse = TRUE, type = "file"),
      error = function(e) {
        cli::cli_abort("Failed to list files in {.path {pkg_path}}.")
      }
    )

    pkg_info <- get_pkg_name_version(pkg_path)
    pkg_name <- pkg_info$name
    pkg_version <- pkg_info$version

    cli::cli_inform("Inspecting {.pkg {pkg_name}}...")

    num_dirs <- fs::dir_ls(pkg_path, recurse = TRUE, type = "directory") |>
      length()

    r_files <- all_files[stringr::str_detect(all_files, "\\.R$")]
    test_files <- all_files[stringr::str_detect(
      all_files,
      "^.*/tests(/|/testthat/).*\\.R$"
    )]
    vignette_files <- all_files[stringr::str_detect(
      all_files,
      "^.*/vignettes/.*\\.(Rmd|rmd|Qmd|qmd)$"
    )]
    doc_files <- all_files[stringr::str_detect(all_files, "^.*/man/.*\\.Rd$")]
    data_files <- all_files[stringr::str_detect(
      all_files,
      "^.*/data/.*\\.(rda|RData)$"
    )]

    is_function_file <- function(file) {
      lines <- readLines(file, warn = FALSE)
      any(stringr::str_detect(lines, "<-\\s*function\\s*\\("))
    }

    has_function <- purrr::map_lgl(r_files, is_function_file)

    dplyr::tibble(
      package = pkg_name,
      version = pkg_version,
      num_files = length(all_files),
      num_dirs = num_dirs,
      num_r_files = length(r_files),
      num_function_r_files = sum(has_function),
      num_non_function_r_files = sum(!has_function),
      num_test_files = length(test_files),
      num_vignettes = length(vignette_files),
      num_docs = length(doc_files),
      num_data_files = length(data_files)
    )
  })

  class(result) <- c("pkginspectr_tbl", class(result))
  result
}
