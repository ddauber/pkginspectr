#' Check if a string is a GitHub repo shorthand
#'
#' Determines whether the input string follows the GitHub shorthand format
#' `"user/repo"` (e.g., `"tidyverse/ggplot2"`), and is not a local directory.
#'
#' @param x A character string.
#'
#' @return `TRUE` if the input looks like a GitHub shorthand and does not correspond
#'   to a local folder, `FALSE` otherwise.
#'
#' @examples
#' is_github_spec("tidyverse/ggplot2")
#' is_github_spec("https://github.com/tidyverse/ggplot2")  # FALSE
#' is_github_spec("local/folder/path")                     # FALSE
#'
#' @keywords internal
#' @noRd
is_github_spec <- function(x) {
  grepl("^[^/]+/[^/]+$", x) && !fs::dir_exists(x)
}


#' Check if a string is a full GitHub URL
#'
#' Determines whether the input is a valid full GitHub repository URL,
#' such as `"https://github.com/user/repo"`.
#'
#' @param x A character string.
#'
#' @return `TRUE` if the string matches the GitHub URL pattern, `FALSE` otherwise.
#'
#' @examples
#' is_github_url("https://github.com/tidyverse/ggplot2")
#' is_github_url("tidyverse/ggplot2")  # FALSE
#'
#' @keywords internal
#' @noRd
is_github_url <- function(x) {
  grepl("^https://github.com/.+/.+", x)
}


#' Check if a package exists on CRAN
#'
#' This utility function checks whether a given package name refers to a valid
#' CRAN package by attempting to access its package index page.
#'
#' @param pkg A character string containing the name of a CRAN package (e.g., `"ggplot2"`).
#'
#' @return A logical value: `TRUE` if the package exists on CRAN, `FALSE` otherwise.
#'
#' @keywords internal
#' @noRd
# nocov start
is_cran_package <- function(pkg) {
  url <- glue::glue("https://cran.r-project.org/web/packages/{pkg}/index.html")
  res <- try(httr::http_error(url), silent = TRUE)
  if (inherits(res, "try-error")) {
    return(FALSE)
  }
  !res
}
# nocov end

#' Resolve the appropriate cache directory
#'
#' @param cache Cache mode: "session", "persistent", or "none".
#' @return A string path or NULL if caching is disabled.
#' @keywords internal
#' @noRd
get_cache_dir <- function(cache = c("session", "persistent", "none")) {
  cache <- match.arg(cache)
  switch(
    cache,
    session = fs::path(tempdir(), "pkginspectr"),
    persistent = rappdirs::user_cache_dir("pkginspectr"),
    none = NULL
  )
}


#' Clear downloaded package cache
#'
#' Deletes the cache directory used by `pkginspectr` to store downloaded
#' CRAN and GitHub packages. This is useful for freeing up disk space or
#' forcing fresh downloads in future analyses.
#'
#' You can choose to clear either the session-level cache (temporary files
#' created during the current R session) or the persistent cache (files
#' stored across sessions in a user-specific cache directory).
#'
#' @param type A string indicating which cache to clear. Must be one of:
#'   - `"session"`: temporary cache stored in `tempdir()` (default)
#'   - `"persistent"`: long-term cache stored in a platform-specific user
#'     cache directory via `rappdirs::user_cache_dir("pkginspectr")`
#'
#' @return Used for side effects. Invisibly returns the path that was cleared,
#'   or `NULL` if no cache was found.
#'
#' @export
#'
#' @examples
#' # Clear session cache
#' clear_pkg_cache("session")
#'
#' # Clear persistent cache
#' clear_pkg_cache("persistent")
clear_pkg_cache <- function(type = c("session", "persistent")) {
  dir <- get_cache_dir(type)
  if (fs::dir_exists(dir)) {
    fs::dir_delete(dir)
    cli::cli_alert_success("Cleared {.val {type}} cache at {.path {dir}}")
  } else {
    cli::cli_alert_info("No cache found at {.path {dir}}")
  }
}


#' Get default branch of a GitHub repository
#'
#' Queries the GitHub API to retrieve the default branch of a repository.
#'
#' @param repo A GitHub shorthand like "user/repo" or full URL like "https://github.com/user/repo".
#' @return A character string with the name of the default branch.
#' @noRd
#' @keywords internal
# nocov start
get_default_branch <- function(repo) {
  # Convert full URL to "user/repo"
  if (grepl("^https://github.com/", repo)) {
    repo <- sub("^https://github.com/", "", repo)
  }

  api_url <- glue::glue("https://api.github.com/repos/{repo}")

  resp <- httr::GET(api_url)
  if (httr::http_error(resp)) {
    cli::cli_abort("Failed to query GitHub API for {.val {repo}}.")
  }

  content <- httr::content(resp)
  if (!"default_branch" %in% names(content)) {
    cli::cli_abort("Could not detect default branch for {.val {repo}}.")
  }

  cli::cli_inform("Detected default branch: {.val {content$default_branch}}")

  content$default_branch
}
# nocov end

#' Retrieve package name and version from DESCRIPTION
#'
#' Extracts the `Package` and `Version` fields from a package's
#' DESCRIPTION file. Falls back to the folder name and `NA` version if the
#' file is missing or cannot be read.
#'
#' @param pkg_path Path to the package directory.
#' @return A list with elements `name` and `version`.
#' @noRd
#' @keywords internal
get_pkg_name_version <- function(pkg_path) {
  desc_path <- fs::path(pkg_path, "DESCRIPTION")
  if (fs::file_exists(desc_path)) {
    info <- tryCatch(
      desc::desc_get(c("Package", "Version"), file = desc_path),
      error = function(e) {
        cli::cli_warn(
          "Failed to read DESCRIPTION for {.path {pkg_path}}. Using folder name."
        )
        c(Package = fs::path_file(pkg_path), Version = NA_character_)
      }
    )
    list(name = info[["Package"]], version = info[["Version"]])
  } else {
    cli::cli_warn(
      "No DESCRIPTION file found in {.path {pkg_path}}. Using folder name."
    )
    list(name = fs::path_file(pkg_path), version = NA_character_)
  }
}
