#' Cache directory for a cache type
#'
#' The locations can be redirected with the internal options
#' `pkginspectr.session_cache` and `pkginspectr.persistent_cache`, which the
#' tests use so they never touch a user's real cache.
#'
#' @param cache `"session"`, `"persistent"` or `"none"`.
#' @return A path, or `NULL` for `"none"`.
#' @keywords internal
#' @noRd
get_cache_dir <- function(cache = c("session", "persistent", "none")) {
  cache <- match.arg(cache)
  switch(
    cache,
    session = getOption(
      "pkginspectr.session_cache",
      fs::path(tempdir(), "pkginspectr")
    ),
    persistent = getOption(
      "pkginspectr.persistent_cache",
      rappdirs::user_cache_dir("pkginspectr")
    ),
    none = NULL
  )
}


#' Clear the cache of downloaded packages
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
#' # Clear the cache of the current R session
#' pkg_cache_clear("session")
#'
#' @examplesIf interactive()
#' # Clear packages cached across R sessions
#' pkg_cache_clear("persistent")
pkg_cache_clear <- function(type = c("session", "persistent")) {
  type <- match.arg(type)
  dir <- get_cache_dir(type)
  if (fs::dir_exists(dir)) {
    fs::dir_delete(dir)
    cli::cli_alert_success("Cleared {.val {type}} cache at {.path {dir}}")
    return(invisible(dir))
  }
  cli::cli_alert_info("No cache found at {.path {dir}}")
  invisible(NULL)
}


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
