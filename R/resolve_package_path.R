#' Resolve package paths from local, GitHub or CRAN sources
#'
#' Accept one or more local file paths, GitHub repository names or CRAN package
#' names and return the corresponding local paths. GitHub and CRAN packages are
#' downloaded and extracted into a cache directory when required.
#'
#' @param path Character vector of paths, GitHub repos (e.g. "user/repo") or
#'   CRAN package names.
#' @param cache Cache mode: "session" (tempdir), "persistent" (user cache) or
#'   "none".
#'
#' @return A character vector of absolute paths to package directories.
#' @noRd
#' @keywords internal
resolve_package_path <- function(
  path,
  cache = c("session", "persistent", "none")
) {
  cache <- match.arg(cache)
  cache_dir <- get_cache_dir(cache)
  paths <- purrr::map_chr(path, function(p) {
    # Local folder
    if (fs::dir_exists(p)) {
      return(fs::path_abs(p))
    }

    # GitHub shorthand or full URL
    if (is_github_spec(p) || is_github_url(p)) {
      # nocov start
      cli::cli_inform("Detected GitHub repo: {.val {p}}")
      return(download_and_extract_github_repo(p, cache_dir))
    } # nocov end

    # CRAN package
    if (is_cran_package(p)) {
      # nocov start
      cli::cli_inform("Detected CRAN package: {.val {p}}")
      return(download_and_extract_cran_package(p, cache_dir))
    } # nocov end

    cli::cli_abort(
      "Could not resolve path {.val {p}} - must be a local folder, GitHub repo, or CRAN package name."
    )
  })

  fs::path_abs(paths)
}

#' Download and extract GitHub repo from main branch
#'
#' @param repo_url Full URL to GitHub repo (e.g., https://github.com/user/repo)
#' @return Path to extracted folder
#' @noRd
# nocov start
download_and_extract_github_repo <- function(spec, cache_dir = NULL) {
  owner_repo <- if (grepl("^https://", spec)) {
    sub("^https://github.com/", "", spec)
  } else {
    spec
  }

  repo_url <- paste0("https://github.com/", owner_repo)
  branch <- get_default_branch(repo_url)
  archive_url <- paste0(repo_url, "/archive/refs/heads/", branch, ".tar.gz")

  repo_name <- basename(owner_repo)
  tag <- paste0(repo_name, "_", branch)
  cache_path <- if (!is.null(cache_dir)) fs::path(cache_dir, tag) else NULL

  if (!is.null(cache_path) && fs::dir_exists(cache_path)) {
    cli::cli_inform("Using cached GitHub repo: {.val {tag}}")
    cached_dir <- fs::dir_ls(cache_path, type = "directory")[1]
    if (!is.na(cached_dir) && fs::dir_exists(cached_dir)) {
      return(cached_dir)
    }
    return(cache_path)
  }

  tmp_file <- fs::file_temp(ext = "tar.gz")
  utils::download.file(archive_url, destfile = tmp_file, quiet = TRUE)

  outdir <- if (!is.null(cache_path)) {
    cache_path
  } else {
    fs::path_temp("pkginspectr", tag)
  }
  utils::untar(tmp_file, exdir = outdir)

  pkg_dir <- fs::dir_ls(outdir, type = "directory")[1]
  if (is.null(pkg_dir) || !fs::dir_exists(pkg_dir)) {
    cli::cli_abort("Failed to extract GitHub repo from {.val {archive_url}}")
  }

  pkg_dir
}

#' Download and extract a CRAN package
#'
#' @param pkg Package name (character string)
#' @return Path to extracted folder
#' @noRd
download_and_extract_cran_package <- function(pkg, cache_dir = NULL) {
  tarball_name <- utils::available.packages()[pkg, "Package"]
  version <- utils::available.packages()[pkg, "Version"]
  tag <- paste0(pkg, "_", version)

  cache_path <- if (!is.null(cache_dir)) fs::path(cache_dir, tag) else NULL
  if (!is.null(cache_path) && fs::dir_exists(cache_path)) {
    cli::cli_inform("Using cached CRAN package: {.val {tag}}")
    cached_dir <- fs::dir_ls(cache_path, type = "directory")[1]
    if (!is.na(cached_dir) && fs::dir_exists(cached_dir)) {
      return(cached_dir)
    }
    return(cache_path)
  }

  tarball_url <- paste0(
    "https://cran.r-project.org/src/contrib/",
    tag,
    ".tar.gz"
  )
  dest_file <- fs::file_temp(ext = "tar.gz")
  utils::download.file(tarball_url, destfile = dest_file, quiet = TRUE)

  outdir <- if (!is.null(cache_path)) {
    cache_path
  } else {
    fs::path_temp("pkginspectr", tag)
  }
  utils::untar(dest_file, exdir = outdir)

  pkg_dir <- fs::dir_ls(outdir, type = "directory")[1]
  if (is.null(pkg_dir) || !fs::dir_exists(pkg_dir)) {
    cli::cli_abort("Failed to extract CRAN package from {.val {tarball_url}}")
  }

  pkg_dir
}
# nocov end
