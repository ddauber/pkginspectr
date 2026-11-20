#' List the CRAN releases of a package
#'
#' Return every version of a package published on CRAN, with its release
#' date, sorted by version number (oldest first). Use it to choose versions for [pkg_history()] or for
#' `"cran::pkg@version"` specifications.
#'
#' Dates are those recorded by CRAN: the file date in CRAN's archive for
#' older releases, and the `Date/Publication` field for the current one.
#' A date is `NA` if CRAN does not report it.
#'
#' @param package Name of a CRAN package, such as `"dplyr"`. A `"cran::"`
#'   prefix is accepted.
#'
#' @return A tibble with one row per release and columns `package`,
#'   `version`, `date` (a `Date`) and `current` (`TRUE` for the release
#'   currently on CRAN). Archived packages have no current release.
#' @seealso [pkg_history()] to analyse several releases.
#' @export
#'
#' @examplesIf interactive()
#' pkg_versions("desc")
pkg_versions <- function(package) {
  name <- check_cran_name(package)
  repos <- cran_repos()

  db <- cran_available(repos)
  current <- if (name %in% rownames(db)) db[name, "Version"] else NA_character_
  releases <- cran_archive_listing(name, repos)

  if (is.na(current) && nrow(releases) == 0) {
    cli::cli_abort("Package {.pkg {name}} not found on CRAN.")
  }
  if (!is.na(current) && !current %in% releases$version) {
    releases <- rbind(
      releases,
      dplyr::tibble(version = current, date = cran_publication_date(name, repos))
    )
  }

  releases <- releases[order(package_version(releases$version)), ]
  dplyr::tibble(
    package = rep(name, nrow(releases)),
    version = releases$version,
    date = releases$date,
    current = releases$version %in% current
  )
}

#' Follow a package across its CRAN releases
#'
#' Run [pkg_summary()] on several CRAN releases of one package to see how it
#' grew over time. By default `n` releases are picked evenly across the
#' package's history, always including the first and the latest; or name the
#' versions you want.
#'
#' Each release is downloaded from CRAN. Old releases never change, so
#' `cache = "persistent"` is a good choice if you will revisit them.
#'
#' @param package Name of a CRAN package, such as `"dplyr"`.
#' @param versions Optional character vector of versions to analyse, e.g.
#'   `c("0.8.0", "1.0.0")`. See [pkg_versions()] for what is available. If
#'   given, `n` is ignored.
#' @param n Number of releases to pick when `versions` is not given. If the
#'   package has `n` releases or fewer, all are used.
#' @inheritParams pkg_lines
#'
#' @return A `pkginspectr_tbl` like [pkg_summary()], with one row per release,
#'   sorted by version number (oldest first), and an extra `date` column after `version`. Plot it with
#'   `pkg_plot(type = "metric")` to compare releases side by side.
#' @seealso [pkg_versions()] to list the releases.
#' @export
#'
#' @examplesIf interactive()
#' # Five releases spread across desc's history
#' history <- pkg_history("desc")
#' history
#' pkg_plot(history, type = "metric")
#'
#' # Specific releases, kept across R sessions
#' pkg_history("desc", versions = c("1.2.0", "1.4.0"), cache = "persistent")
pkg_history <- function(
  package,
  versions = NULL,
  n = 5,
  include_ignored = FALSE,
  quiet = FALSE,
  cache = c("session", "persistent", "none")
) {
  cache <- match.arg(cache)
  releases <- pkg_versions(package)
  name <- releases$package[[1]]

  if (is.null(versions)) {
    if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1 || n != round(n)) {
      cli::cli_abort("{.arg n} must be a single whole number of at least 1.")
    }
    chosen <- releases[pick_spread(nrow(releases), n), ]
  } else {
    versions <- as.character(versions)
    missing <- setdiff(versions, releases$version)
    if (length(missing) > 0) {
      recent <- utils::tail(releases$version, 5)
      cli::cli_abort(c(
        "{cli::qty(missing)}Version{?s} {.val {missing}} of {.pkg {name}} {?is/are} not on CRAN.",
        "i" = "Most recent releases: {.val {recent}}. See {.fn pkg_versions}."
      ))
    }
    chosen <- releases[releases$version %in% versions, ]
  }

  specs <- paste0("cran::", name, "@", chosen$version)
  history <- pkg_summary(
    specs,
    include_ignored = include_ignored,
    quiet = quiet,
    cache = cache
  )
  # Failed downloads come back as NA rows; label them with the release.
  history$package <- name
  history$version <- chosen$version
  history$date <- chosen$date
  new_pkginspectr_tbl(dplyr::relocate(history, "date", .after = "version"))
}

#' Indices of `n` items spread evenly over `1:total`, always including the
#' last one (and the first when `n > 1`)
#' @noRd
pick_spread <- function(total, n) {
  if (n >= total) {
    return(seq_len(total))
  }
  if (n == 1) {
    return(total)
  }
  unique(round(seq(1, total, length.out = n)))
}

#' Validate a single CRAN package name, dropping an optional `cran::` prefix
#' @noRd
check_cran_name <- function(package, call = rlang::caller_env()) {
  if (!is.character(package) || length(package) != 1 || is.na(package)) {
    cli::cli_abort("{.arg package} must be a single CRAN package name.", call = call)
  }
  name <- sub("^cran::", "", package)
  if (grepl("@", name, fixed = TRUE)) {
    cli::cli_abort(
      c(
        "{.arg package} must be a package name without a version.",
        "i" = "Use the {.arg versions} argument of {.fn pkg_history} to choose versions."
      ),
      call = call
    )
  }
  parse_cran_spec(name, package)$name
}
