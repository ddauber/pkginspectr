#' Apply a per-package function to one or more packages
#'
#' Shared driver for [pkg_structure()], [pkg_lines()], [pkg_summary()] and
#' [pkg_functions()]: resolves each entry of `path` (downloading GitHub or
#' CRAN packages if needed), reads its name and version once, applies `fun`
#' and row-binds the results.
#'
#' With several packages, one that fails (a typo, a failed download, an
#' unreadable folder) does not abort the others: it is reported in a single
#' warning and, if `failed_rows = TRUE`, kept as a row of `NA`s whose
#' `package` is the specification as given. A single package, or all packages
#' failing, is an error.
#'
#' @param path Character vector of local paths, GitHub repos or CRAN names.
#' @param fun Function of `(pkg_path, info)` returning a tibble, where `info`
#'   is the list returned by `get_pkg_name_version()`.
#' @param verb Progress message verb, e.g. `"Inspecting"`.
#' @param quiet If `TRUE`, suppress progress messages (not warnings).
#' @param cache Cache mode passed to `resolve_one()`.
#' @param class Class added to the combined result.
#' @param failed_rows Keep an `NA` row for each failed package?
#' @return The combined tibble.
#' @keywords internal
#' @noRd
map_packages <- function(
  path,
  fun,
  verb,
  quiet = FALSE,
  cache = c("session", "persistent", "none"),
  class = "pkginspectr_tbl",
  failed_rows = TRUE
) {
  cache <- match.arg(cache)
  path <- check_path(path)

  analyse <- function(p) {
    pkg_path <- resolve_one(p, cache)
    if (!fs::dir_exists(pkg_path)) {
      cli::cli_abort("The path {.path {pkg_path}} does not exist.")
    }
    info <- get_pkg_name_version(pkg_path)
    cli::cli_inform("{verb} {.pkg {info$name}}...")
    fun(pkg_path, info)
  }

  with_quiet(quiet, {
    if (length(path) == 1) {
      results <- list(analyse(path))
      failures <- list()
    } else {
      outcomes <- lapply(path, function(p) {
        tryCatch(analyse(p), error = function(e) e)
      })
      failed <- vapply(outcomes, inherits, logical(1), what = "error")
      failures <- outcomes[failed]
      names(failures) <- path[failed]
      if (all(failed)) {
        abort_all_failed(failures)
      }
      results <- outcomes
      if (failed_rows) {
        template <- outcomes[[which(!failed)[1]]]
        results[failed] <- lapply(path[failed], na_row, template = template)
      } else {
        results[failed] <- list(NULL)
      }
    }

    result <- purrr::list_rbind(results)
    class(result) <- unique(c(class, class(result)))
    if (length(failures) > 0) {
      warn_failed(failures, failed_rows)
    }
    result
  })
}

#' A one-row tibble shaped like `template`, all `NA` except `package`
#' @noRd
na_row <- function(spec, template) {
  row <- dplyr::as_tibble(lapply(template[1, ], function(col) col[NA_integer_]))
  row$package <- spec
  row
}

#' One bullet per failure: the specification and the error's first line
#' @noRd
failure_bullets <- function(failures) {
  escape <- function(x) gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)
  reasons <- vapply(
    failures,
    function(e) strsplit(cli::ansi_strip(conditionMessage(e)), "\n", fixed = TRUE)[[1]][1],
    character(1)
  )
  specs <- vapply(names(failures), function(s) cli::format_inline("{.val {s}}"), character(1))
  bullets <- escape(paste0(specs, ": ", reasons))
  names(bullets) <- rep("x", length(bullets))
  bullets
}

warn_failed <- function(failures, failed_rows) {
  outcome <- if (failed_rows) "{?is/are} shown as {.code NA}" else "{?is/are} left out"
  cli::cli_warn(c(
    paste0(
      "{cli::qty(length(failures))}{length(failures)} package{?s} could not be analysed and ",
      outcome,
      ":"
    ),
    failure_bullets(failures)
  ))
}

abort_all_failed <- function(failures, call = rlang::caller_env()) {
  cli::cli_abort(
    c("None of the packages could be analysed:", failure_bullets(failures)),
    call = call
  )
}

#' Mark a tibble as a pkginspectr result
#' @noRd
new_pkginspectr_tbl <- function(x) {
  class(x) <- unique(c("pkginspectr_tbl", class(x)))
  x
}

#' Evaluate `expr`, suppressing messages when `quiet` is `TRUE`
#' @noRd
with_quiet <- function(quiet, expr) {
  if (isTRUE(quiet)) suppressMessages(expr) else expr
}
