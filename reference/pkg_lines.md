# Count lines of R code in packages

Analyse the R source files (`.R` and `.r`) in `R/`, `tests/` and
`data-raw/` of one or more packages and count their lines, broken down
two ways: by *type* (code, comment, roxygen, blank) and by *location*
(`R/`, `tests/`, elsewhere).

## Usage

``` r
pkg_lines(
  path = ".",
  include_ignored = FALSE,
  quiet = FALSE,
  cache = c("session", "persistent", "none")
)
```

## Arguments

- path:

  A character vector of packages. Each element is one of:

  - A local folder, such as `"."` or `"~/code/mypkg"`. Use
    `"local::path"` to be explicit.

  - A GitHub repository, such as `"r-lib/cli"` or a `github.com` URL.
    Add `@ref` for a branch, tag or commit (`"r-lib/cli@v3.6.0"`). For a
    package in a subfolder, use the `github::` prefix:
    `"github::owner/repo/subdir"`.

  - A CRAN package, such as `"desc"`. Add `@version` for an older
    release (`"cran::desc@1.4.0"`). Archived packages are supported.

  Without a prefix, existing local folders take precedence, then
  `"owner/repo"` is treated as GitHub and a bare name as CRAN. A path
  that does not exist (e.g. `"./mypkg"`) is an error, not a GitHub
  request.

- include_ignored:

  If `TRUE`, also analyse files matched by the package's
  `.Rbuildignore`. Version-control folders and R's built-in build
  exclusions are always skipped.

- quiet:

  If `TRUE`, suppress progress messages.

- cache:

  Where to keep downloaded GitHub and CRAN packages:

  - `"session"` (default): a temporary folder, removed when R closes.

  - `"persistent"`: a user cache folder kept across R sessions. Only
    fixed versions are stored there: CRAN releases and GitHub refs that
    look like a commit SHA or a version tag (`"owner/repo@v1.2.0"`).
    Requests that can change over time (no ref, or a branch) always
    fetch the latest code and use the session cache instead.

  - `"none"`: always download afresh.

  Use
  [`pkg_cache_clear()`](https://ddauber.github.io/pkginspectr/reference/pkg_cache_clear.md)
  to empty a cache.

## Value

A `pkginspectr_tbl` with one row per package and columns:

- `package`, `version`

- `total_lines`: all lines in the analysed files.

By type (these four add up to `total_lines`):

- `code_lines`: lines containing code, including code followed by a
  comment and lines inside multi-line strings.

- `comment_lines`: lines containing only a regular (`#`) comment.

- `roxygen_lines`: lines containing only a roxygen (`#'`) comment.

- `blank_lines`: empty or whitespace-only lines.

By location (these three add up to `total_lines`):

- `r_lines`: lines in `R/`.

- `test_lines`: lines in `tests/`.

- `other_lines`: lines elsewhere (e.g. `data-raw/`).

And:

- `function_lines`: code lines inside function definitions (a subset of
  `code_lines`). Nested functions are counted once.

## Details

Lines are classified with R's own parser
([`utils::getParseData()`](https://rdrr.io/r/utils/getParseData.html)),
so strings that contain `#` or unbalanced braces are counted correctly,
and functions are detected however they are written (`<-`, `=`, `\(x)`
lambdas, or anonymous functions inside calls). Files that fail to parse
fall back to a line-based heuristic and their function lines are not
counted; a warning names each such file.

Only files that `R CMD build` would ship are analysed, so results for a
local folder are comparable with results for a CRAN tarball. In
particular, anything listed in `.Rbuildignore` (often `data-raw/`) is
skipped unless `include_ignored = TRUE`.

## See also

[`pkg_structure()`](https://ddauber.github.io/pkginspectr/reference/pkg_structure.md)
for file counts,
[`pkg_summary()`](https://ddauber.github.io/pkginspectr/reference/pkg_summary.md)
for both, and
[`pkg_plot()`](https://ddauber.github.io/pkginspectr/reference/pkg_plot.md)
to visualise the result.

## Examples

``` r
pkg_lines(pkginspectr_example())
#> Counting lines in examplepkg...
#> # A tibble: 1 × 11
#>   package version total_lines code_lines comment_lines roxygen_lines blank_lines
#>   <chr>   <chr>         <int>      <int>         <int>         <int>       <int>
#> 1 exampl… 1.2.3            32         21             2             4           5
#> # ℹ 4 more variables: r_lines <int>, test_lines <int>, other_lines <int>,
#> #   function_lines <int>

if (FALSE) { # interactive()
# GitHub repositories and CRAN packages are downloaded and cached
pkg_lines(c("r-lib/cli", "desc"))

# Specific versions, kept across sessions
pkg_lines(c("r-lib/cli@v3.6.0", "cran::desc@1.4.0"), cache = "persistent")
}
```
