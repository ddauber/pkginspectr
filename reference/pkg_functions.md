# List the functions defined in packages

Return one row per named function defined in the R source files of one
or more packages, with its size, number of arguments and whether it is
exported. Use it to find the largest functions, or to see how much of a
package is internal.

## Usage

``` r
pkg_functions(
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

  With several packages, one that cannot be analysed (a typo, a failed
  download) does not stop the others: it is reported in a warning and
  returned as a row of `NA`s whose `package` is the specification as
  given (`pkg_functions()` leaves it out instead). A single package that
  fails, or all packages failing, is an error.

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

A tibble of class `pkginspectr_functions` with one row per function and
columns:

- `package`, `version`

- `file`: path relative to the package root, e.g. `"R/utils.R"`.

- `location`: `"R"`, `"tests"` or `"other"` (e.g. `data-raw/`).

- `name`: the function's name.

- `line`: the line where the definition starts.

- `code_lines`: code lines in the definition, excluding comments and
  blank lines (as in
  [`pkg_lines()`](https://ddauber.github.io/pkginspectr/reference/pkg_lines.md)).

- `n_args`: number of arguments, counting `...` as one.

- `exported`: whether the package's `NAMESPACE` exports the function,
  via `export()` or `exportPattern()`. `NA` for functions outside `R/`
  or when the package has no readable `NAMESPACE`. S3 methods registered
  with `S3method()` are not exported in this sense.

Rows are ordered by package, file and line. A package without functions
contributes no rows.

## Details

A function is listed when it is assigned to a name at the top level of a
file, with `<-`, `=` or `<<-`, written as `function(...)` or `\(...)`.
Functions defined inside other functions, inside lists or R6 classes, or
with `setMethod()` are not listed. Files that fail to parse are skipped
with a warning.

The same files are analysed as in
[`pkg_lines()`](https://ddauber.github.io/pkginspectr/reference/pkg_lines.md):
R source files in `R/`, `tests/` and `data-raw/` that `R CMD build`
would ship.

## See also

[`pkg_lines()`](https://ddauber.github.io/pkginspectr/reference/pkg_lines.md)
for package-level line counts.

## Examples

``` r
fns <- pkg_functions(pkginspectr_example())
#> Listing functions in examplepkg...
fns
#> # pkginspectr: Functions, 3 functions in 1 package
#> # A tibble:    3 × 9
#>   package    version file     location name     line code_lines n_args exported
#>   <chr>      <chr>   <chr>    <chr>    <chr>   <int>      <int>  <int> <lgl>   
#> 1 examplepkg 1.2.3   R/funs.R R        add_one     5          3      1 TRUE    
#> 2 examplepkg 1.2.3   R/funs.R R        square     10          1      1 FALSE   
#> 3 examplepkg 1.2.3   R/funs.R R        compose    12          3      2 FALSE   

# The largest functions
fns[order(fns$code_lines, decreasing = TRUE), c("name", "code_lines")]
#> Warning: Unknown or uninitialised column: `package`.
#> # pkginspectr: Functions, 3 functions in 0 packages
#> # A tibble:    3 × 2
#>   name    code_lines
#>   <chr>        <int>
#> 1 add_one          3
#> 2 compose          3
#> 3 square           1

# Exported vs internal functions
table(fns$exported)
#> 
#> FALSE  TRUE 
#>     2     1 

if (FALSE) { # interactive()
# The ten longest functions in cli
pkg_functions("r-lib/cli", quiet = TRUE) |>
  dplyr::slice_max(code_lines, n = 10)
}
```
