# Path to the example package bundled with pkginspectr

pkginspectr ships a tiny package, `examplepkg`, with known contents. It
lets examples run offline and makes results easy to check by hand: its R
files use `<-`, `=` and `\(x)` function definitions, a lower-case `.r`
test file, and strings containing `#` and unbalanced braces.

## Usage

``` r
pkginspectr_example()
```

## Value

Path to the root directory of the example package.

## Examples

``` r
pkginspectr_example()
#> [1] "/home/runner/work/_temp/Library/pkginspectr/extdata/examplepkg"

pkg_structure(pkginspectr_example())
#> Inspecting examplepkg...
#> # A tibble: 1 × 11
#>   package    version num_files num_dirs num_r_files num_function_r_files
#>   <chr>      <chr>       <int>    <int>       <int>                <int>
#> 1 examplepkg 1.2.3           9        6           2                    1
#> # ℹ 5 more variables: num_non_function_r_files <int>, num_test_files <int>,
#> #   num_vignettes <int>, num_docs <int>, num_data_files <int>
```
