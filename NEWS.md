# pkginspectr 0.1.0

-   `pkginspectr` returns statistical insights into R packages:

-   Added `inspect_package()` which returns key statistics about an R package. In particular:

    -   `package`: package name\
    -   `version`: version number
    -   `num_files`: number of files in the package
    -   `num_dirs`: number of directories in the package
    -   `num_r_files`: number of R files in the package
    -   `num_function_r_files`: number of R files containing functions
    -   `num_non_function_r_files`: number of R files not containing functions
    -   `num_test_files`: number of test files in the package
    -   `num_vignettes`: number of vignettes in the package
    -   `num_docs`: number of documentation files in the package
    -   `num_data_files`: number of data files in the package

-   Added `get_code_stats()` which returns code statistics for an R package. In particular:

    -   `package`: package name
    -   `version`: version number
    -   `total_lines`: total number of lines of code
    -   `blank_lines`: total number of blank lines
    -   `comment_lines`: total number of comment lines
    -   `roxygen_lines`: total number of roxygen lines
    -   `function_lines`: total number of lines within functions
    -   `test_lines`: total number of lines within test files

-   Added `plot_pkg()` which plots output from `inspect_package()` or `get_code_stats()` in the console. Users can modify colours and shapes of the plot to make it uniquely theirs. It offers two modes `type = "package"` and `type = "metric"`:

    -   `type = "package"` plots package statistics for multiple packages grouped by package name.

    -   `type = "metric"` plots package statistics grouped by metric.

-   `pkginspectr` retrieves package information from

    -   local packages
    -   CRAN, and
    -   GitHub repositories.

-   Updated `README.md` with examples of how to use the package.

- Added tests for all functions using `testthat`.

- Create informative `README.md`.

# pkginspectr 0.0.0.9000

-   Initial commit.