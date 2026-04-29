# Helper function to extract code from package source files.

This function reads all `.R` files in the `R` directory and optionally
includes files from the `tests` directory. It can also exclude roxygen2
documentation lines.

## Usage

``` r
extract_code_source(pkg_path, include_tests = FALSE, include_roxygen = FALSE)
```

## Arguments

- pkg_path:

  Path to the package source directory.

- include_tests:

  `logical`. If `TRUE`, for non-installed packages, the function will
  also include R source code from the `tests` directory. Defaults to
  `FALSE`.

- include_roxygen:

  `logical`. If `TRUE`, roxygen2 documentation lines (lines starting
  with "#'") from R files will be included in the output. Defaults to
  `FALSE`.

## Value

A single string containing the source code from the package's R files.
