# Combine Rd files into a single character vector. This function reads the Rd files from a package source directory or an installed package and combines them into a single string.

Combine Rd files into a single character vector. This function reads the
Rd files from a package source directory or an installed package and
combines them into a single string.

## Usage

``` r
combine_rd(pkg_path, is_installed = FALSE, pkg_name = NULL)
```

## Arguments

- pkg_path:

  Path to the package source directory or the installed package.

- is_installed:

  Logical indicating whether the package is installed (`TRUE`) or a
  source package (`FALSE`).

- pkg_name:

  Optional package name if the package is installed.

## Value

A single string containing the combined Rd documentation.
