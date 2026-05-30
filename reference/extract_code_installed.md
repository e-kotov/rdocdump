# Extract code from an installed package using its namespace.

This function retrieves all functions from the package namespace and
deparses them to get their source code. Note that extracting from an
installed package silently skips S4 classes, R6 classes, environment
objects, and datasets since it filters for
[`is.function()`](https://rdrr.io/r/base/is.function.html). For more
complete code extraction, prefer extracting from source packages.

## Usage

``` r
extract_code_installed(pkg_name)
```

## Arguments

- pkg_name:

  The name of the installed package.

## Value

A single string containing the source code of all functions in the
package.
