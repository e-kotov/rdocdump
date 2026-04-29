# Extract code from an installed package using its namespace.

This function retrieves all functions from the package namespace and
deparses them to get their source code.

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
