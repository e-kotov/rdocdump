# Get Current `rdocdump` Repository Options

This function returns the current repository URLs used by `rdocdump`.
The default is set to the CRAN repository at
"https://cloud.r-project.org". This does not affect the repositories
used by
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html) in
your current R session and/or project. To set repository options, use
[`rdd_set_repos`](https://www.ekotov.pro/rdocdump/reference/rdd_set_repos.md).

## Usage

``` r
rdd_get_repos()
```

## Value

A character vector of repository URLs.

## Examples

``` r
# Get current rdocdump repository options
rdd_get_repos()
#>                          CRAN 
#> "https://cloud.r-project.org" 
```
