# Set `rdocdump` Repository Options

This function sets the package repository URLs used by `rdocdump` when
fetching package sources. May be useful for setting custom repositories
or mirrors. This does not affect the repositories used by
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html) in
your current R session and/or project.

## Usage

``` r
rdd_set_repos(repos)
```

## Arguments

- repos:

  A character vector of repository URLs.

## Value

Invisibly returns the new repository URLs.

## Examples

``` r
# Set rdocdump repository options
rdd_set_repos(c("CRAN" = "https://cloud.r-project.org"))
#> rdocdump.repos set to: https://cloud.r-project.org
```
