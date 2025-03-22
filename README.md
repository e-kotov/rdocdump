

<!-- README.md is generated from README.Rmd. Please edit that file -->

# rdocdump

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/rdocdump.png)](https://CRAN.R-project.org/package=rdocdump)
<!-- badges: end -->

The goal of rdocdump is to …

## Installation

You can install the development version of rdocdump from GitHub with:

``` r
# install.packages("pak")
pak::pak("e-kotov/rdocdump")
```

## Example

Extract documenation of `{rJavaEnv}` package byt downloading source from
CRAN and save it to file `rJavaEnv_docs.txt`

``` r
rdd_to_txt(
  pkg = "rJavaEnv",
  file = "rJavaEnv_docs.txt",
  force_fetch = TRUE, # force download even if package is installed
  keep_files = "none" # delete temp files
)
```
