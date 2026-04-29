# Dump Package Source, Documentation and Vignettes into Plain Text

This function produces a single text output for an R package by
processing its documentation (Rd files from the package source or the
documentation from already installed packages), vignettes, and/or R
source code.

## Usage

``` r
rdd_to_txt(
  pkg,
  file = NULL,
  content = "all",
  force_fetch = FALSE,
  version = NULL,
  keep_files = "none",
  cache_path = getOption("rdocdump.cache_path"),
  repos = getOption("rdocdump.repos", getOption("repos"))
)
```

## Arguments

- pkg:

  A `character` string specifying the package. This can be:

  - an installed package name (e.g., `"ggplot2"`),

  - a full path to a package source directory,

  - a full path to a package archive file (tar.gz),

  - a package name not installed (which will then be downloaded from
    CRAN),

  - a GitHub repository reference (e.g., `"tidyverse/ggplot2"` or
    `"github::tidyverse/ggplot2"`),

  - a GitLab repository reference (e.g., `"gitlab::user/repo"`),

  - a repository reference with specific branch/tag/commit (e.g.,
    `"user/repo@v1.0.0"` or `"user/repo@main"`),

  - a repository reference with subdirectory (e.g., `"user/repo/subdir"`
    for packages not at repo root),

  - a full GitHub or GitLab web URL (e.g.,
    `"https://github.com/apache/sedona-db/tree/main/r/sedonadb"` or
    `"https://github.com/ipeaGIT/r5r/tree/master/r-package"`).

- file:

  Optional. Save path for the output text file. If set, the function
  will return the path to the file instead of the combined text.
  Defaults to `NULL`.

- content:

  A character vector specifying which components to include in the
  output. Possible values are:

  - `"all"`: Include Rd documentation, vignettes, and R source code
    (default).

  - `"docs"`: Include only the Rd documentation.

  - `"vignettes"`: Include only the vignettes.

  - `"code"`: Include only the R source code. When extracting code for
    non-installed packages, the function will not include roxygen2
    documentation, as the documentation can be imported from the Rd
    files. If you want to extract the R source code with the roxygen2
    documentation, use
    [`rdd_extract_code`](https://www.ekotov.pro/rdocdump/reference/rdd_extract_code.md)
    and set `include_roxygen` to `TRUE`.

  You can specify multiple options (e.g., `c("docs", "code")` to include
  both documentation and source code).

- force_fetch:

  `logical`. If `TRUE`, the package source will be fetched from CRAN as
  a tar.gz archive even if the package is already installed locally.
  Default is `FALSE`, but when `version` is specified, it will be set to
  `TRUE`.

- version:

  Optional. A `character` string specifying the package version to fetch
  from CRAN. If not provided, the latest version will be used.

- keep_files:

  A `character` value controlling whether temporary files should be
  kept. Possible values are:

  - `"none"`: Delete both the tar.gz archive and the extracted files
    (default).

  - `"tgz"`: Keep only the tar.gz archive.

  - `"extracted"`: Keep only the extracted files.

  - `"both"`: Keep both the tar.gz archive and the extracted files.

- cache_path:

  A `character` string specifying the directory where kept temporary
  files will be stored. By default, it uses the value of
  `getOption("rdocdump.cache_path")` which sets the cache directory to
  the temporary directory of the current R session.

- repos:

  A `character` vector of repository URLs. By default, it uses the value
  of `getOption("rdocdump.repos")` which sets the repository URLs to the
  default R repositories and is itself set to
  `c("CRAN" = "https://cloud.r-project.org")` on package load to prevent
  accidental downloads of pre-built packages from Posit Package Manager
  and R Universe.

## Value

A single string containing the combined package documentation,
vignettes, and/or code as specified by the `content` argument. If the
`file` argument is set, returns the path to the file.

## Examples

``` r
# Extract documentation for built-in `stats` package (both docs and vignettes).
docs <- rdd_to_txt("splines")
#> Warning: Neither 'vignettes' nor 'doc' directory found in the package source.
cat(substr(docs, 1, 500))
#> DESCRIPTION:
#> Package: splines
#> Version: 4.6.0
#> Priority: base
#> Imports: graphics, stats
#> Title: Regression Spline Functions and Classes
#> Author: Douglas M. Bates <bates@stat.wisc.edu> and
#>  William N. Venables <Bill.Venables@csiro.au>
#> Maintainer: R Core Team <do-use-Contact-address@r-project.org>
#> Contact: R-help mailing list <r-help@r-project.org>
#> Description: Regression spline functions and classes.
#> License: Part of R 4.6.0
#> Suggests: Matrix, methods
#> NeedsCompilation: yes
#> Encoding: UTF-8
#> Built: R 4.6.

if (FALSE) { # \dontrun{
# Extract from GitHub repository
docs <- rdd_to_txt("r-lib/rlang")

# Extract specific version from GitHub
docs <- rdd_to_txt("r-lib/rlang@v1.1.0")

# Extract from GitLab
docs <- rdd_to_txt("gitlab::user/repo")
} # }

# \donttest{
# set cache directory for `rdocdump`
rdd_set_cache_path(paste0(tempdir(), "/rdocdump_cache"))
#> rdocdump.cache_path set to: /tmp/RtmpmhHgKL/rdocdump_cache

# Extract only documentation for rJavaEnv by downloading its source from CRAN
docs <- rdd_to_txt(
  "rJavaEnv",
  force_fetch = TRUE,
  content = "docs",
  repos = c("CRAN" = "https://cran.r-project.org")
)
#> Fetching package source from CRAN...
lines <- unlist(strsplit(docs, "\n"))
# Print the first 3 lines
cat(head(lines, 3), sep = "\n")
#> DESCRIPTION:
#> Package: rJavaEnv
#> Title: 'Java' Environments for R Projects
# Print the last 3 lines
cat(tail(lines, 3), sep = "\n")
#>      "17" == java_check_version_rjava(quiet = TRUE)
#>      ## End(Not run)
#>      

# clean cache directory
unlink(getOption("rdocdump.cache_path"), recursive = TRUE, force = TRUE)
# }
```
