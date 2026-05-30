# Resolve the path to a package directory or tarball

This function resolves the path to a package directory or tarball,
handling both installed packages and source packages from CRAN.

## Usage

``` r
resolve_pkg_path(
  pkg,
  cache_path = NULL,
  force_fetch = FALSE,
  version = NULL,
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

- cache_path:

  A `character` string specifying the directory where kept temporary
  files will be stored. By default, it uses the value of
  `getOption("rdocdump.cache_path")` which sets the cache directory to
  the temporary directory of the current R session.

- force_fetch:

  `logical`. If `TRUE`, the package source will be fetched from CRAN as
  a tar.gz archive even if the package is already installed locally.
  Default is `FALSE`, but when `version` is specified, it will be set to
  `TRUE`.

- version:

  Optional. A `character` string specifying the package version to fetch
  from CRAN. If not provided, the latest version will be used.

- repos:

  A `character` vector of repository URLs. By default, it uses the value
  of `getOption("rdocdump.repos")` which sets the repository URLs to the
  default R repositories and is itself set to
  `c("CRAN" = "https://cloud.r-project.org")` on package load to prevent
  accidental downloads of pre-built packages from Posit Package Manager
  and R Universe.

## Value

A list containing:

- `pkg_path`: Path to the package directory or tarball.

- `extracted_path`: Path to the extracted package directory (if
  applicable).

- `tar_path`: Path to the tarball if it was downloaded.

- `is_installed`: Logical indicating if the package is installed.

- `pkg_name`: Package name (always populated when known).

- `pkg_version`: Package version (NULL if not known).
