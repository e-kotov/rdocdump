# Changelog

## rdocdump (development version)

#### New Features

- Migrated to `pak` for remote package resolution and downloads.

- Added auto-discovery of R packages in subdirectories for remote
  repositories.

#### Improvements

- Enhanced robustness of remote package resolution with automatic
  fallbacks (e.g., `git clone`).

- Improved cross-platform compatibility by using internal `tar`
  operations.

- Expanded support for various remote URL formats and platforms
  (Bioconductor, GitLab, etc.).

#### Bug Fixes

- Fixed directory cleanup logic and improved performance of vignette
  concatenation.

## rdocdump 0.2.0 (2026-04-29)

CRAN release: 2026-04-29

- Added support for remote repository references (GitHub, GitLab,
  Bitbucket) in
  [`rdd_to_txt()`](https://www.ekotov.pro/rdocdump/reference/rdd_to_txt.md)
  and
  [`rdd_extract_code()`](https://www.ekotov.pro/rdocdump/reference/rdd_extract_code.md).
  This includes support for specific branches/tags/commits and packages
  in subdirectories, as well as direct URLs (to branches and/or folders
  within repositories). Therefore, added the `remotes` package to
  `Suggests` to handle remote downloads.

## rdocdump 0.1.1 (2025-08-21)

CRAN release: 2025-08-21

- Added `version` argument to
  [`rdd_to_txt()`](https://www.ekotov.pro/rdocdump/reference/rdd_to_txt.md)
  and
  [`rdd_extract_code()`](https://www.ekotov.pro/rdocdump/reference/rdd_extract_code.md)
  functions to specify the version of the package to download from CRAN
  for processing.

## rdocdump 0.1.0 (2025-06-15)

CRAN release: 2025-06-18

- Initial CRAN submission.

- Basic functionality for parsing R documentation and source files.
