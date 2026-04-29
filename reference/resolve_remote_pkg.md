# Resolve Remote Package References (GitHub, GitLab, Bitbucket)

Downloads package source from remote repositories (GitHub, GitLab,
Bitbucket) without installing the package. Uses the `remotes` package
for downloading.

## Usage

``` r
resolve_remote_pkg(pkg_ref, cache_path = NULL)
```

## Arguments

- pkg_ref:

  A character string specifying the remote package reference. Supports
  formats like:

  - `"user/repo"` - GitHub shorthand (default)

  - `"github::user/repo"` - Explicit GitHub

  - `"gitlab::user/repo"` - GitLab

  - `"bitbucket::user/repo"` - Bitbucket

  - `"user/repo@ref"` - Specific commit, branch, or tag

  - `"user/repo/subdir"` - Package in subdirectory

- cache_path:

  Optional path to cache directory. If NULL, uses temp directory.

## Value

A list containing:

- `pkg_path`: Path to the package directory

- `extracted_path`: Path to the extracted bundle

- `tar_path`: Path to the downloaded tarball

- `is_installed`: FALSE (always FALSE for remote packages)

- `remote_info`: Parsed remote reference information
