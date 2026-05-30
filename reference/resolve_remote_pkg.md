# Resolve Remote Package References (GitHub, GitLab, Bioconductor, etc.)

Downloads package source from remote repositories without installing the
package. Uses the `pak` package for downloading.

If `pak` fails to resolve the reference (e.g., because the R package is
in a subdirectory and no `subdir` was provided), the function
automatically falls back to downloading the full repository and scanning
for the shallowest directory containing a `DESCRIPTION` file.

## Usage

``` r
resolve_remote_pkg(pkg_ref, cache_path = NULL)
```

## Arguments

- pkg_ref:

  A character string specifying the remote package reference. Supports
  any format supported by `pak`. See `?pak::pak_package_sources` for a
  full list of supported formats. Examples:

  - `"user/repo"` - GitHub shorthand (default)

  - `"github::user/repo"` - Explicit GitHub

  - `"gitlab::user/repo"` - GitLab

  - `"bioc::pkgname"` - Bioconductor

  - `"git::https://..."` - Arbitrary Git URL

  - `"bitbucket::user/repo"` - Bitbucket (translated to `git::`)

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

- `pkg_name`: Package name reported by pak (or repo slug fallback)

- `pkg_version`: Package version reported by pak, if available

- `remote_info`: Parsed remote reference information

## Details

The auto-discovery mechanism uses two fallback tiers if `pak` resolution
fails:

1.  **Archive Download:** Attempts to download a `.tar.gz` archive of
    the repository for known hosts (GitHub, GitLab, Bitbucket).

2.  **Git Clone:** Uses `git clone --depth 1` for arbitrary Git URLs or
    if the archive download fails (requires system `git`).

Once downloaded, it recursively searches for `DESCRIPTION` files and
selects the one closest to the repository root.
