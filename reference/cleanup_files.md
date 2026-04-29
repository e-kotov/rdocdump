# Cleanup Temporary Files

Clean up temporary package archive and extracted files according to a
keep_files policy.

## Usage

``` r
cleanup_files(pkg_info, keep_files)
```

## Arguments

- pkg_info:

  A list returned by
  [`resolve_pkg_path()`](https://www.ekotov.pro/rdocdump/reference/resolve_pkg_path.md),
  containing `tar_path` and `extracted_path`.

- keep_files:

  A `character` value controlling whether temporary files should be
  kept. Possible values are:

  - `"none"`: Delete both the tar.gz archive and the extracted files
    (default).

  - `"tgz"`: Keep only the tar.gz archive.

  - `"extracted"`: Keep only the extracted files.

  - `"both"`: Keep both the tar.gz archive and the extracted files.

## Value

Invisibly returns `NULL`. If there are any issues with file deletion,
warnings are issued.
