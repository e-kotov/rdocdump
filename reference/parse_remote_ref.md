# Parse Remote Reference String

Supports any format supported by `pak`. See `?pak::pak_package_sources`
for details.

## Usage

``` r
parse_remote_ref(ref)
```

## Arguments

- ref:

  Character string reference

## Value

List with components: type, user, repo, ref, subdir

## Details

Ambiguous web URLs (e.g. branch names like `feat/foo/pkg`) are best
supplied as `user/repo@ref/subdir`; the URL heuristic is intentionally
limited.
