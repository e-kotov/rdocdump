# Set `rdocdump` Cache Path in the Current R Session

This function sets the cache path used by `rdocdump` to store temporary
files (downloaded tar.gz archives and/or extracted directories) for the
current R session. The cache path is stored in the option
`"rdocdump.cache_path"`, which can be checked with
`getOption("rdocdump.cache_path")`. The path is created if it does not
exist.

## Usage

``` r
rdd_set_cache_path(path)
```

## Arguments

- path:

  A `character` string specifying the directory to be used as the cache
  path.

## Value

Invisibly returns the new cache path.

## Examples

``` r
# set cache directory for `rdocdump`
rdd_set_cache_path(paste0(tempdir(), "/rdocdump_cache"))
#> rdocdump.cache_path set to: /tmp/Rtmp3eZRvK/rdocdump_cache
# default cache directory
unlink(getOption("rdocdump.cache_path"), recursive = TRUE)
```
