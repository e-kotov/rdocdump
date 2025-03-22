.onLoad <- function(libname, pkgname) {
  default_cache <- file.path(tempdir(), "rdocdump_cache")
  if (is.null(getOption("rdocdump.cache_path"))) {
    options(rdocdump.cache_path = default_cache)
  }
}
