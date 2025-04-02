.onLoad <- function(libname, pkgname) {
  default_cache <- file.path(tempdir(), "rdocdump_cache")
  if (is.null(getOption("rdocdump.cache_path"))) {
    options(rdocdump.cache_path = default_cache)
  }
  # Set default repository option for rdocdump
  if (is.null(getOption("rdocdump.repos"))) {
    options(rdocdump.repos = c("CRAN" = "https://cloud.r-project.org"))
  }
}
