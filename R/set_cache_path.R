#' Set `rdocdump` Cache Path in the Current R Session
#'
#' @description
#' This function sets the cache path used by `rdocdump` to store temporary files (downloaded tar.gz archives and/or extracted directories) for the current R session. The cache path is stored in the option `"rdocdump.cache_path"`, which can be checked with `getOption("rdocdump.cache_path")`. The path is created if it does not exist.
#'
#' @param path A `character` string specifying the directory to be used as the cache path.
#'
#' @return Invisibly returns the new cache path.
#' @export
#' @examples
#' # set cache directory for `rdocdump`
#' rdd_set_cache_path(paste0(tempdir(), "/rdocdump_cache"))
#' # default cache directory
#' unlink(getOption("rdocdump.cache_path"), recursive = TRUE)
rdd_set_cache_path <- function(
  path
) {
  # Validate the input path
  if (!is.character(path) || length(path) != 1) {
    stop(
      "`path` argument must be a single character string specifying the cache directory."
    )
  }

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  options(rdocdump.cache_path = normalizePath(path, winslash = "/"))
  message("rdocdump.cache_path set to: ", getOption("rdocdump.cache_path"))
  invisible(getOption("rdocdump.cache_path"))
}
