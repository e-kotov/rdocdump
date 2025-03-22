#' Set RDocDump Cache Path for the Session
#'
#' This function sets the cache path used to store temporary files (downloaded tar.gz archives
#' and/or extracted directories) for the current session. The cache path is stored in the option
#' `"rdocdump.cache_path"`.
#'
#' @param path A character string specifying the directory to be used as the cache path.
#'
#' @return Invisibly returns the new cache path.
#' @export
rdd_set_cache_path <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  options(rdocdump.cache_path = normalizePath(path, winslash = "/"))
  message("rdocdump.cache_path set to: ", getOption("rdocdump.cache_path"))
  invisible(getOption("rdocdump.cache_path"))
}
