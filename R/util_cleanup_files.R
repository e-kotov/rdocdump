#' Cleanup Temporary Files
#'
#' @description
#' Clean up temporary package archive and extracted files according to a keep_files policy.
#'
#' @param pkg_info A list returned by `resolve_pkg_path()`, containing `tar_path` and `extracted_path`.
#'
#' @inheritParams rdd_to_txt
#'
#' @return Invisibly returns `NULL` after deleting the specified files.
#' @keywords internal
#'
cleanup_files <- function(pkg_info, keep_files) {
  # delete archive unless “tgz” or “both”
  if (!keep_files %in% c("tgz", "both") && !is.null(pkg_info$tar_path)) {
    unlink(pkg_info$tar_path)
  }
  # delete extracted folder unless “extracted” or “both”
  if (
    !keep_files %in% c("extracted", "both") && !is.null(pkg_info$extracted_path)
  ) {
    unlink(dirname(pkg_info$extracted_path), recursive = TRUE)
  }
}
