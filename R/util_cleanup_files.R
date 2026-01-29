#' Cleanup Temporary Files
#'
#' @description
#' Clean up temporary package archive and extracted files according to a keep_files policy.
#'
#' @param pkg_info A list returned by `resolve_pkg_path()`, containing `tar_path` and `extracted_path`.
#'
#' @inheritParams rdd_to_txt
#'
#' @return Invisibly returns `NULL`. If there are any issues with file deletion, warnings are issued.
#'
#' @keywords internal
#'
cleanup_files <- function(
  pkg_info,
  keep_files
) {
  if (!keep_files %in% c("tgz", "both") && !is.null(pkg_info$tar_path)) {
    res <- unlink(pkg_info$tar_path)
    if (res != 0L) {
      warning(
        "cleanup_files: failed to delete archive: ",
        pkg_info$tar_path,
        call. = FALSE
      )
    }
  }

  if (!keep_files %in% c("extracted", "both")) {
    # If we have an extracted root, remove it (this includes the pkg_path inside it).
    if (!is.null(pkg_info$extracted_path)) {
      dir_to_remove <- pkg_info$extracted_path
      res <- unlink(dir_to_remove, recursive = TRUE)
      if (res != 0L) {
        warning(
          "cleanup_files: failed to delete extracted directory: ",
          dir_to_remove,
          call. = FALSE
        )
      }
    } else if (!is.null(pkg_info$pkg_path) && !isTRUE(pkg_info$is_installed)) {
      # Fallback: if there is no extracted_path (e.g. source dir), but we are supposed to clean up?
      # Actually, if is_installed is FALSE and tar_path was NULL, it might be a local source dir provided by user.
      # resolve_pkg_path sets extracted_path=NULL for existing source dirs.
      # We generally should NOT delete user-provided source dirs unless we created them.
      # But resolve_pkg_path sets extracted_path only if it extracted/downloaded something.
      # So relying on extracted_path is correct.
    }
  }

  invisible(NULL)
}
