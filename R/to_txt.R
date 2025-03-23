#' Create Combined Package Documentation as Text
#'
#' This function produces a single text output for a package by processing its Rd
#' files. The package can be provided as an installed package name, a package source
#' directory, a tar.gz archive, or a package name to download from CRAN. (In the future,
#' vignettes will be processed as well.)
#'
#' @param pkg A character string specifying the package. This can be:
#' \itemize{
#'   \item an installed package name,
#'   \item a full path to a package source directory,
#'   \item a full path to a package archive file (tar.gz), or
#'   \item a package name not installed (which will then be downloaded from CRAN).
#' }
#' @param file Optional. Full path for the output text file.
#' @param keep_files Character value controlling whether temporary files should be kept.
#' Possible values are:
#' \itemize{
#'   \item `"none"`: Delete both the tar.gz archive and the extracted files (default).
#'   \item `"tgz"`: Keep only the tar.gz archive.
#'   \item `"extracted"`: Keep only the extracted files.
#'   \item `"both"`: Keep both the tar.gz archive and the extracted files.
#' }
#' @param cache_path Character string specifying the directory where kept temporary files
#' will be stored. By default, it uses the value of option("rdocdump.cache_path").
#' @param force_fetch Logical. If TRUE, the package source will be fetched from CRAN
#' as a tar.gz archive even if the package is already installed locally.
#' Default is FALSE.
#'
#' @return A single string containing the combined package documentation.
#' @export
rdd_to_txt <- function(
  pkg,
  file = NULL,
  keep_files = "none",
  cache_path = getOption("rdocdump.cache_path"),
  force_fetch = FALSE
) {
  # Validate keep_files argument.
  if (!keep_files %in% c("none", "tgz", "extracted", "both")) {
    stop(
      'Invalid value for keep_files. Choose one of "none", "tgz", "extracted", "both".'
    )
  }

  pkg_info <- resolve_pkg_path(pkg, cache_path, force_fetch = force_fetch)
  pkg_path <- pkg_info$pkg_path

  # Pass is_installed and pkg_name (if available) to combine_rd.
  rd_text <- combine_rd(
    pkg_path,
    is_installed = pkg_info$is_installed,
    pkg_name = pkg_info$pkg_name
  )

  # TODO: In the future, add processing for vignettes.
  combined_text <- rd_text

  # Decide whether to keep or delete temporary files.
  if (
    keep_files %in%
      c("tgz", "both") &&
      !is.null(pkg_info$tar_path) &&
      !is.null(cache_path)
  ) {
    if (!dir.exists(cache_path)) {
      dir.create(cache_path, recursive = TRUE)
    }
    dest_archive <- file.path(cache_path, basename(pkg_info$tar_path))
    # Optionally, you might copy or leave the file in place.
  } else if (keep_files %in% c("none", "extracted")) {
    if (!is.null(pkg_info$tar_path)) {
      unlink(pkg_info$tar_path)
    }
  }
  if (
    keep_files %in%
      c("extracted", "both") &&
      !is.null(pkg_info$extracted_path) &&
      !is.null(cache_path)
  ) {
    if (!dir.exists(cache_path)) {
      dir.create(cache_path, recursive = TRUE)
    }
    dest_extracted <- file.path(cache_path, basename(pkg_info$extracted_path))
    # Optionally, you might copy or leave the folder in place.
  } else if (keep_files %in% c("none", "tgz")) {
    if (!is.null(pkg_info$extracted_path)) {
      unlink(pkg_info$extracted_path, recursive = TRUE)
    }
  }

  if (!is.null(file)) {
    writeLines(combined_text, con = file)
    return(file)
  }
  return(combined_text)
}
