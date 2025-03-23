#' Create Combined Package Documentation as Text
#'
#' @description
#' This function produces a single text output for a package by processing its documentation (Rd files from the package source, or the documentation from already installed packages) and/or vignettes (from `*.Rmd`, `*.qmd`, `*.md`, and `*.Rnw` files). The user can provide the function with an installed package name, a package source directory, a tar.gz archive, or a package name to download from CRAN.
#'
#' @param pkg A `character` string specifying the package. This can be:
#' \itemize{
#'   \item an installed package name,
#'   \item a full path to a package source directory,
#'   \item a full path to a package archive file (tar.gz), or
#'   \item a package name not installed (which will then be downloaded from CRAN).
#' }
#' @param file Optional. Save path for the output text file. If set, the function will return the path to the file instead of the combined text. Defaults to `NULL`.
#'
#' @param force_fetch `logical`. If `TRUE`, the package source will be fetched from CRAN as a tar.gz archive even if the package is already installed locally. Default is `FALSE`.
#'
#' @param content A `character` string specifying which components to include in the output.
#' Possible values are:
#' \itemize{
#'   \item `"both"`: Include both Rd documentation and vignettes (default).
#'   \item `"docs"`: Include only the Rd documentation.
#'   \item `"vignettes"`: Include only the vignettes.
#' }
#'
#' @param keep_files A `character` value controlling whether temporary files should be kept.
#' Possible values are:
#' \itemize{
#'   \item `"none"`: Delete both the tar.gz archive and the extracted files (default).
#'   \item `"tgz"`: Keep only the tar.gz archive.
#'   \item `"extracted"`: Keep only the extracted files.
#'   \item `"both"`: Keep both the tar.gz archive and the extracted files.
#' }
#'
#' @param cache_path A `character` string specifying the directory where kept temporary files will be stored. By default, it uses the value of `getOption("rdocdump.cache_path")` which sets the cache directory to the temporary directory of the current R session.
#'
#' @return A single string containing the combined package documentation. If the `file` argument is set, returns the path to the file.
#'
#' @export
#'
#' @examples
#' # Extract documentation for built-in `stats` package (both docs and vignettes).
#' docs <- rdd_to_txt("stats")
#'
#' \donttest{
#' # Extract only documentation for rJavaEnv by downloading its source from CRAN
#' local({
#'   old_repos <- getOption("repos")
#'   options(repos = c(CRAN = "https://cran.r-project.org"))
#'   docs <- rdd_to_txt("rJavaEnv", force_fetch = TRUE, content = "docs")
#'   lines <- unlist(strsplit(docs, "\n"))
#'   # Print the first 3 lines
#'   cat(head(lines, 3), sep = "\n")
#'   # Print the last 3 lines
#'   cat(tail(lines, 3), sep = "\n")
#'   options(repos = old_repos)
#'   #unlink(getOption("rdocdump.cache_path"), recursive = TRUE)
#' })
#' }
rdd_to_txt <- function(
  pkg,
  file = NULL,
  content = "both",
  force_fetch = FALSE,
  keep_files = "none",
  cache_path = getOption("rdocdump.cache_path")
) {
  # Validate keep_files argument.
  if (!keep_files %in% c("none", "tgz", "extracted", "both")) {
    stop(
      'Invalid value for keep_files. Choose one of "none", "tgz", "extracted", "both".'
    )
  }

  # Validate content argument.
  if (!content %in% c("both", "vignettes", "docs")) {
    stop(
      'Invalid value for content. Choose one of "both", "vignettes", "docs".'
    )
  }

  # Resolve package source path using existing helper.
  pkg_info <- resolve_pkg_path(pkg, cache_path, force_fetch = force_fetch)
  pkg_path <- pkg_info$pkg_path

  # Process Rd documentation.
  rd_text <- combine_rd(
    pkg_path,
    is_installed = pkg_info$is_installed,
    pkg_name = pkg_info$pkg_name
  )

  # Process vignettes.
  vignettes_text <- combine_vignettes(pkg_path)

  # Combine documentation and/or vignettes based on the 'content' argument.
  if (content == "docs") {
    combined_text <- rd_text
  } else if (content == "vignettes") {
    combined_text <- vignettes_text
  } else {
    # content == "both"
    combined_text <- paste(rd_text, "\n\n", vignettes_text, sep = "")
  }

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
