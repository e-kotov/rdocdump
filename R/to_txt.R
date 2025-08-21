#' Dump Package Source, Documentationm and Vignettes into Plain Text
#'
#' @description
#' This function produces a single text output for an R package by processing its documentation (Rd files from the package source or the documentation from already installed packages), vignettes, and/or R source code.
#'
#' @param pkg A `character` string specifying the package. This can be:
#' \itemize{
#'   \item an installed package name,
#'   \item a full path to a package source directory,
#'   \item a full path to a package archive file (tar.gz), or
#'   \item a package name not installed (which will then be downloaded from CRAN).
#' }
#' @param file Optional. Save path for the output text file. If set, the function will return the path to the file instead of the combined text. Defaults to `NULL`.
#' @param force_fetch `logical`. If `TRUE`, the package source will be fetched from CRAN as a tar.gz archive even if the package is already installed locally. Default is `FALSE`, but when `version` is specified, it will be set to `TRUE`.
#' @param version Optional. A `character` string specifying the package version to fetch from CRAN. If not provided, the latest version will be used.
#' @param content A character vector specifying which components to include in the output.
#' Possible values are:
#' \itemize{
#'   \item `"all"`: Include Rd documentation, vignettes, and R source code (default).
#'   \item `"docs"`: Include only the Rd documentation.
#'   \item `"vignettes"`: Include only the vignettes.
#'   \item `"code"`: Include only the R source code. When extracting code for non-installed packages, the function will not include roxygen2 documentation, as the documentation can be imported from the Rd files. If you want to extract the R source code with the roxygen2 documentation, use \code{\link{rdd_extract_code}} and set `include_roxygen` to `TRUE`.
#' }
#' You can specify multiple options (e.g., `c("docs", "code")` to include both documentation and source code).
#' @param keep_files A `character` value controlling whether temporary files should be kept.
#' Possible values are:
#' \itemize{
#'   \item `"none"`: Delete both the tar.gz archive and the extracted files (default).
#'   \item `"tgz"`: Keep only the tar.gz archive.
#'   \item `"extracted"`: Keep only the extracted files.
#'   \item `"both"`: Keep both the tar.gz archive and the extracted files.
#' }
#' @param cache_path A `character` string specifying the directory where kept temporary files will be stored.
#' By default, it uses the value of `getOption("rdocdump.cache_path")` which sets the cache directory to the temporary directory of the current R session.
#'
#' @param repos A `character` vector of repository URLs. By default, it uses the value of `getOption("rdocdump.repos")` which sets the repository URLs to the default R repositories and is itself set to `c("CRAN" = "https://cloud.r-project.org")` on package load to prevent accidental downloads of pre-built packages from Posit Package Manager and R Universe.
#'
#' @return A single string containing the combined package documentation, vignettes, and/or code as specified by the `content` argument.
#' If the `file` argument is set, returns the path to the file.
#'
#' @export
#'
#' @examples
#' # Extract documentation for built-in `stats` package (both docs and vignettes).
#' docs <- rdd_to_txt("splines")
#' cat(substr(docs, 1, 500))
#'
#' \donttest{
#' # set cache directory for `rdocdump`
#' rdd_set_cache_path(paste0(tempdir(), "/rdocdump_cache"))
#'
#' # Extract only documentation for rJavaEnv by downloading its source from CRAN
#' docs <- rdd_to_txt(
#'   "rJavaEnv",
#'   force_fetch = TRUE,
#'   content = "docs",
#'   repos = c("CRAN" = "https://cran.r-project.org")
#' )
#' lines <- unlist(strsplit(docs, "\n"))
#' # Print the first 3 lines
#' cat(head(lines, 3), sep = "\n")
#' # Print the last 3 lines
#' cat(tail(lines, 3), sep = "\n")
#'
#' # clean cache directory
#' unlink(getOption("rdocdump.cache_path"), recursive = TRUE, force = TRUE)
#' }
#'
rdd_to_txt <- function(
  pkg,
  file = NULL,
  content = "all",
  force_fetch = FALSE,
  version = NULL,
  keep_files = "none",
  cache_path = getOption("rdocdump.cache_path"),
  repos = getOption("rdocdump.repos", getOption("repos"))
) {
  # Validate keep_files argument.
  if (!keep_files %in% c("none", "tgz", "extracted", "both")) {
    stop(
      'Invalid value for keep_files. Choose one of "none", "tgz", "extracted", "both".'
    )
  }

  # Define allowed content options.
  allowed_content <- c("docs", "vignettes", "code")
  # If "all" is specified, use all allowed options.
  if ("all" %in% content) {
    effective_content <- allowed_content
  } else {
    effective_content <- content
  }
  if (!all(effective_content %in% allowed_content)) {
    stop(paste(
      'Invalid value for content. Choose any of',
      paste(c("all", allowed_content), collapse = ", "),
      "."
    ))
  }

  # Resolve package source path using the existing helper.
  pkg_info <- resolve_pkg_path(
    pkg,
    cache_path,
    force_fetch = force_fetch || !is.null(version),
    version = version,
    repos = repos
  )
  pkg_path <- pkg_info$pkg_path

  # Initialize component texts.
  docs_text <- ""
  vignettes_text <- ""
  code_text <- ""

  if ("docs" %in% effective_content) {
    docs_text <- combine_rd(
      pkg_path,
      is_installed = pkg_info$is_installed,
      pkg_name = pkg_info$pkg_name
    )
  }
  if ("vignettes" %in% effective_content) {
    vignettes_text <- combine_vignettes(pkg_path)
  }
  if ("code" %in% effective_content) {
    code_text <- rdd_extract_code(
      pkg = if (pkg_info$is_installed) pkg else pkg_path,
      file = NULL,
      include_tests = FALSE,
      include_roxygen = FALSE,
      force_fetch = force_fetch || !is.null(version),
      version = version,
      cache_path = cache_path,
      keep_files = "both" # make sure the files are not deleted prematurely, as rdd_to_txt will take care of that later
    )
  }

  # Combine components in a fixed order: docs, vignettes, then code.
  components <- list()
  if ("docs" %in% effective_content && nzchar(docs_text)) {
    components <- c(components, docs_text)
  }
  if ("vignettes" %in% effective_content && nzchar(vignettes_text)) {
    components <- c(components, vignettes_text)
  }
  if ("code" %in% effective_content && nzchar(code_text)) {
    components <- c(components, code_text)
  }
  combined_text <- paste(components, collapse = "\n\n")

  # Clean up temporary files according to keep_files
  cleanup_result <- cleanup_files(pkg_info, keep_files)

  if (!is.null(file)) {
    writeLines(combined_text, con = file)
    return(file)
  }
  return(combined_text)
}
