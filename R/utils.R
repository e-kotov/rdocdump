#' NULL default operator
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Flatten an extracted bundle when it has a single top-level directory
#'
#' Many source bundles (CRAN tarballs, GitHub/GitLab archives) extract into a
#' single wrapper directory. This helper moves the contents of that wrapper
#' up to `extract_dir` and removes the wrapper, so callers can treat
#' `extract_dir` as the package root.
#'
#' @param extract_dir Directory containing the freshly-extracted bundle.
#' @return `extract_dir`, invisibly.
#' @keywords internal
#' @noRd
flatten_extracted_dir <- function(extract_dir) {
  top_level <- list.files(
    extract_dir,
    all.files = TRUE,
    no.. = TRUE,
    full.names = TRUE
  )
  if (length(top_level) == 1L && dir.exists(top_level[1L])) {
    wrapper <- top_level[1L]
    files <- list.files(
      wrapper,
      full.names = TRUE,
      all.files = TRUE,
      no.. = TRUE
    )
    # Use file.rename for an atomic, fast move instead of recursive file.copy
    ok <- file.rename(files, file.path(extract_dir, basename(files)))
    if (!all(ok)) {
      stop("Failed to move extracted package files out of the wrapper directory.")
    }
    unlink(wrapper, recursive = TRUE)
  }
  invisible(extract_dir)
}

#' Read Package and Version from a DESCRIPTION file
#'
#' @param pkg_dir Path to a package directory.
#' @return Named list with `pkg_name` and `pkg_version`. Either may be `NA`
#'   if the DESCRIPTION is missing or malformed.
#' @keywords internal
#' @noRd
read_pkg_meta <- function(pkg_dir) {
  desc <- file.path(pkg_dir, "DESCRIPTION")
  result <- list(pkg_name = NA_character_, pkg_version = NA_character_)
  if (!file.exists(desc)) {
    return(result)
  }
  fields <- tryCatch(
    read.dcf(desc, fields = c("Package", "Version")),
    error = function(e) {
      warning(sprintf("Failed to read DESCRIPTION file at %s: %s", desc, conditionMessage(e)))
      NULL
    }
  )
  if (is.null(fields) || nrow(fields) < 1L) {
    return(result)
  }
  if ("Package" %in% colnames(fields)) {
    val <- unname(fields[1L, "Package"])
    if (!is.na(val) && nzchar(val)) result$pkg_name <- val
  }
  if ("Version" %in% colnames(fields)) {
    val <- unname(fields[1L, "Version"])
    if (!is.na(val) && nzchar(val)) result$pkg_version <- val
  }
  result
}
