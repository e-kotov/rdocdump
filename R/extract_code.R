#' Extract R Source Code from a Package
#'
#' @description
#' This function extracts the R source code from a package. For installed packages, it retrieves the package namespace and deparses all functions found in the package. For package source directories or archives (non-installed packages), it reads all `.R` files from the `R` directory and, optionally, from the `tests` directory. Optionally, it can include roxygen2 documentation from these files.
#'
#' @param pkg A `character` string specifying the package. This can be:
#' \itemize{
#'   \item an installed package name,
#'   \item a full path to a package source directory,
#'   \item a full path to a package archive file (tar.gz), or
#'   \item a package name not installed (which will then be downloaded from CRAN).
#' }
#' @param file Optional. Save path for the output text file. If set, the function will return the path to the file instead of the combined text. Defaults to `NULL`.
#' @param include_tests `logical`. If `TRUE`, for non-installed packages, the function will also include R source code from the `tests` directory. Defaults to `FALSE`.
#' @param include_roxygen `logical`. If `TRUE`, roxygen2 documentation lines (lines starting with "#'") from R files will be included in the output. Defaults to `FALSE`.
#' @param force_fetch `logical`. If `TRUE`, the package source will be fetched from CRAN even if the package is installed locally. Default is `FALSE`.
#' @param cache_path A `character` string specifying the directory to use as a cache. Defaults to the value of `getOption("rdocdump.cache_path")`.
#'
#' @inheritParams rdd_to_txt
#'
#' @return A single string containing the combined R source code (and, optionally, roxygen2 documentation) from the package.
#'
#' @export
#'
#' @examples
#' # Extract only R source code (excluding roxygen2 documentation) from an installed package.
#' code <- rdd_extract_code("splines")
#' cat(substr(code, 1, 1000))
#'
#' # Extract R source code including roxygen2 documentation from a package source directory.
#' \donttest{
#' # set cache directory for `rdocdump`
#' rdd_set_cache_path(paste0(tempdir(), "/rdocdump_cache"))
#'
#' local({
#'  code_with_roxygen <- rdd_extract_code(
#'   "ini",
#'   include_roxygen = TRUE,
#'   force_fetch = TRUE,
#'   repos = c("CRAN" = "https://cran.r-project.org")
#' )
#'  cat(substr(code_with_roxygen, 1, 1000))
#'})
#'
#' # Extract R source code from a package source directory,
#' # including test files but excluding roxygen2 docs.
#' local({
#'  code_with_tests <- rdd_extract_code(
#'   "ini",
#'   include_roxygen = TRUE,
#'   include_tests = TRUE,
#'   force_fetch = TRUE,
#'   repos = c("CRAN" = "https://cran.r-project.org")
#' )
#'  cat(substr(code_with_tests, 1, 1000))
#'})
#' # clean cache directory
#' unlink(getOption("rdocdump.cache_path"), recursive = TRUE, force = TRUE)
#' }
#'
rdd_extract_code <- function(
  pkg,
  file = NULL,
  include_tests = FALSE,
  include_roxygen = FALSE,
  force_fetch = FALSE,
  cache_path = getOption("rdocdump.cache_path"),
  keep_files = "none",
  repos = getOption("rdocdump.repos", getOption("repos"))
) {
  pkg_info <- resolve_pkg_path(
    pkg,
    cache_path,
    force_fetch = force_fetch,
    repos = repos
  )

  combined_code <- if (
    !is.null(pkg_info$is_installed) && pkg_info$is_installed
  ) {
    if (is.null(pkg_info$pkg_name)) {
      stop("Installed package does not provide pkg_name information.")
    }
    extract_code_installed(pkg_info$pkg_name)
  } else {
    extract_code_source(
      pkg_info$pkg_path,
      include_tests,
      include_roxygen
    )
  }

  # Clean up temporary files according to keep_files
  cleanup_result <- cleanup_files(pkg_info, keep_files)

  if (!is.null(file)) {
    writeLines(combined_code, con = file)
    return(file)
  }
  combined_code
}

#' Extract code from an installed package using its namespace.
#' This function retrieves all functions from the package namespace and deparses them to get their source code.
#' @param pkg_name The name of the installed package.
#' @return A single string containing the source code of all functions in the package.
#' @keywords internal
extract_code_installed <- function(pkg_name) {
  # Load the namespace of the installed package.
  ns <- asNamespace(pkg_name)
  obj_names <- ls(ns, all.names = TRUE)
  code_strings <- lapply(obj_names, function(nm) {
    obj <- get(nm, envir = ns)
    if (is.function(obj)) {
      # Deparse the function to retrieve its source code.
      code <- deparse(obj)
      header <- paste0(strrep("-", 80), "\nFunction: ", nm, "()\n")
      paste0(header, paste(code, collapse = "\n"))
    } else {
      NULL
    }
  })
  # Combine all function source codes.
  combined_code <- paste(unlist(code_strings), collapse = "\n\n")
  return(combined_code)
}

#' Helper function to extract code from package source files.
#' This function reads all `.R` files in the `R` directory and optionally includes files from the `tests` directory.
#' It can also exclude roxygen2 documentation lines.
#' @param pkg_path Path to the package source directory.
#' @inheritParams rdd_extract_code
#' @return A single string containing the source code from the package's R files.
#' @keywords internal
extract_code_source <- function(
  pkg_path,
  include_tests = FALSE,
  include_roxygen = FALSE
) {
  code_text <- ""
  # Read all .R files in the R directory.
  r_dir <- file.path(pkg_path, "R")
  if (dir.exists(r_dir)) {
    r_files <- list.files(r_dir, pattern = "\\.[rR]$", full.names = TRUE)
    for (rf in r_files) {
      header <- paste0(strrep("-", 80), "\nFile: ", basename(rf), "\n")
      file_content <- readLines(rf, warn = FALSE)
      # Exclude roxygen2 documentation if not requested.
      if (!include_roxygen) {
        file_content <- file_content[!grepl("^#'", file_content)]
      }
      code_text <- paste(
        code_text,
        header,
        paste(file_content, collapse = "\n"),
        "\n\n",
        sep = "\n"
      )
    }
  } else {
    warning("R directory not found in the package source.")
  }

  # Optionally, include R files from tests directory.
  if (include_tests) {
    tests_dir <- file.path(pkg_path, "tests")
    if (dir.exists(tests_dir)) {
      test_files <- list.files(
        tests_dir,
        pattern = "\\.[rR]$",
        full.names = TRUE
      )
      for (tf in test_files) {
        header <- paste0(strrep("-", 80), "\nTest File: ", basename(tf), "\n")
        file_content <- readLines(tf, warn = FALSE)
        if (!include_roxygen) {
          file_content <- file_content[!grepl("^#'", file_content)]
        }
        code_text <- paste(
          code_text,
          header,
          paste(file_content, collapse = "\n"),
          "\n\n",
          sep = "\n"
        )
      }
    }
  }

  return(code_text)
}
