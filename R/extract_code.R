#' Extract R Source Code from a Package
#'
#' @description
#' This function extracts the R source code from a package. For installed packages, it retrieves the
#' package namespace and deparses all functions found in the package. For package source directories
#' or archives (non-installed packages), it reads all `.R` files from the `R` directory and, optionally, from the `tests`
#' directory.
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
#' @param force_fetch `logical`. If `TRUE`, the package source will be fetched from CRAN even if the package is installed locally. Default is `FALSE`.
#' @param cache_path A `character` string specifying the directory to use as a cache. Defaults to the value of `getOption("rdocdump.cache_path")`.
#'
#' @return A single string containing the combined R source code from the package.
#'
#' @export
#'
#' @examples
#' # Extract only R source code from an installed package.
#' code <- rdd_extract_code("stats")
#' cat(code)
#'
#' # Extract R source code from a package source directory (and optionally include test files).
#' code_with_tests <- rdd_extract_code("mypackage", include_tests = TRUE)
#' cat(code_with_tests)
rdd_extract_code <- function(
  pkg,
  file = NULL,
  include_tests = FALSE,
  force_fetch = FALSE,
  cache_path = getOption("rdocdump.cache_path")
) {
  pkg_info <- resolve_pkg_path(pkg, cache_path, force_fetch = force_fetch)

  combined_code <- ""

  if (!is.null(pkg_info$is_installed) && pkg_info$is_installed) {
    # For installed packages, extract code via the package namespace.
    if (is.null(pkg_info$pkg_name)) {
      stop("Installed package does not provide pkg_name information.")
    }
    combined_code <- extract_code_installed(pkg_info$pkg_name)
  } else {
    # For non-installed packages, read the source files.
    combined_code <- extract_code_source(pkg_info$pkg_path, include_tests)
  }

  if (!is.null(file)) {
    writeLines(combined_code, con = file)
    return(file)
  }

  return(combined_code)
}

# Helper function to extract code from an installed package using its namespace.
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

# Helper function to extract code from package source files.
extract_code_source <- function(pkg_path, include_tests = FALSE) {
  code_text <- ""
  # Read all .R files in the R directory.
  r_dir <- file.path(pkg_path, "R")
  if (dir.exists(r_dir)) {
    r_files <- list.files(r_dir, pattern = "\\.[rR]$", full.names = TRUE)
    for (rf in r_files) {
      header <- paste0(strrep("-", 80), "\nFile: ", basename(rf), "\n")
      file_content <- readLines(rf, warn = FALSE)
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
