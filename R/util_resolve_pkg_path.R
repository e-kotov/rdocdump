#' Resolve the path to a package directory or tarball
#' @description
#' This function resolves the path to a package directory or tarball, handling
#' both installed packages and source packages from CRAN.
#' @inheritParams rdd_to_txt
#' @return A list containing:
#' - `pkg_path`: Path to the package directory or tarball.
#' - `extracted_path`: Path to the extracted package directory (if applicable).
#' - `tar_path`: Path to the tarball if it was downloaded.
#' - `is_installed`: Logical indicating if the package is installed.
#' - `pkg_name`: Package name (always populated when known).
#' - `pkg_version`: Package version (NULL if not known).
#'
#' @keywords internal
#'
resolve_pkg_path <- function(
  pkg,
  cache_path = NULL,
  force_fetch = FALSE,
  version = NULL,
  repos = getOption("rdocdump.repos", getOption("repos"))
) {
  if (!is.character(pkg) || length(pkg) != 1L) {
    stop("Argument 'pkg' must be a single character string.")
  }

  # Check if it's a remote reference (GitHub, GitLab, etc.)
  if (is_remote_reference(pkg)) {
    message("Fetching package source from remote repository...")
    return(resolve_remote_pkg(pkg, cache_path))
  }

  # Helper function to parse tarball filename into package name and version.
  parse_tarball_name <- function(tar_path) {
    base_name <- basename(tar_path) # e.g., "rJavaEnv_0.2.2.tar.gz"
    folder_name <- sub("\\.tar\\.gz$", "", base_name)
    parts <- strsplit(folder_name, "_")[[1]]
    if (length(parts) < 2) {
      stop(
        "Tarball filename does not conform to the expected pattern ",
        "'pkgname_version.tar.gz'."
      )
    }
    version_part <- parts[length(parts)]
    pkgname <- paste(parts[-length(parts)], collapse = "_")
    list(pkgname = pkgname, version = version_part)
  }

  # Helper function to determine extraction directory.
  get_extract_dir <- function(pkgname, pkgversion) {
    if (!is.null(cache_path)) {
      file.path(cache_path, pkgname, pkgversion)
    } else {
      tempfile(paste0(pkgname, "_", pkgversion))
    }
  }

  if (file.exists(pkg)) {
    if (dir.exists(pkg)) {
      # Check if directory is a source package by looking for DESCRIPTION file
      if (file.exists(file.path(pkg, "DESCRIPTION"))) {
        # It is a source package
        desc <- read_pkg_meta(pkg)
        return(list(
          pkg_path = pkg,
          extracted_path = NULL,
          tar_path = NULL,
          is_installed = FALSE,
          pkg_name = if (is.na(desc$pkg_name)) NULL else desc$pkg_name,
          pkg_version = if (is.na(desc$pkg_version)) NULL else desc$pkg_version
        ))
      } else {
        # No DESCRIPTION found -> assume it's an installed package name.
        return(list(
          pkg_path = pkg,
          extracted_path = NULL,
          tar_path = NULL,
          is_installed = TRUE,
          pkg_name = pkg,
          pkg_version = NULL
        ))
      }
    } else {
      # pkg is a file; assume it is a tar.gz archive.
      if (!grepl("\\.tar\\.gz$", pkg)) {
        stop(
          "The specified file is not a recognized package archive (expected ",
          "extension .tar.gz)."
        )
      }
      info <- parse_tarball_name(pkg)
      extract_dir <- get_extract_dir(info$pkgname, info$version)
      if (!dir.exists(extract_dir)) {
        dir.create(extract_dir, recursive = TRUE)
      }
      res <- utils::untar(pkg, exdir = extract_dir, tar = "internal")
      if (!identical(as.integer(res), 0L)) {
        stop(sprintf("Extraction failed: utils::untar() returned non-zero status code %s.", res))
      }
      flatten_extracted_dir(extract_dir)
      check_if_binary(extract_dir)
      desc <- read_pkg_meta(extract_dir)
      pkg_name <- if (!is.na(desc$pkg_name)) desc$pkg_name else info$pkgname
      pkg_version <- if (!is.na(desc$pkg_version)) {
        desc$pkg_version
      } else {
        info$version
      }
      return(list(
        pkg_path = extract_dir,
        extracted_path = extract_dir,
        tar_path = NULL,
        is_installed = FALSE,
        pkg_name = pkg_name,
        pkg_version = pkg_version
      ))
    }
  } else {
    # pkg is not an existing file/directory: treat it as a package name.
    # If force_fetch is TRUE, ignore any locally installed package.
    pkg_found <- if (!force_fetch) {
      tryCatch(find.package(pkg), error = function(e) NULL)
    } else {
      NULL
    }
    if (!is.null(pkg_found) && is.null(version)) {
      # Installed package found.
      desc <- read_pkg_meta(pkg_found)
      return(list(
        pkg_path = pkg_found,
        extracted_path = NULL,
        tar_path = NULL,
        is_installed = TRUE,
        pkg_name = pkg,
        pkg_version = if (is.na(desc$pkg_version)) NULL else desc$pkg_version
      ))
    } else {
      # Check if pak package is installed
      if (!requireNamespace("pak", quietly = TRUE)) {
        stop(
          "The 'pak' package is required to download package sources. ",
          "Please install it with: install.packages('pak')"
        )
      }

      message("Fetching package source from CRAN...")
      dest_dir <- if (!is.null(cache_path)) cache_path else tempdir()
      if (!dir.exists(dest_dir)) {
        dir.create(dest_dir, recursive = TRUE)
      }

      # Pre-emptive hint for Linux users who point at binary-serving repos.
      if (
        any(grepl("posit\\.co|r-universe\\.dev", repos, ignore.case = TRUE))
      ) {
        message(
          "Note: 'repos' includes posit.co or r-universe.dev, which may ",
          "serve pre-built binaries on Linux. If extraction fails with a ",
          "binary-package error, switch to a source-serving repository."
        )
      }

      pkg_ref <- if (!is.null(version)) paste0(pkg, "@", version) else pkg

      if (!requireNamespace("withr", quietly = TRUE)) {
        stop(
          "The 'withr' package is required to scope the 'repos' option for ",
          "pak. Please install it with: install.packages('withr')"
        )
      }
      withr::local_options(list(repos = repos))
      if (Sys.getenv("R_USER_CACHE_DIR") == "") {
        withr::local_envvar(c(R_USER_CACHE_DIR = tempfile("pak-cache-")))
      }

      dl_info <- tryCatch(
        pak::pkg_download(
          pkg_ref,
          dest_dir = dest_dir,
          platforms = "source",
          dependencies = FALSE
        ),
        error = function(e) {
          stop(sprintf(
            "Could not download package '%s' from CRAN: %s",
            pkg_ref,
            conditionMessage(e)
          ))
        }
      )

      archive <- select_pak_download_archive(dl_info, dest_dir, pkg_ref)

      pkg_row <- find_pak_target_row(dl_info, pkg_ref)
      pkg_name_from_pak <- pak_row_value(pkg_row, "package")
      version_from_pak <- if (!is.null(version)) {
        version
      } else {
        pak_row_value(pkg_row, "version")
      }

      if (is.na(pkg_name_from_pak) || !nzchar(pkg_name_from_pak)) {
        stop(sprintf("pak did not return a package name for '%s'.", pkg_ref))
      }
      if (is.na(version_from_pak) || !nzchar(version_from_pak)) {
        stop(sprintf("pak did not return a package version for '%s'.", pkg_ref))
      }

      extract_dir <- get_extract_dir(pkg_name_from_pak, version_from_pak)
      if (!dir.exists(extract_dir)) {
        dir.create(extract_dir, recursive = TRUE)
      }

      success <- FALSE
      on.exit({
        if (!success && dir.exists(extract_dir)) {
          unlink(extract_dir, recursive = TRUE)
        }
      }, add = TRUE)

      res <- utils::untar(archive, exdir = extract_dir, tar = "internal")
      if (!identical(as.integer(res), 0L)) {
        stop(sprintf("Extraction failed: utils::untar() returned non-zero status code %s.", res))
      }
      flatten_extracted_dir(extract_dir)
      check_if_binary(extract_dir)
      
      success <- TRUE
      return(list(
        pkg_path = extract_dir,
        extracted_path = extract_dir,
        tar_path = archive,
        is_installed = FALSE,
        pkg_name = pkg_name_from_pak,
        pkg_version = version_from_pak
      ))
    }
  }
}

#' Check if a directory contains a pre-built binary package
#' @param pkg_dir Path to the package directory.
#' @return Logical indicating if it is a binary package.
#' @keywords internal
is_binary_pkg <- function(pkg_dir) {
  r_dir <- file.path(pkg_dir, "R")
  help_dir <- file.path(pkg_dir, "help")

  # Binary packages usually have .rdx and .rdb files in R/ and help/
  has_r_binaries <- if (dir.exists(r_dir)) {
    length(list.files(r_dir, pattern = "\\.(rdx|rdb)$", ignore.case = TRUE)) > 0
  } else {
    FALSE
  }

  has_help_binaries <- if (dir.exists(help_dir)) {
    length(
      list.files(help_dir, pattern = "\\.(rdx|rdb)$", ignore.case = TRUE)
    ) > 0
  } else {
    FALSE
  }

  # Binary packages always have a Meta directory
  has_meta <- dir.exists(file.path(pkg_dir, "Meta"))

  has_r_binaries || has_help_binaries || has_meta
}

#' Throw an error if the package is a pre-built binary
#' @param pkg_dir Path to the package directory.
#' @keywords internal
check_if_binary <- function(pkg_dir) {
  if (is_binary_pkg(pkg_dir)) {
    stop(
      "The downloaded package appears to be a pre-built binary rather than a ",
      "source package. This can happen when downloading from Posit Package ",
      "Manager (PPM) on Linux. Please ensure you are downloading a source ",
      "package (e.g., by checking your 'repos' option)."
    )
  }
}
