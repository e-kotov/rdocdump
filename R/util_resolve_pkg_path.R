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
    version <- parts[length(parts)]
    pkgname <- paste(parts[-length(parts)], collapse = "_")
    list(pkgname = pkgname, version = version)
  }

  # Helper function to determine extraction directory.
  get_extract_dir <- function(tar_path) {
    info <- parse_tarball_name(tar_path)
    if (!is.null(cache_path)) {
      file.path(cache_path, info$pkgname, info$version)
    } else {
      tempfile(paste0(info$pkgname, "_", info$version))
    }
  }

  if (file.exists(pkg)) {
    if (dir.exists(pkg)) {
      # Check if directory is a source package by looking for DESCRIPTION file
      if (file.exists(file.path(pkg, "DESCRIPTION"))) {
        # It is a source package
        return(list(
          pkg_path = pkg,
          extracted_path = NULL,
          tar_path = NULL,
          is_installed = FALSE
        ))
      } else {
        # No DESCRIPTION found -> assume it's an installed package name.
        return(list(
          pkg_path = pkg,
          extracted_path = NULL,
          tar_path = NULL,
          is_installed = TRUE,
          pkg_name = pkg
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
      extract_dir <- get_extract_dir(pkg)
      if (!dir.exists(extract_dir)) {
        dir.create(extract_dir, recursive = TRUE)
      }
      utils::untar(pkg, exdir = extract_dir)
      # Flatten extra top-level folder if necessary.
      subdirs <- list.dirs(extract_dir, recursive = FALSE, full.names = TRUE)
      if (length(subdirs) == 1L) {
        files <- list.files(
          subdirs[1],
          full.names = TRUE,
          all.files = TRUE,
          no.. = TRUE
        )
        file.copy(files, extract_dir, recursive = TRUE)
        unlink(subdirs[1], recursive = TRUE)
      }
      check_if_binary(extract_dir)
      return(list(
        pkg_path = extract_dir,
        extracted_path = extract_dir,
        tar_path = NULL,
        is_installed = FALSE
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
      return(list(
        pkg_path = pkg_found,
        extracted_path = NULL,
        tar_path = NULL,
        is_installed = TRUE,
        pkg_name = pkg
      ))
    } else {
      message("Fetching package source from CRAN...")
      dest_dir <- if (!is.null(cache_path)) cache_path else tempdir()
      if (!dir.exists(dest_dir)) {
        dir.create(dest_dir, recursive = TRUE)
      }
      # Warn if repos contains known problematic URLs.
      if (
        any(grepl("posit\\.co|r-universe\\.dev", repos, ignore.case = TRUE))
      ) {
        warning(
          "Using a repository URL from posit.co or r-universe.dev may result ",
          "in pre-built binaries being downloaded instead of the package source."
        )
      }

      if (!is.null(version)) {
        # Construct URL for a specific version.
        repo_url <- repos[1] # Use the first repo.
        tar_filename <- paste0(pkg, "_", version, ".tar.gz")
        # Try archive first.
        url <- file.path(repo_url, "src/contrib/Archive", pkg, tar_filename)
        # Try downloading.
        res <- try(
          suppressWarnings(
            utils::download.file(
              url,
              destfile = file.path(dest_dir, tar_filename),
              mode = "wb",
              quiet = TRUE
            )
          ),
          silent = TRUE
        )
        # If archive fails, try main contrib.
        if (inherits(res, "try-error") || res != 0) {
          url <- file.path(repo_url, "src/contrib", tar_filename)
          res <- try(
            suppressWarnings(
              utils::download.file(
                url,
                destfile = file.path(dest_dir, tar_filename),
                mode = "wb",
                quiet = TRUE
              )
            ),
            silent = TRUE
          )
        }
        if (inherits(res, "try-error") || res != 0) {
          stop(paste(
            "Could not download package",
            pkg,
            "version",
            version,
            "from",
            url
          ))
        }
        dp <- matrix(
          c(tar_filename, file.path(dest_dir, tar_filename)),
          nrow = 1
        )
      } else {
        dp <- utils::download.packages(
          pkg,
          destdir = dest_dir,
          type = "source",
          repos = repos
        )
      }

      if (nrow(dp) < 1L) {
        stop("Package not found on CRAN.")
      }
      archive <- dp[, 2]
      base_name <- basename(archive)
      # If cache_path is provided, move the archive there.
      if (!is.null(cache_path)) {
        dest_archive <- file.path(cache_path, base_name)
        file.rename(archive, dest_archive)
        archive <- dest_archive
      }
      extract_dir <- get_extract_dir(archive)
      if (!dir.exists(extract_dir)) {
        dir.create(extract_dir, recursive = TRUE)
      }
      utils::untar(archive, exdir = extract_dir)
      subdirs <- list.dirs(extract_dir, recursive = FALSE, full.names = TRUE)
      if (length(subdirs) == 1L) {
        files <- list.files(
          subdirs[1],
          full.names = TRUE,
          all.files = TRUE,
          no.. = TRUE
        )
        file.copy(files, extract_dir, recursive = TRUE)
        unlink(subdirs[1], recursive = TRUE)
      }
      check_if_binary(extract_dir)
      return(list(
        pkg_path = extract_dir,
        extracted_path = extract_dir,
        tar_path = archive,
        is_installed = FALSE
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
    length(list.files(help_dir, pattern = "\\.(rdx|rdb)$", ignore.case = TRUE)) > 0
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
