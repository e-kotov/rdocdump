resolve_pkg_path <- function(pkg, cache_path = NULL, force_fetch = FALSE) {
  if (!is.character(pkg) || length(pkg) != 1L) {
    stop("Argument 'pkg' must be a single character string.")
  }

  # Helper function to parse tarball filename into package name and version.
  parse_tarball_name <- function(tar_path) {
    base_name <- basename(tar_path) # e.g., "rJavaEnv_0.2.2.tar.gz"
    folder_name <- sub("\\.tar\\.gz$", "", base_name)
    parts <- strsplit(folder_name, "_")[[1]]
    if (length(parts) < 2) {
      stop(
        "Tarball filename does not conform to the expected pattern 'pkgname_version.tar.gz'."
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
      # pkg is a directory; check for a DESCRIPTION file.
      if (!file.exists(file.path(pkg, "DESCRIPTION"))) {
        stop(
          "The specified directory does not appear to be a package source (missing DESCRIPTION file)."
        )
      }
      return(list(pkg_path = pkg, extracted_path = NULL, tar_path = NULL))
    } else {
      # pkg is a file; assume it is a tar.gz archive.
      if (!grepl("\\.tar\\.gz$", pkg)) {
        stop(
          "The specified file is not a recognized package archive (expected extension .tar.gz)."
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

      return(list(
        pkg_path = extract_dir,
        extracted_path = extract_dir,
        tar_path = NULL
      ))
    }
  } else {
    # pkg is not an existing file/directory: treat it as a package name.
    # If force_fetch is TRUE, ignore any locally installed package.
    pkg_found <- if (!force_fetch)
      tryCatch(find.package(pkg), error = function(e) NULL) else NULL
    if (!is.null(pkg_found)) {
      return(list(pkg_path = pkg_found, extracted_path = NULL, tar_path = NULL))
    } else {
      message("Fetching package source from CRAN...")
      dest_dir <- if (!is.null(cache_path)) cache_path else tempdir()
      dp <- utils::download.packages(pkg, destdir = dest_dir, type = "source")
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

      return(list(
        pkg_path = extract_dir,
        extracted_path = extract_dir,
        tar_path = archive
      ))
    }
  }
}
