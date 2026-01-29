#' Resolve the path to a package directory or tarball
#' @description
#' This function resolves the path to a package directory or tarball, handling both installed packages and source packages from CRAN.
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

  # Helper function to find the package root (containing DESCRIPTION) within an extracted directory.
  # This avoids the need to move/rename files and robustly handles wrapper directories.
  find_pkg_root <- function(path) {
    # Check if the path itself is the root
    if (file.exists(file.path(path, "DESCRIPTION"))) {
      return(path)
    }

    # Check immediate subdirectories (to handle wrapper folders like pkgname/ or user-repo-sha/)
    contents <- list.files(path, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    dirs <- contents[dir.exists(contents)]

    # Ignore hidden directories like .git or .github
    dirs <- dirs[!startsWith(basename(dirs), ".")]

    # If there is exactly one subdirectory, check inside it
    if (length(dirs) == 1L) {
      subdir <- dirs[[1]]
      if (file.exists(file.path(subdir, "DESCRIPTION"))) {
        return(subdir)
      }
    }

    # If not found, return the original path and let downstream functions fail/warn
    return(path)
  }

  if (file.exists(pkg)) {
    if (dir.exists(pkg)) {
      # Check if directory is a source package by looking for Rd files in "man/"
      man_dir <- file.path(pkg, "man")
      rd_files <- if (dir.exists(man_dir)) {
        list.files(man_dir, pattern = "\\.Rd$", full.names = TRUE)
      } else {
        character(0)
      }
      if (length(rd_files) > 0) {
        # It is a source package
        return(list(
          pkg_path = pkg,
          extracted_path = NULL,
          tar_path = NULL,
          is_installed = FALSE
        ))
      } else {
        # No .Rd files found in "man/" -> assume it's an installed package.
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
          "The specified file is not a recognized package archive (expected extension .tar.gz)."
        )
      }
      extract_dir <- get_extract_dir(pkg)
      if (!dir.exists(extract_dir)) {
        dir.create(extract_dir, recursive = TRUE)
      }
      utils::untar(pkg, exdir = extract_dir)

      pkg_root <- find_pkg_root(extract_dir)

      return(list(
        pkg_path = pkg_root,
        extracted_path = extract_dir,
        tar_path = NULL,
        is_installed = FALSE
      ))
    }
  } else {
    # pkg is not an existing file/directory: treat it as a package name or remote.

    # Heuristics to distinguish non-existent local paths from remote specifications.
    # If it resembles a path (absolute, relative, home, Windows drive, or a path
    # with separators plus a file extension), treat it as a missing local path
    # rather than a remote package specification.
    likely_path_prefix <- grepl("^(/|\\./|\\.\\./|~|\\\\|[A-Za-z]:[/\\\\])", pkg)
    has_path_sep <- grepl("[/\\\\]", pkg)
    has_extension <- grepl("\\.[A-Za-z0-9]+$", basename(pkg))
    is_likely_local_path <- likely_path_prefix || (has_path_sep && has_extension)
    if (is_likely_local_path) {
      stop("The specified path does not exist: ", pkg)
    }

    # Check if pkg is a remote specification (contains /) or uses "type::pkg" syntax.
    # Note: Valid CRAN package names cannot contain slashes.
    is_likely_remote <- (grepl("/", pkg) && !likely_path_prefix) || grepl("::", pkg)

    if (is_likely_remote) {
      if (!requireNamespace("pak", quietly = TRUE)) {
        stop(
          "The 'pak' package is required to download packages from remote sources (e.g., GitHub, GitLab).\n",
          "Please install it using install.packages('pak')."
        )
      }

      # Handle version parameter if provided
      pkg_for_download <- pkg
      if (!is.null(version) && nzchar(version) && !grepl("@", pkg, fixed = TRUE)) {
        pkg_for_download <- paste0(pkg, "@", version)
      }

      message("Fetching package source from remote: ", pkg_for_download, " ...")
      dest_dir <- if (!is.null(cache_path)) cache_path else tempdir()
      if (!dir.exists(dest_dir)) {
        dir.create(dest_dir, recursive = TRUE)
      }

      dl_info <- tryCatch(
        pak::pkg_download(pkg_for_download, dest_dir = dest_dir, dependencies = FALSE),
        error = function(e) {
          msg <- conditionMessage(e)
          prefix <- "Failed to download package from remote:"
          # Avoid double prefixing if the message already has it (though pak errors usually don't)
          if (grepl(prefix, msg, fixed = TRUE)) {
            stop(msg)
          } else {
            stop(paste(prefix, msg))
          }
        }
      )

      if (is.null(dl_info) || nrow(dl_info) < 1) {
        stop("Failed to download package from remote (no file returned).")
      }

      archive <- dl_info$fulltarget[1]

      extract_dir <- tryCatch(
        get_extract_dir(archive),
        error = function(e) tempfile("pak_extract")
      )

      if (!dir.exists(extract_dir)) {
        dir.create(extract_dir, recursive = TRUE)
      }
      utils::untar(archive, exdir = extract_dir)

      pkg_root <- find_pkg_root(extract_dir)

      return(list(
        pkg_path = pkg_root,
        extracted_path = extract_dir,
        tar_path = archive,
        is_installed = FALSE
      ))
    }

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
          "Using a repository URL from posit.co or r-universe.dev may result in pre-built binaries being downloaded instead of the package source."
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

      pkg_root <- find_pkg_root(extract_dir)

      return(list(
        pkg_path = pkg_root,
        extracted_path = extract_dir,
        tar_path = archive,
        is_installed = FALSE
      ))
    }
  }
}
