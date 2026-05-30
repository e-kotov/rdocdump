#' Resolve Remote Package References (GitHub, GitLab, Bioconductor, etc.)
#'
#' @description
#' Downloads package source from remote repositories without installing the
#' package. Uses the `pak` package for downloading.
#'
#' If `pak` fails to resolve the reference (e.g., because the R package is in a
#' subdirectory and no `subdir` was provided), the function automatically
#' falls back to downloading the full repository and scanning for the
#' shallowest directory containing a `DESCRIPTION` file.
#'
#' @param pkg_ref A character string specifying the remote package reference.
#'   Supports any format supported by `pak`. See `?pak::pak_package_sources`
#'   for a full list of supported formats. Examples:
#'   - `"user/repo"` - GitHub shorthand (default)
#'   - `"github::user/repo"` - Explicit GitHub
#'   - `"gitlab::user/repo"` - GitLab
#'   - `"bioc::pkgname"` - Bioconductor
#'   - `"git::https://..."` - Arbitrary Git URL
#'   - `"bitbucket::user/repo"` - Bitbucket (translated to `git::`)
#'   - `"user/repo@ref"` - Specific commit, branch, or tag
#'   - `"user/repo/subdir"` - Package in subdirectory
#'
#' @details
#' The auto-discovery mechanism uses two fallback tiers if `pak` resolution
#' fails:
#' 1. **Archive Download:** Attempts to download a `.tar.gz` archive of the
#'    repository for known hosts (GitHub, GitLab, Bitbucket).
#' 2. **Git Clone:** Uses `git clone --depth 1` for arbitrary Git URLs or if
#'    the archive download fails (requires system `git`).
#'
#' Once downloaded, it recursively searches for `DESCRIPTION` files and selects
#' the one closest to the repository root.
#'
#' @param cache_path Optional path to cache directory. If NULL, uses temp
#'   directory.
#'
#' @return A list containing:
#'   - `pkg_path`: Path to the package directory
#'   - `extracted_path`: Path to the extracted bundle
#'   - `tar_path`: Path to the downloaded tarball
#'   - `is_installed`: FALSE (always FALSE for remote packages)
#'   - `pkg_name`: Package name reported by pak (or repo slug fallback)
#'   - `pkg_version`: Package version reported by pak, if available
#'   - `remote_info`: Parsed remote reference information
#'
#' @keywords internal
#'
resolve_remote_pkg <- function(pkg_ref, cache_path = NULL) {
  if (!is.character(pkg_ref) || length(pkg_ref) != 1L) {
    stop("Argument 'pkg_ref' must be a single character string.")
  }

  # Check if pak package is installed
  if (!requireNamespace("pak", quietly = TRUE)) {
    stop(
      "The 'pak' package is required to download from remote repositories. ",
      "Please install it with: install.packages('pak')"
    )
  }

  # Parse the remote reference
  parsed <- parse_remote_ref(pkg_ref)

  message(sprintf(
    "Downloading package from %s (%s)...",
    parsed$type,
    remote_display_name(parsed)
  ))

  pak_ref <- build_pak_remote_ref(parsed)

  # Setup cache directory
  dest_dir <- get_remote_cache_dir(parsed, cache_path)

  # Ensure destination directory exists before downloading
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Download the bundle (NO INSTALLATION - just download)
  # pak::pkg_download creates a src/contrib structure
  if (!requireNamespace("withr", quietly = TRUE)) {
    stop(
      "The 'withr' package is required to handle pak downloads safely. ",
      "Please install it with: install.packages('withr')"
    )
  }
  if (Sys.getenv("R_USER_CACHE_DIR") == "") {
    withr::local_envvar(c(R_USER_CACHE_DIR = tempfile("pak-cache-")))
  }

  dl_info <- tryCatch(
    pak::pkg_download(
      pak_ref,
      dest_dir = dest_dir,
      platforms = "source",
      dependencies = FALSE
    ),
    error = function(e) {
      msg <- conditionMessage(e)
      # If pak fails to resolve (likely because it expects a DESCRIPTION at
      # the root or specific subdir), we fall back to downloading the entire
      # repository and scanning for packages.
      if (grepl("Resolution has errors|Cannot start downloading", msg, ignore.case = TRUE)) {
        return(NULL)
      }
      stop(sprintf(
        "Failed to download remote package '%s': %s",
        pkg_ref,
        msg
      ))
    }
  )

  if (is.null(dl_info)) {
    message("pak resolution failed. Attempting full repository download for auto-discovery...")
    bundle_path <- fallback_download_repo(parsed, dest_dir)
    pkg_name_from_pak <- NA_character_
    pkg_version_from_pak <- NA_character_
  } else {
    bundle_path <- select_pak_download_archive(dl_info, dest_dir, pak_ref)
    direct_row <- find_pak_target_row(dl_info, pak_ref)
    pkg_name_from_pak <- pak_row_value(direct_row, "package")
    pkg_version_from_pak <- pak_row_value(direct_row, "version")
  }

  # Extract
  extract_dir <- file.path(dest_dir, "extracted")
  if (dir.exists(extract_dir)) {
    unlink(extract_dir, recursive = TRUE)
  }
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)

  success <- FALSE
  on.exit({
    if (!success && dir.exists(extract_dir)) {
      unlink(extract_dir, recursive = TRUE)
    }
  }, add = TRUE)

  # Handle both tarball and git-clone directory cases
  if (dir.exists(bundle_path)) {
    # It's a directory from git clone
    files_to_copy <- list.files(bundle_path, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    file.copy(files_to_copy, extract_dir, recursive = TRUE)
  } else {
    first_bytes <- readBin(bundle_path, raw(), n = 2L)
    if (length(first_bytes) == 2L &&
        identical(first_bytes, as.raw(c(0x50, 0x4b)))) {
      utils::unzip(bundle_path, exdir = extract_dir)
    } else {
      res <- utils::untar(bundle_path, exdir = extract_dir, tar = "internal")
      if (!identical(as.integer(res), 0L)) {
        stop(sprintf(
          "Extraction failed: utils::untar() returned non-zero status code %s.",
          res
        ))
      }
    }
  }

  # Flatten extra top-level folder if necessary (common in GitHub/GitLab
  # bundles)
  flatten_extracted_dir(extract_dir)

  # Find package directory (handle subdirectories)
  pkg_path <- find_pkg_dir(extract_dir, parsed$subdir)

  # Validate it's a package
  if (!file.exists(file.path(pkg_path, "DESCRIPTION"))) {
    stop(
      "Downloaded package does not contain a valid R package ",
      "(no DESCRIPTION found)."
    )
  }

  check_if_binary(pkg_path)

  # Prefer the DESCRIPTION on disk for definitive pkg_name/pkg_version,
  # falling back to whatever pak reported and finally to the repo slug.
  desc <- read_pkg_meta(pkg_path)
  pkg_name <- desc$pkg_name
  if (is.na(pkg_name)) pkg_name <- pkg_name_from_pak
  if (is.na(pkg_name)) pkg_name <- parsed$repo
  pkg_version <- desc$pkg_version
  if (is.na(pkg_version)) pkg_version <- pkg_version_from_pak

  success <- TRUE
  list(
    pkg_path = pkg_path,
    extracted_path = extract_dir,
    tar_path = bundle_path,
    is_installed = FALSE,
    pkg_name = pkg_name,
    pkg_version = if (is.na(pkg_version)) NULL else pkg_version,
    remote_info = parsed
  )
}

#' Parse Remote Reference String
#'
#' Supports any format supported by `pak`. See `?pak::pak_package_sources`
#' for details.
#'
#' Ambiguous web URLs (e.g. branch names like `feat/foo/pkg`) are best
#' supplied as `user/repo@ref/subdir`; the URL heuristic is intentionally
#' limited.
#'
#' @param ref Character string reference
#' @return List with components: type, user, repo, ref, subdir
#' @keywords internal
parse_remote_ref <- function(ref) {
  original_ref <- ref

  # Handle web URLs
  if (grepl("^https?://", ref)) {
    return(parse_remote_url(ref))
  }

  # Check for explicit type prefix (type::)
  type_pattern <- "^([a-zA-Z0-9]+)::"
  type_match <- regexpr(type_pattern, ref, perl = TRUE)

  if (type_match[1] != -1) {
    type <- regmatches(ref, type_match)
    type <- sub("::$", "", type)
    ref <- sub(type_pattern, "", ref)
  } else {
    # Default to GitHub for "user/repo" format
    type <- "github"
  }

  # `git::` refs come in URL form and are passed through verbatim.
  if (type == "git") {
    return(list(
      type = "git",
      user = NA_character_,
      repo = NA_character_,
      ref = NULL,
      subdir = NULL,
      original = original_ref
    ))
  }

  # `bioc::` refs are single-segment package names with an optional @ref.
  if (type == "bioc") {
    ref_pattern <- "@([^/@]+)$"
    ref_match <- regexpr(ref_pattern, ref, perl = TRUE)
    commit_ref <- NULL
    if (ref_match[1] != -1) {
      commit_ref <- regmatches(ref, ref_match)
      commit_ref <- sub("^@", "", commit_ref)
      ref <- sub(ref_pattern, "", ref)
    }
    if (!nzchar(ref) || grepl("/", ref)) {
      stop(sprintf(
        "Invalid Bioconductor reference '%s'. Expected: 'bioc::pkgname'",
        ref
      ))
    }
    return(list(
      type = "bioc",
      user = NA_character_,
      repo = ref,
      ref = commit_ref,
      subdir = NULL,
      original = original_ref
    ))
  }

  # Extract ref (commit/branch/tag) if present
  ref_pattern <- "@([^/@]+)$"
  ref_match <- regexpr(ref_pattern, ref, perl = TRUE)
  commit_ref <- NULL
  if (ref_match[1] != -1) {
    commit_ref <- regmatches(ref, ref_match)
    commit_ref <- sub("^@", "", commit_ref)
    ref <- sub(ref_pattern, "", ref)
  }

  # Split remaining path
  parts <- strsplit(ref, "/")[[1]]

  # For transparent pass-through of unknown types (like url::),
  # we don't strictly require user/repo parts.
  if (length(parts) < 2) {
    if (type %in% c("github", "gitlab", "bitbucket")) {
      stop(sprintf(
        paste0(
          "Invalid remote reference '%s'. Expected format: 'user/repo' ",
          "or 'user/repo/subdir'"
        ),
        ref
      ))
    }
    # Pass through others (like url::) with original ref
    return(list(
      type = type,
      user = NA_character_,
      repo = ref,
      ref = commit_ref,
      subdir = NULL,
      original = original_ref
    ))
  }

  user <- parts[1]
  repo <- parts[2]

  # Everything after repo is subdirectory
  subdir <- NULL
  if (length(parts) > 2) {
    subdir <- paste(parts[3:length(parts)], collapse = "/")
  }

  list(
    type = type,
    user = user,
    repo = repo,
    ref = commit_ref,
    subdir = subdir,
    original = original_ref
  )
}

#' Build a pak Remote Reference
#'
#' @param parsed Parsed reference from `parse_remote_ref()`.
#' @return A package reference suitable for `pak::pkg_download()`.
#' @keywords internal
#' @noRd
build_pak_remote_ref <- function(parsed) {
  pak_ref <- switch(
    parsed$type,
    github = {
      ref <- sprintf("github::%s/%s", parsed$user, parsed$repo)
      if (!is.null(parsed$subdir)) {
        ref <- paste0(ref, "/", parsed$subdir)
      }
      if (!is.null(parsed$ref)) {
        ref <- paste0(ref, "@", parsed$ref)
      }
      ref
    },
    gitlab = {
      ref <- sprintf("gitlab::%s/%s", parsed$user, parsed$repo)
      if (!is.null(parsed$subdir)) {
        # pak requires /-/ for GitLab subdirectories.
        # Ensure we don't double up if it's already present.
        clean_subdir <- sub("^-/", "", parsed$subdir)
        ref <- paste0(ref, "/-/", clean_subdir)
      }
      if (!is.null(parsed$ref)) {
        ref <- paste0(ref, "@", parsed$ref)
      }
      ref
    },
    bioc = {
      # bioc:: refs are built from repo and ref to ensure correctness
      # even if original was missing the prefix.
      ref <- sprintf("bioc::%s", parsed$repo)
      if (!is.null(parsed$ref)) {
        ref <- paste0(ref, "@", parsed$ref)
      }
      ref
    },
    git = {
      # For git:: we just use the original untouched string if it has the prefix
      if (grepl("^git::", parsed$original)) {
        parsed$original
      } else {
        ref <- sprintf("git::%s", parsed$original)
        if (!is.null(parsed$ref)) {
          ref <- paste0(ref, "@", parsed$ref)
        }
        ref
      }
    },
    bitbucket = {
      # Backward compatibility: translate bitbucket:: to git:: URL
      ref <- sprintf("git::https://bitbucket.org/%s/%s.git", parsed$user, parsed$repo)
      if (!is.null(parsed$subdir)) {
        warning("Subdirectories are not supported for Bitbucket legacy references. Using repository root.")
      }
      if (!is.null(parsed$ref)) {
        ref <- paste0(ref, "@", parsed$ref)
      }
      ref
    },
    # Transparent pass-through for other pak types
    if (grepl(paste0("^", parsed$type, "::"), parsed$original)) {
      parsed$original
    } else {
      ref <- sprintf("%s::%s", parsed$type, parsed$original)
      if (!is.null(parsed$ref)) {
        ref <- paste0(ref, "@", parsed$ref)
      }
      ref
    }
  )

  pak_ref
}

#' Build a human-readable label for a parsed remote
#' @keywords internal
#' @noRd
remote_display_name <- function(parsed) {
  if (parsed$type %in% c("bioc", "git") || is.na(parsed$user %||% NA)) {
    return(parsed$original %||% parsed$repo)
  }
  sprintf("%s/%s", parsed$user, parsed$repo)
}

#' Parse GitHub or GitLab Web URL
#'
#' @param url The full URL string
#' @return List with components: type, user, repo, ref, subdir
#' @keywords internal
parse_remote_url <- function(url) {
  # Remove trailing slash
  url <- sub("/$", "", url)

  type <- NULL
  if (grepl("github\\.com", url)) {
    type <- "github"
  } else if (grepl("gitlab\\.com", url)) {
    type <- "gitlab"
  } else {
    # For other URLs, treat as a generic git:: source for pak
    return(list(
      type = "git",
      user = NA_character_,
      repo = NA_character_,
      ref = NULL,
      subdir = NULL,
      original = url
    ))
  }

  # GitHub: https://github.com/user/repo/tree/ref/subdir
  # GitLab: https://gitlab.com/user/repo/-/tree/ref/subdir
  pattern <- if (type == "github") {
    "^https?://github\\.com/([^/]+)/([^/]+)(/tree/|/blob/)?(.*)$"
  } else {
    "^https?://gitlab\\.com/([^/]+)/([^/]+)(/-/tree/|/-/blob/)?(.*)$"
  }

  matches <- regexec(pattern, url)
  parts <- regmatches(url, matches)[[1]]

  if (length(parts) < 3) {
    stop(sprintf("Could not parse %s URL: %s", type, url))
  }

  user <- parts[2]
  repo <- parts[3]
  ref_and_subdir <- if (length(parts) >= 5) parts[5] else ""

  # If there's no ref/subdir part, we're done
  if (ref_and_subdir == "") {
    return(list(
      type = type,
      user = user,
      repo = repo,
      ref = NULL,
      subdir = NULL,
      original = url
    ))
  }

  # Heuristic to split ref and subdir. This is intentionally limited; for
  # ambiguous refs, prefer the explicit "user/repo@ref/subdir" form.
  ref_parts <- strsplit(ref_and_subdir, "/")[[1]]
  ref <- NULL
  subdir <- NULL

  # Common single-segment branches and version-tag-like first segments are
  # treated as a complete ref.
  common_branches <- c(
    "main", "master", "develop", "dev", "trunk", "HEAD"
  )
  conventional_prefixes <- c(
    "feature", "release", "hotfix", "patch",
    "chore", "fix", "feat", "refactor", "test", "docs",
    "ci", "build", "perf", "style", "revert",
    "renovate", "dependabot"
  )
  is_version_tag <- grepl("^v?\\d+(\\.\\d+)*([._-][A-Za-z0-9.+-]+)?$", ref_parts[1])

  if (ref_parts[1] %in% common_branches || is_version_tag) {
    ref <- ref_parts[1]
    if (length(ref_parts) > 1) {
      subdir <- paste(ref_parts[2:length(ref_parts)], collapse = "/")
    }
  } else if (length(ref_parts) >= 2 &&
             ref_parts[1] %in% conventional_prefixes) {
    # Pattern: <prefix>/<branch-name>[/subdir]
    ref <- paste(ref_parts[1:2], collapse = "/")
    if (length(ref_parts) > 2) {
      subdir <- paste(ref_parts[3:length(ref_parts)], collapse = "/")
    }
  } else {
    # Fallback: assume first part is ref
    ref <- ref_parts[1]
    if (length(ref_parts) > 1) {
      subdir <- paste(ref_parts[2:length(ref_parts)], collapse = "/")
    }
  }

  list(
    type = type,
    user = user,
    repo = repo,
    ref = ref,
    subdir = subdir,
    original = url
  )
}

#' Get Cache Directory for Remote Package
#'
#' @param parsed Parsed reference
#' @param cache_path Base cache path
#' @return Path to cache directory
#' @keywords internal
get_remote_cache_dir <- function(parsed, cache_path) {
  if (!is.null(cache_path)) {
    ref_suffix <- parsed$ref %||% "HEAD"
    subdir_suffix <- if (!is.null(parsed$subdir)) {
      gsub("/", "_", parsed$subdir)
    } else {
      ""
    }
    user_part <- if (is.na(parsed$user %||% NA)) "" else parsed$user

    dir_name <- paste(
      parsed$type,
      user_part,
      parsed$repo,
      ref_suffix,
      subdir_suffix,
      sep = "_"
    )
    dir_name <- gsub("_+", "_", dir_name)
    dir_name <- gsub("_$", "", dir_name)

    file.path(cache_path, "pak", dir_name)
  } else {
    tempfile(pattern = paste0("remote_", parsed$type, "_"))
  }
}

#' Find Package Directory Within Extracted Bundle
#'
#' @param extract_dir Directory where bundle was extracted
#' @param subdir Optional subdirectory path
#' @return Path to package directory
#' @keywords internal
find_pkg_dir <- function(extract_dir, subdir = NULL) {
  if (!is.null(subdir)) {
    # Use specified subdirectory
    pkg_path <- file.path(extract_dir, subdir)
    if (!dir.exists(pkg_path)) {
      stop(sprintf("Specified subdirectory '%s' not found in package", subdir))
    }
    return(pkg_path)
  }

  # Auto-detect: look for DESCRIPTION file
  # Recursively search for all DESCRIPTION files to find the primary package
  desc_files <- list.files(
    extract_dir,
    pattern = "^DESCRIPTION$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(desc_files) == 0) {
    return(extract_dir)
  }

  # Pick the one with the shallowest depth to avoid tests/testthat/DESCRIPTION
  # or deep sub-packages unless it's the only one.
  # Count depth by number of path separators
  depths <- vapply(strsplit(dirname(desc_files), "[/\\\\]"), length, integer(1))
  best_idx <- which.min(depths)

  dirname(desc_files[best_idx])
}

#' Check if String is a Remote Package Reference
#'
#' @param pkg Character string to check
#' @return TRUE if it looks like a remote reference
#' @keywords internal
#'
is_remote_reference <- function(pkg) {
  if (!is.character(pkg) || length(pkg) != 1L) {
    return(FALSE)
  }

  # Check for explicit type prefix (any type::)
  if (grepl("^[a-zA-Z0-9]+::", pkg)) {
    return(TRUE)
  }

  # Check for web URLs
  if (grepl("^https?://", pkg)) {
    return(TRUE)
  }

  # Check for user/repo pattern (GitHub shorthand)
  # Must have exactly one / with valid characters on both sides.
  # We use a slightly more restrictive regex to avoid matching local paths.
  # GitHub usernames can be alphanumeric with hyphens.
  # Repo names can be alphanumeric with hyphens, underscores, and dots.
  if (grepl("^[a-zA-Z0-9-]{1,39}/[a-zA-Z0-9._-]{1,100}(@|/|$)", pkg)) {
    # Make sure it's not an absolute or relative path
    if (!grepl("^(/|[A-Za-z]:|\\.|~)", pkg)) {
      return(TRUE)
    }
  }

  FALSE
}

#' Fallback Download for Auto-Discovery
#' @keywords internal
#' @noRd
fallback_download_repo <- function(parsed, dest_dir) {
  # Tier 1: Try tarball for known hosts
  tarball <- download_tarball(parsed, dest_dir)
  if (!is.null(tarball) && file.exists(tarball)) {
    return(tarball)
  }

  # Tier 2: Try git clone
  cloned <- git_clone_repo(parsed, dest_dir)
  if (!is.null(cloned) && dir.exists(cloned)) {
    return(cloned)
  }

  stop(sprintf(
    "Failed to download repository for auto-discovery: %s",
    remote_display_name(parsed)
  ))
}

#' Download Repository Tarball
#' @keywords internal
#' @noRd
download_tarball <- function(parsed, dest_dir) {
  ref <- parsed$ref %||% "HEAD"
  url <- switch(
    parsed$type,
    github = sprintf(
      "https://github.com/%s/%s/archive/%s.tar.gz",
      parsed$user, parsed$repo, ref
    ),
    gitlab = sprintf(
      "https://gitlab.com/%s/%s/-/archive/%s/%s-%s.tar.gz",
      parsed$user, parsed$repo, ref, parsed$repo, ref
    ),
    bitbucket = sprintf(
      "https://bitbucket.org/%s/%s/get/%s.tar.gz",
      parsed$user, parsed$repo, ref
    ),
    NULL
  )

  if (is.null(url)) return(NULL)

  # Support authentication for private GitHub repositories
  headers <- NULL
  if (parsed$type == "github") {
    token <- Sys.getenv("GITHUB_PAT", Sys.getenv("GITHUB_TOKEN", ""))
    if (nzchar(token)) {
      headers <- c(Authorization = paste("token", token))
    }
  }

  destfile <- file.path(dest_dir, sprintf("fallback_repo_%s.tar.gz", parsed$repo))
  res <- try(
    utils::download.file(
      url,
      destfile = destfile,
      mode = "wb",
      quiet = TRUE,
      headers = headers
    ),
    silent = TRUE
  )

  if (inherits(res, "try-error") || res != 0) {
    if (file.exists(destfile)) unlink(destfile)
    return(NULL)
  }

  destfile
}

#' Clone Repository via Git
#' @keywords internal
#' @noRd
git_clone_repo <- function(parsed, dest_dir) {
  git_bin <- Sys.which("git")
  if (git_bin == "") {
    return(NULL)
  }

  url <- if (parsed$type == "git") {
    parsed$original
  } else if (!is.na(parsed$user %||% NA)) {
    host <- switch(
      parsed$type,
      github = "github.com",
      gitlab = "gitlab.com",
      bitbucket = "bitbucket.org",
      NULL
    )
    if (is.null(host)) return(NULL)
    sprintf("https://%s/%s/%s.git", host, parsed$user, parsed$repo)
  } else {
    return(NULL)
  }

  clone_dir <- file.path(dest_dir, "fallback_clone")
  if (dir.exists(clone_dir)) unlink(clone_dir, recursive = TRUE)

  args <- c("clone", "--depth", "1")
  if (!is.null(parsed$ref)) {
    args <- c(args, "--branch", parsed$ref)
  }
  args <- c(args, url, clone_dir)

  res <- system2(git_bin, args, stdout = FALSE, stderr = FALSE)
  if (res != 0) {
    if (dir.exists(clone_dir)) unlink(clone_dir, recursive = TRUE)
    return(NULL)
  }

  clone_dir
}
