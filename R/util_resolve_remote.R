#' Resolve Remote Package References (GitHub, GitLab, Bitbucket)
#'
#' @description
#' Downloads package source from remote repositories (GitHub, GitLab, Bitbucket)
#' without installing the package. Uses the `remotes` package for downloading.
#'
#' @param pkg_ref A character string specifying the remote package reference.
#'   Supports formats like:
#'   - `"user/repo"` - GitHub shorthand (default)
#'   - `"github::user/repo"` - Explicit GitHub
#'   - `"gitlab::user/repo"` - GitLab
#'   - `"bitbucket::user/repo"` - Bitbucket
#'   - `"user/repo@ref"` - Specific commit, branch, or tag
#'   - `"user/repo/subdir"` - Package in subdirectory
#' @param cache_path Optional path to cache directory. If NULL, uses temp
#'   directory.
#'
#' @return A list containing:
#'   - `pkg_path`: Path to the package directory
#'   - `extracted_path`: Path to the extracted bundle
#'   - `tar_path`: Path to the downloaded tarball
#'   - `is_installed`: FALSE (always FALSE for remote packages)
#'   - `remote_info`: Parsed remote reference information
#'
#' @keywords internal
#'
resolve_remote_pkg <- function(pkg_ref, cache_path = NULL) {
  if (!is.character(pkg_ref) || length(pkg_ref) != 1L) {
    stop("Argument 'pkg_ref' must be a single character string.")
  }

  # Check if remotes package is installed
  if (!requireNamespace("remotes", quietly = TRUE)) {
    stop(
      "The 'remotes' package is required to download from remote repositories. ",
      "Please install it with: install.packages('remotes')"
    )
  }

  # Parse the remote reference
  parsed <- parse_remote_ref(pkg_ref)

  message(sprintf(
    "Downloading package from %s (%s/%s)...",
    parsed$type,
    parsed$user,
    parsed$repo
  ))

  # Create appropriate remote object
  remote <- create_remote(parsed)

  # Setup cache directory
  dest_dir <- get_remote_cache_dir(parsed, cache_path)

  # Ensure destination directory exists before downloading
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Download the bundle (NO INSTALLATION - just download)
  # remotes downloads to a temp file, so we need to move it to our cache
  temp_bundle <- tryCatch(
    remotes::remote_download(remote, quiet = TRUE),
    error = function(e) {
      stop(sprintf(
        "Failed to download remote package '%s': %s",
        pkg_ref,
        conditionMessage(e)
      ))
    }
  )

  # Move to our cache location
  bundle_path <- file.path(dest_dir, basename(temp_bundle))
  if (file.exists(bundle_path)) {
    file.remove(bundle_path)
  }
  file.copy(temp_bundle, bundle_path, overwrite = TRUE)
  # Clean up original temp file
  file.remove(temp_bundle)

  # Extract
  extract_dir <- file.path(dest_dir, "extracted")
  if (dir.exists(extract_dir)) {
    unlink(extract_dir, recursive = TRUE)
  }
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)

  utils::untar(bundle_path, exdir = extract_dir)

  # Flatten extra top-level folder if necessary (common in GitHub/GitLab
  # bundles)
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

  # Find package directory (handle subdirectories)
  pkg_path <- find_pkg_dir(extract_dir, parsed$subdir)

  # Validate it's a package
  if (!file.exists(file.path(pkg_path, "DESCRIPTION"))) {
    stop(sprintf(
      paste0(
        "Downloaded package does not contain a valid R package ",
        "(no DESCRIPTION found). "
      )
    ))
  }

  list(
    pkg_path = pkg_path,
    extracted_path = extract_dir,
    tar_path = bundle_path,
    is_installed = FALSE,
    pkg_name = parsed$repo,
    remote_info = parsed
  )
}

#' Parse Remote Reference String
#'
#' Supports formats:
#' - "user/repo" -> GitHub shorthand
#' - "github::user/repo" -> Explicit GitHub
#' - "gitlab::user/repo" -> GitLab
#' - "bitbucket::user/repo" -> Bitbucket
#' - "user/repo@ref" -> With commit/branch/tag
#' - "user/repo/subdir" -> With subdirectory
#' - "user/repo/subdir@ref" -> Combined
#' - "https://github.com/user/repo/tree/ref/subdir" -> Web URL
#'
#' @param ref Character string reference
#' @return List with components: type, user, repo, ref, subdir
#' @keywords internal
parse_remote_ref <- function(ref) {
  # Handle web URLs
  if (grepl("^https?://", ref)) {
    return(parse_remote_url(ref))
  }

  # Check for explicit type prefix
  type_pattern <- "^(github|gitlab|bitbucket|git|bioc)::"
  type_match <- regexpr(type_pattern, ref, perl = TRUE)

  if (type_match[1] != -1) {
    type <- regmatches(ref, type_match)
    type <- sub("::$", "", type)
    ref <- sub(type_pattern, "", ref)
  } else {
    # Default to GitHub for "user/repo" format
    type <- "github"
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

  if (length(parts) < 2) {
    stop(sprintf(
      paste0(
        "Invalid remote reference '%s'. Expected format: 'user/repo' ",
        "or 'user/repo/subdir'"
      ),
      ref
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
    original = ref
  )
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
    stop("Only GitHub and GitLab URLs are currently supported.")
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

  # Heuristic to split ref and subdir
  # 1. Common branches at the start
  common_branches <- c("main", "master", "develop", "dev", "trunk")
  ref <- NULL
  subdir <- NULL

  # Split by slash
  ref_parts <- strsplit(ref_and_subdir, "/")[[1]]

  if (ref_parts[1] %in% common_branches) {
    ref <- ref_parts[1]
    if (length(ref_parts) > 1) {
      subdir <- paste(ref_parts[2:length(ref_parts)], collapse = "/")
    }
  } else if (length(ref_parts) >= 2 &&
             ref_parts[1] %in% c("feature", "release", "hotfix", "patch")) {
    # Pattern: feature/branch-name/subdir
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

#' Create Remote Object for remotes Package
#'
#' @param parsed Parsed reference from parse_remote_ref()
#' @return Remote object suitable for remotes::remote_download()
#' @keywords internal
create_remote <- function(parsed) {
  switch(
    parsed$type,
    github = remotes::github_remote(
      repo = paste(parsed$user, parsed$repo, sep = "/"),
      ref = parsed$ref %||% "HEAD",
      subdir = parsed$subdir
    ),
    gitlab = structure(
      list(
        host = "gitlab.com",
        repo = parsed$repo,
        subdir = parsed$subdir,
        username = parsed$user,
        ref = parsed$ref %||% "HEAD",
        sha = NULL,
        auth_token = NULL
      ),
      class = c("gitlab_remote", "remote")
    ),
    bitbucket = structure(
      list(
        host = "api.bitbucket.org/2.0",
        repo = parsed$repo,
        subdir = parsed$subdir,
        username = parsed$user,
        ref = parsed$ref %||% "HEAD",
        sha = NULL,
        auth_user = NULL,
        password = NULL
      ),
      class = c("bitbucket_remote", "remote")
    ),
    stop(sprintf(
      "Unsupported remote type '%s'. Supported: github, gitlab, bitbucket",
      parsed$type
    ))
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

    dir_name <- paste(
      parsed$type,
      parsed$user,
      parsed$repo,
      ref_suffix,
      subdir_suffix,
      sep = "_"
    )
    dir_name <- gsub("_$", "", dir_name)

    file.path(cache_path, "remotes", dir_name)
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
  # Usually the bundle extracts to a single directory
  subdirs <- list.dirs(extract_dir, recursive = FALSE, full.names = TRUE)

  if (length(subdirs) == 1) {
    # Single subdirectory - likely the package
    return(subdirs[1])
  }

  # Multiple items - check if any is a package
  for (dir in subdirs) {
    if (file.exists(file.path(dir, "DESCRIPTION"))) {
      return(dir)
    }
  }

  # Return extract_dir if nothing else found
  extract_dir
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

  # Check for explicit type prefix
  if (grepl("^(github|gitlab|bitbucket|git|bioc)::", pkg)) {
    return(TRUE)
  }

  # Check for web URLs
  if (grepl("^https?://(github\\.com|gitlab\\.com)/", pkg)) {
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

#' Helper: NULL default operator
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
