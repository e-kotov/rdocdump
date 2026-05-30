#' Find the target row from a `pak::pkg_download()` result
#'
#' Filters `dl_info` to rows where `direct` is `TRUE` if the column exists,
#' and matches `pkg_ref` if provided. Always returns a 1-row data frame.
#'
#' @param dl_info Data frame returned by `pak::pkg_download()`.
#' @param pkg_ref Optional package reference requested from pak.
#' @return One-row data frame.
#' @keywords internal
#' @noRd
find_pak_target_row <- function(dl_info, pkg_ref = NULL) {
  if (!is.data.frame(dl_info) || nrow(dl_info) < 1L) {
    stop("No package download information returned by pak.")
  }

  if ("direct" %in% names(dl_info)) {
    direct <- !is.na(dl_info$direct) & dl_info$direct
    if (any(direct)) {
      dl_info <- dl_info[direct, , drop = FALSE]
    }
  }

  if (!is.null(pkg_ref) && "ref" %in% names(dl_info)) {
    exact_ref <- !is.na(dl_info$ref) & dl_info$ref == pkg_ref
    if (any(exact_ref)) {
      dl_info <- dl_info[exact_ref, , drop = FALSE]
    }
  }

  dl_info[1L, , drop = FALSE]
}

#' Safely extract a single character value from a one-row data frame
#'
#' Returns `NA_character_` if the column is missing or the value is `NULL`.
#'
#' @param row One-row data frame.
#' @param col Column name to extract.
#' @return Single character value, or `NA_character_`.
#' @keywords internal
#' @noRd
pak_row_value <- function(row, col) {
  if (!col %in% names(row)) {
    return(NA_character_)
  }
  val <- row[[col]][1L]
  if (is.null(val)) {
    return(NA_character_)
  }
  as.character(val)
}

#' Select the archive downloaded by pak
#'
#' @param dl_info Data frame returned by `pak::pkg_download()`.
#' @param dest_dir Destination directory passed to `pak::pkg_download()`.
#' @param pkg_ref Package reference requested from pak.
#' @return Path to a usable `.tar.gz` archive.
#' @keywords internal
#' @noRd
select_pak_download_archive <- function(dl_info, dest_dir, pkg_ref = NULL) {
  row <- find_pak_target_row(dl_info, pkg_ref)
  paths <- pak_download_paths(row, dest_dir)

  for (path in paths) {
    archive <- pak_materialize_archive(path, dest_dir)
    if (!is.null(archive)) {
      return(archive)
    }
  }

  fallback_name <- pak_row_value(row, "package")
  stop(sprintf(
    "No downloaded archive found in %s for '%s'.",
    dest_dir,
    pkg_ref %||% (if (is.na(fallback_name)) "requested package" else fallback_name)
  ))
}

pak_download_paths <- function(row, dest_dir) {
  values <- character()

  if ("fulltarget" %in% names(row) && !is.na(row$fulltarget[1L])) {
    values <- c(values, row$fulltarget[1L])
  }

  if ("target" %in% names(row) && !is.na(row$target[1L])) {
    target <- row$target[1L]
    # Recognize POSIX absolute, Windows drive-letter, and Windows UNC paths.
    if (grepl("^(/|[A-Za-z]:|\\\\\\\\)", target)) {
      values <- c(values, target)
    } else {
      values <- c(values, file.path(dest_dir, target))
    }
    values <- c(values, file.path(dest_dir, basename(target)))
  }

  unique(values[nzchar(values)])
}

pak_materialize_archive <- function(path, dest_dir) {
  archive <- file.path(dest_dir, basename(sub("-t$", "", path)))

  if (file.exists(path) && !dir.exists(path)) {
    return(copy_pak_archive(path, archive))
  }

  tree_path <- if (grepl("-t$", path)) path else paste0(path, "-t")
  if (file.exists(tree_path) && !dir.exists(tree_path)) {
    return(copy_pak_archive(tree_path, archive))
  }

  if (dir.exists(tree_path)) {
    return(tar_pak_tree(tree_path, archive))
  }

  if (file.exists(archive) && !dir.exists(archive)) {
    return(archive)
  }

  NULL
}

copy_pak_archive <- function(from, to) {
  from_norm <- normalizePath(from, mustWork = TRUE)
  to_norm <- normalizePath(to, mustWork = FALSE)

  if (from_norm == to_norm) {
    return(to)
  }

  if (file.exists(to)) {
    unlink(to, recursive = TRUE)
  }

  if (!dir.exists(dirname(to))) {
    dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  }

  ok <- file.copy(from, to, overwrite = TRUE)
  if (!isTRUE(ok)) {
    stop(sprintf("Failed to copy downloaded archive to %s.", to))
  }

  to
}

tar_pak_tree <- function(tree_path, archive) {
  files <- list.files(tree_path, all.files = TRUE, no.. = TRUE)
  if (length(files) < 1L) {
    stop(sprintf("Downloaded package tree is empty: %s.", tree_path))
  }

  if (file.exists(archive)) {
    unlink(archive, recursive = TRUE)
  }

  if (!dir.exists(dirname(archive))) {
    dir.create(dirname(archive), recursive = TRUE, showWarnings = FALSE)
  }

  # utils::tar() needs relative file paths to avoid absolute paths in archives.
  archive <- normalizePath(archive, mustWork = FALSE)
  old_wd <- setwd(tree_path)
  on.exit(setwd(old_wd), add = TRUE)

  # Prefer a system tar (env var or auto-detected) so long paths and non-ASCII
  # names are handled correctly; fall back to R's internal ustar implementation.
  tar_cmd <- Sys.getenv("TAR", "internal")
  utils::tar(
    tarfile = archive,
    files = files,
    compression = "gzip",
    tar = tar_cmd
  )

  if (!file.exists(archive)) {
    stop(sprintf(
      "Failed to create archive from downloaded package tree: %s.",
      archive
    ))
  }

  archive
}
