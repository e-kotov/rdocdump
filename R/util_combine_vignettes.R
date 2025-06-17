#' Helper function to combine package vignettes
#' @param pkg_path Path to the package source directory.
#' @return A single string containing the combined vignettes from the package.
#' @keywords internal
combine_vignettes <- function(pkg_path) {
  # Check for a "vignettes" directory; if not present, try "doc"
  if (dir.exists(file.path(pkg_path, "vignettes"))) {
    vignette_dir <- file.path(pkg_path, "vignettes")
  } else if (dir.exists(file.path(pkg_path, "doc"))) {
    vignette_dir <- file.path(pkg_path, "doc")
  } else {
    warning(
      "Neither 'vignettes' nor 'doc' directory found in the package source."
    )
    return("")
  }

  # List files with common vignette extensions: .Rmd, .Rnw, .md, .qmd
  vignette_files <- list.files(
    vignette_dir,
    pattern = "\\.(Rmd|Rnw|md|qmd)$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(vignette_files) == 0) {
    warning("No vignette files found in the directory: ", vignette_dir)
    return("")
  }

  combined_text <- ""

  for (vf in vignette_files) {
    header_line <- paste0(strrep("-", 80), "\nVignette: ", basename(vf), "\n")
    text <- readLines(vf, warn = FALSE)
    combined_text <- paste(
      combined_text,
      header_line,
      paste(text, collapse = "\n"),
      "\n\n",
      sep = "\n"
    )
  }

  return(combined_text)
}
