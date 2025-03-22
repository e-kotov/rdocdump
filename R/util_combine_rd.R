combine_rd <- function(pkg_path) {
  rd_db <- tools::Rd_db(dir = pkg_path)

  rd_texts <- mapply(
    function(rd, rd_name) {
      txt <- capture.output(tools::Rd2txt(Rd = rd))
      txt_clean <- gsub("_\b", "", txt, fixed = TRUE)
      # Extract function name by removing ".Rd" suffix from the file name.
      fname <- sub("\\.Rd$", "", rd_name)
      header_line <- paste0(strrep("-", 80), "\nFunction: ", fname, "()\n")
      paste0(header_line, paste(txt_clean, collapse = "\n"))
    },
    rd_db,
    names(rd_db),
    SIMPLIFY = FALSE
  )

  # Optionally include the DESCRIPTION file as a header.
  desc_file <- file.path(pkg_path, "DESCRIPTION")
  if (file.exists(desc_file)) {
    desc_text <- paste(readLines(desc_file), collapse = "\n")
    header <- paste0("DESCRIPTION:\n", desc_text, "\n\n")
  } else {
    header <- ""
  }

  combined_rd <- paste0(header, paste(unlist(rd_texts), collapse = "\n\n"))
  return(combined_rd)
}
