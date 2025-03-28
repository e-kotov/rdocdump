% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/to_txt.R
\name{rdd_to_txt}
\alias{rdd_to_txt}
\title{Create Combined Package Documentation as Text}
\usage{
rdd_to_txt(
  pkg,
  file = NULL,
  content = "both",
  force_fetch = FALSE,
  keep_files = "none",
  cache_path = getOption("rdocdump.cache_path")
)
}
\arguments{
\item{pkg}{A \code{character} string specifying the package. This can be:
\itemize{
\item an installed package name,
\item a full path to a package source directory,
\item a full path to a package archive file (tar.gz), or
\item a package name not installed (which will then be downloaded from CRAN).
}}

\item{file}{Optional. Save path for the output text file. If set, the function will return the path to the file instead of the combined text. Defaults to \code{NULL}.}

\item{content}{A \code{character} string specifying which components to include in the output.
Possible values are:
\itemize{
\item \code{"both"}: Include both Rd documentation and vignettes (default).
\item \code{"docs"}: Include only the Rd documentation.
\item \code{"vignettes"}: Include only the vignettes.
}}

\item{force_fetch}{\code{logical}. If \code{TRUE}, the package source will be fetched from CRAN as a tar.gz archive even if the package is already installed locally. Default is \code{FALSE}.}

\item{keep_files}{A \code{character} value controlling whether temporary files should be kept.
Possible values are:
\itemize{
\item \code{"none"}: Delete both the tar.gz archive and the extracted files (default).
\item \code{"tgz"}: Keep only the tar.gz archive.
\item \code{"extracted"}: Keep only the extracted files.
\item \code{"both"}: Keep both the tar.gz archive and the extracted files.
}}

\item{cache_path}{A \code{character} string specifying the directory where kept temporary files will be stored. By default, it uses the value of \code{getOption("rdocdump.cache_path")} which sets the cache directory to the temporary directory of the current R session.}
}
\value{
A single string containing the combined package documentation. If the \code{file} argument is set, returns the path to the file.
}
\description{
This function produces a single text output for a package by processing its documentation (Rd files from the package source, or the documentation from already installed packages) and/or vignettes (from \verb{*.Rmd}, \verb{*.qmd}, \verb{*.md}, and \verb{*.Rnw} files). The user can provide the function with an installed package name, a package source directory, a tar.gz archive, or a package name to download from CRAN.
}
\examples{
# Extract documentation for built-in `stats` package (both docs and vignettes).
docs <- rdd_to_txt("stats")

\donttest{
# Extract only documentation for rJavaEnv by downloading its source from CRAN
local({
  old_repos <- getOption("repos")
  options(repos = c(CRAN = "https://cran.r-project.org"))
  docs <- rdd_to_txt("rJavaEnv", force_fetch = TRUE, content = "docs")
  lines <- unlist(strsplit(docs, "\n"))
  # Print the first 3 lines
  cat(head(lines, 3), sep = "\n")
  # Print the last 3 lines
  cat(tail(lines, 3), sep = "\n")
  options(repos = old_repos)
})
}
}
