#' Set `rdocdump` Repository Options
#'
#' @description
#' This function sets the package repository URLs used by `rdocdump` when fetching package sources. May be useful for setting custom repositories or mirrors. This does not affect the repositories used by `install.packages()` in your current R session and/or project.
#'
#' @param repos A character vector of repository URLs.
#'
#' @return Invisibly returns the new repository URLs.
#'
#' @examples
#' # Set rdocdump repository options
#' rdd_set_repos(c("CRAN" = "https://cloud.r-project.org"))
#'
#' @export
rdd_set_repos <- function(repos) {
  if (!is.character(repos)) {
    stop("repos must be a character vector of repository URLs.")
  }
  options(rdocdump.repos = repos)
  message("rdocdump.repos set to: ", paste(repos, collapse = ", "))
  invisible(getOption("rdocdump.repos"))
}

#' Get Current `rdocdump` Repository Options
#'
#' @description
#' This function returns the current repository URLs used by `rdocdump`. The default is set to the CRAN repository at "https://cloud.r-project.org". This does not affect the repositories used by `install.packages()` in your current R session and/or project. To set repository options, use \code{\link{rdd_set_repos}}.
#'
#' @return A character vector of repository URLs.
#'
#' @examples
#' # Get current rdocdump repository options
#' rdd_get_repos()
#'
#' @export
rdd_get_repos <- function() {
  getOption("rdocdump.repos", getOption("repos"))
}
