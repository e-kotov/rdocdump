#' Set rdocdump Repository Options
#'
#' @description
#' This function sets the repository URLs used by rdocdump when fetching package sources.
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
  if (!is.character(repos))
    stop("repos must be a character vector of repository URLs.")
  options(rdocdump.repos = repos)
  message("rdocdump.repos set to: ", paste(repos, collapse = ", "))
  invisible(getOption("rdocdump.repos"))
}

#' Get Current rdocdump Repository Options
#'
#' @description
#' This function returns the current repository URLs used by rdocdump.
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
