# Extract R Source Code from a Package

This function extracts the R source code from a package. For installed
packages, it retrieves the package namespace and deparses all functions
found in the package. For package source directories or archives
(non-installed packages), it reads all `.R` files from the `R` directory
and, optionally, from the `tests` directory. Optionally, it can include
roxygen2 documentation from these files.

## Usage

``` r
rdd_extract_code(
  pkg,
  file = NULL,
  include_tests = FALSE,
  include_roxygen = FALSE,
  force_fetch = FALSE,
  version = NULL,
  cache_path = getOption("rdocdump.cache_path"),
  keep_files = "none",
  repos = getOption("rdocdump.repos", getOption("repos"))
)
```

## Arguments

- pkg:

  A `character` string specifying the package. This can be:

  - an installed package name (e.g., `"ggplot2"`),

  - a full path to a package source directory,

  - a full path to a package archive file (tar.gz),

  - a package name not installed (which will then be downloaded from
    CRAN),

  - a GitHub repository reference (e.g., `"tidyverse/ggplot2"` or
    `"github::tidyverse/ggplot2"`),

  - a GitLab repository reference (e.g., `"gitlab::user/repo"`),

  - a repository reference with specific branch/tag/commit (e.g.,
    `"user/repo@v1.0.0"` or `"user/repo@main"`),

  - a repository reference with subdirectory (e.g., `"user/repo/subdir"`
    for packages not at repo root),

  - a full GitHub or GitLab web URL (e.g.,
    `"https://github.com/apache/sedona-db/tree/main/r/sedonadb"` or
    `"https://github.com/ipeaGIT/r5r/tree/master/r-package"`).

- file:

  Optional. Save path for the output text file. If set, the function
  will return the path to the file instead of the combined text.
  Defaults to `NULL`.

- include_tests:

  `logical`. If `TRUE`, for non-installed packages, the function will
  also include R source code from the `tests` directory. Defaults to
  `FALSE`.

- include_roxygen:

  `logical`. If `TRUE`, roxygen2 documentation lines (lines starting
  with "#'") from R files will be included in the output. Defaults to
  `FALSE`.

- force_fetch:

  `logical`. If `TRUE`, the package source will be fetched from CRAN
  even if the package is installed locally. Default is `FALSE`, but when
  `version` is specified, it will be set to `TRUE`.

- version:

  Optional. A `character` string specifying the package version to fetch
  from CRAN. If not provided, the latest version will be used.

- cache_path:

  A `character` string specifying the directory to use as a cache.
  Defaults to the value of `getOption("rdocdump.cache_path")`.

- keep_files:

  A `character` value controlling whether temporary files should be
  kept. Possible values are:

  - `"none"`: Delete both the tar.gz archive and the extracted files
    (default).

  - `"tgz"`: Keep only the tar.gz archive.

  - `"extracted"`: Keep only the extracted files.

  - `"both"`: Keep both the tar.gz archive and the extracted files.

- repos:

  A `character` vector of repository URLs. By default, it uses the value
  of `getOption("rdocdump.repos")` which sets the repository URLs to the
  default R repositories and is itself set to
  `c("CRAN" = "https://cloud.r-project.org")` on package load to prevent
  accidental downloads of pre-built packages from Posit Package Manager
  and R Universe.

## Value

A single string containing the combined R source code (and, optionally,
roxygen2 documentation) from the package.

## Examples

``` r
# Extract only R source code (excluding roxygen2 documentation) from an installed package.
code <- rdd_extract_code("splines")
cat(substr(code, 1, 1000))
#> --------------------------------------------------------------------------------
#> Function: .onUnload()
#> function (libpath) 
#> library.dynam.unload("splines", libpath)
#> 
#> --------------------------------------------------------------------------------
#> Function: as.data.frame.xyVector()
#> function (x, ...) 
#> data.frame(x = x$x, y = x$y)
#> 
#> --------------------------------------------------------------------------------
#> Function: as.polySpline()
#> function (object, ...) 
#> polySpline(object, ...)
#> 
#> --------------------------------------------------------------------------------
#> Function: asVector()
#> function (object) 
#> UseMethod("asVector")
#> 
#> --------------------------------------------------------------------------------
#> Function: asVector.xyVector()
#> function (object) 
#> object$y
#> 
#> --------------------------------------------------------------------------------
#> Function: backSpline()
#> function (object) 
#> UseMethod("backSpline")
#> 
#> --------------------------------------------------------------------------------
#> F

# Extract R source code including roxygen2 documentation from a package source directory.
# \donttest{
# set cache directory for `rdocdump`
rdd_set_cache_path(paste0(tempdir(), "/rdocdump_cache"))
#> rdocdump.cache_path set to: /tmp/RtmpmhHgKL/rdocdump_cache

local({
code_with_roxygen <- rdd_extract_code(
"ini",
include_roxygen = TRUE,
force_fetch = TRUE,
repos = c("CRAN" = "https://cran.r-project.org")
)
cat(substr(code_with_roxygen, 1, 1000))
})
#> Fetching package source from CRAN...
#> 
#> --------------------------------------------------------------------------------
#> File: ini.R
#> 
#> #' Read and parse .ini file to list
#> #'
#> #' @param filepath file to parse
#> #' @param encoding Encoding of filepath parameter, will default to system
#> #' encoding if not specifield
#> #'
#> #' @details Lines starting with '#' or ';' are comments and will not be parsed
#> #'
#> #' @seealso \code{\link{write.ini}}
#> #'
#> #' @return List with length equivalent to number of [sections], each section is
#> #' a new list
#> #'
#> #' @examples
#> #' ## Create a new temp ini for reading
#> #' iniFile <- tempfile(fileext = '.ini')
#> #'
#> #' sink(iniFile)
#> #' cat("; This line is a comment\n")
#> #' cat("# This one too!\n")
#> #' cat("[    Hello World]\n")
#> #' cat("Foo = Bar          \n")
#> #' cat("Foo1 = Bar=345 \n")
#> #' sink()
#> #'
#> #' ## Read ini
#> #' checkini <- read.ini(iniFile)
#> #'
#> #' ## Check structure
#> #' checkini
#> #' checkini$`Hello World`$Foo
#> #'
#> #' @export
#> #'
#> read.ini <- function(filepath, encoding = getOption("encoding")) {
#> 
#>   index <- function(x, run

# Extract R source code from a package source directory,
# including test files but excluding roxygen2 docs.
local({
code_with_tests <- rdd_extract_code(
"ini",
include_roxygen = TRUE,
include_tests = TRUE,
force_fetch = TRUE,
repos = c("CRAN" = "https://cran.r-project.org")
)
cat(substr(code_with_tests, 1, 1000))
})
#> Fetching package source from CRAN...
#> 
#> --------------------------------------------------------------------------------
#> File: ini.R
#> 
#> #' Read and parse .ini file to list
#> #'
#> #' @param filepath file to parse
#> #' @param encoding Encoding of filepath parameter, will default to system
#> #' encoding if not specifield
#> #'
#> #' @details Lines starting with '#' or ';' are comments and will not be parsed
#> #'
#> #' @seealso \code{\link{write.ini}}
#> #'
#> #' @return List with length equivalent to number of [sections], each section is
#> #' a new list
#> #'
#> #' @examples
#> #' ## Create a new temp ini for reading
#> #' iniFile <- tempfile(fileext = '.ini')
#> #'
#> #' sink(iniFile)
#> #' cat("; This line is a comment\n")
#> #' cat("# This one too!\n")
#> #' cat("[    Hello World]\n")
#> #' cat("Foo = Bar          \n")
#> #' cat("Foo1 = Bar=345 \n")
#> #' sink()
#> #'
#> #' ## Read ini
#> #' checkini <- read.ini(iniFile)
#> #'
#> #' ## Check structure
#> #' checkini
#> #' checkini$`Hello World`$Foo
#> #'
#> #' @export
#> #'
#> read.ini <- function(filepath, encoding = getOption("encoding")) {
#> 
#>   index <- function(x, run
# clean cache directory
unlink(getOption("rdocdump.cache_path"), recursive = TRUE, force = TRUE)
# }
```
