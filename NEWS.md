# rdocdump 0.2.0

* Added support for remote repository references (GitHub, GitLab, Bitbucket) in `rdd_to_txt()` and `rdd_extract_code()`. This includes support for specific branches/tags/commits and packages in subdirectories, as well as direct URLs (to branches and/or folders within repositories). Therefore the `remotes` package to `Suggests` to handle remote downloads.

# rdocdump 0.1.1 (2025-08-21)

* Added `version` argument to `rdd_to_txt()` and `rdd_extract_code()` functions to specify the version of the package to download from CRAN for processing.

# rdocdump 0.1.0 (2025-06-15)

* Initial CRAN submission.

* Basic functionality for parsing R documentation and source files.
