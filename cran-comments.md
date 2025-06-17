# Resubmission

* Description cleaned up to only keep quotes for package names, software names and API names.

* Added explicit cache creation (in temp) and cleanup in examples.

* Revised tests to do proper function mocking and avoid changes to the global environment.

* Revised examples to cleanup temporary files and folders created during examples execution.

* Added an internal helper function that is used in exported functions to clean up the cache more reliably.

* Using tempdir() in examples in README and vignettes.


# Original submission:

* Link checking errors are due to inclusion of URLs that will become available after package release to CRAN.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
