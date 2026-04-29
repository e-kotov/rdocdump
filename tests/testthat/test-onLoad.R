test_that(".onLoad sets default options", {
  # Backup any existing options
  old_cache <- getOption("rdocdump.cache_path")
  old_repos <- getOption("rdocdump.repos")

  options(rdocdump.cache_path = NULL)
  options(rdocdump.repos = NULL)

  # Call .onLoad
  rdocdump:::.onLoad(libname = NULL, pkgname = "rdocdump")

  expect_false(is.null(getOption("rdocdump.cache_path")))
  expect_false(is.null(getOption("rdocdump.repos")))

  # Restore previous options
  options(rdocdump.cache_path = old_cache)
  options(rdocdump.repos = old_repos)
})

test_that(".onLoad does not override existing options", {
  old_cache <- getOption("rdocdump.cache_path")
  old_repos <- getOption("rdocdump.repos")

  custom_cache <- "/tmp/custom_cache"
  custom_repos <- c(CRAN = "https://my.cran.org")

  options(rdocdump.cache_path = custom_cache)
  options(rdocdump.repos = custom_repos)

  rdocdump:::.onLoad(libname = NULL, pkgname = "rdocdump")

  expect_equal(getOption("rdocdump.cache_path"), custom_cache)
  expect_equal(getOption("rdocdump.repos"), custom_repos)

  # Restore
  options(rdocdump.cache_path = old_cache)
  options(rdocdump.repos = old_repos)
})
