test_that("rdd_set_cache_path sets the cache path correctly", {
  # Backup any existing option and ensure it’s restored
  old_opt <- getOption("rdocdump.cache_path")
  on.exit(options(rdocdump.cache_path = old_opt), add = TRUE)

  temp_cache <- tempfile("rdocdump_cache")
  on.exit(unlink(temp_cache, recursive = TRUE), add = TRUE)

  result <- rdd_set_cache_path(temp_cache)
  normalized_path <- normalizePath(temp_cache, winslash = "/")

  expect_equal(getOption("rdocdump.cache_path"), normalized_path)
  expect_equal(result, normalized_path)
})
