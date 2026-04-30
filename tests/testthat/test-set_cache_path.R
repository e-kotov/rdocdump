test_that("rdd_set_cache_path sets the cache path correctly", {
  temp_cache <- tempfile("rdocdump_cache")
  on.exit(unlink(temp_cache, recursive = TRUE), add = TRUE)

  # <<< NEW: scope any existing option so it's auto-restored at test end
  withr::local_options(list(
    rdocdump.cache_path = getOption("rdocdump.cache_path")
  ))

  expect_message(
    {
      result <- rdd_set_cache_path(temp_cache)
    },
    "rdocdump.cache_path set to:"
  )

  normalized_path <- normalizePath(temp_cache, winslash = "/")

  expect_equal(getOption("rdocdump.cache_path"), normalized_path)
  expect_equal(result, normalized_path)

  # Test with existing directory
  expect_message(rdd_set_cache_path(temp_cache), "rdocdump.cache_path set to:")

  # Test validation
  expect_error(rdd_set_cache_path(123), "must be a single character string")
  expect_error(
    rdd_set_cache_path(c("a", "b")),
    "must be a single character string"
  )
})
