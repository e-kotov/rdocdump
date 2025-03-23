# test_that(".onLoad sets rdocdump.cache_path if not already set", {
#   # Backup any existing option
#   old_opt <- getOption("rdocdump.cache_path")
#   options(rdocdump.cache_path = NULL)

#   # Expected default value based on tempdir()
#   default_cache <- file.path(tempdir(), "rdocdump_cache")

#   # Call .onLoad
#   .onLoad(libname = NULL, pkgname = NULL)
#   expect_equal(getOption("rdocdump.cache_path"), default_cache)

#   # Restore previous option
#   options(rdocdump.cache_path = old_opt)
# })

# test_that(".onLoad does not override existing rdocdump.cache_path", {
#   old_value <- file.path(tempdir(), "existing_cache")
#   options(rdocdump.cache_path = old_value)

#   .onLoad(libname = NULL, pkgname = NULL)
#   expect_equal(getOption("rdocdump.cache_path"), old_value)
# })
