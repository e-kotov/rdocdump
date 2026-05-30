# pak (via pkgcache) requires a user cache directory.
# In CI environments or during R CMD check, this might not be set,
# causing pak subprocesses to fail.
if (Sys.getenv("R_USER_CACHE_DIR") == "") {
  # Use a temporary directory for the cache during tests
  Sys.setenv(R_USER_CACHE_DIR = tempfile("pak-cache-"))
}
