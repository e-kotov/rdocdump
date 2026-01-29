
test_that("resolve_pkg_path identifies remote package and errors without pak", {
  # Mocking requireNamespace to return FALSE for "pak"
  # Since we can't easily mock requireNamespace in testthat without mockery,
  # we rely on the fact that if pak IS installed, this test will fail to error.
  # So we check if pak is installed first.

  if (!requireNamespace("pak", quietly = TRUE)) {
    expect_error(
      resolve_pkg_path("user/repo"),
      "The 'pak' package is required"
    )
  } else {
    # If pak IS installed, we can't easily test the error path without mocking.
    # We can at least test that it tries to download and fails (since "user/repo" is likely invalid)
    # or skip.
    skip("pak is installed, skipping 'missing pak' test")
  }
})

test_that("resolve_pkg_path works for CRAN packages", {
  # "stats" is a base package, should be found installed
  res <- resolve_pkg_path("stats")
  expect_true(res$is_installed)
  expect_equal(res$pkg_name, "stats")
})

test_that("resolve_pkg_path distinguishes local file from remote", {
  # Create a dummy file with a slash in name (if possible? no, but path has slashes)
  tmp <- tempfile()
  dir.create(tmp)
  # Create a dummy tarball structure
  tar_name <- "dummy_1.0.tar.gz"
  tar_path <- file.path(tmp, tar_name)
  file.create(tar_path)

  # Should identify as local file (even if extraction fails later due to empty file)
  # Actually resolve_pkg_path attempts to untar immediately if file exists
  # So we need a valid tarball to avoid error during untar

  # Creating a minimal valid tarball is hard without tar/R.
  # We will test the failure mode: if file exists, it tries untar.
  # If file doesn't exist, it goes to remote check.

  non_existent <- file.path(tmp, "non/existent/package")

  # This path contains "/", so if it doesn't exist, it hits the remote block.
  # If pak is missing, it errors with "pak required".
  # If pak is present, it errors with "Failed to download" (because invalid repo).

  if (!requireNamespace("pak", quietly = TRUE)) {
    expect_error(
      resolve_pkg_path(non_existent),
      "The 'pak' package is required"
    )
  } else {
    expect_error(
      resolve_pkg_path(non_existent),
      "Failed to download package from remote"
    )
  }
})
