test_that("resolve_pkg_path identifies remote packages correctly", {
  skip_if_not_installed("pak")

  # Mock pak::pkg_download to avoid actual network calls
  mock_pkg_download <- function(pkg, dest_dir, dependencies) {
    # Simulate a successful download
    tar_file <- file.path(dest_dir, "testpkg_0.1.tar.gz")
    # Create a dummy tarball
    pkg_dir <- file.path(dest_dir, "testpkg")
    dir.create(pkg_dir, showWarnings = FALSE)
    writeLines("Package: testpkg\nVersion: 0.1", file.path(pkg_dir, "DESCRIPTION"))

    # Use withr::with_dir to change directory safely for tar
    old_wd <- getwd()
    on.exit(setwd(old_wd))
    setwd(dest_dir)
    utils::tar("testpkg_0.1.tar.gz", files = "testpkg", tar = "internal")

    # Return data frame like pak::pkg_download
    data.frame(
      fulltarget = tar_file,
      stringsAsFactors = FALSE
    )
  }

  local_mocked_bindings(
    pkg_download = mock_pkg_download,
    .package = "pak"
  )

  # Test user/repo format
  res <- resolve_pkg_path("user/repo", cache_path = tempdir())
  expect_false(res$is_installed)
  expect_true(!is.null(res$extracted_path))

  # Test type::pkg format
  res2 <- resolve_pkg_path("gitlab::user/repo", cache_path = tempdir())
  expect_false(res2$is_installed)

  # Test user/repo@version format
  res3 <- resolve_pkg_path("user/repo", version = "v1.0", cache_path = tempdir())
  expect_false(res3$is_installed)
  # Mock doesn't validate the pkg string content, but we ensure it didn't crash
})

test_that("resolve_pkg_path fails informatively for missing local paths", {
  # If we pass a path starting with ./ or / that doesn't exist,
  # it should FAIL with a specific "path does not exist" error,
  # NOT try to download it as a remote.

  calls <- new.env()
  calls$pak_called <- FALSE

  mock_pkg_download <- function(...) {
    calls$pak_called <- TRUE
    data.frame()
  }

  # Mock download.packages to just return empty to simulate "not found on CRAN"
  mock_download_packages <- function(...) {
    matrix(character(0), nrow = 0, ncol = 2)
  }

  if (requireNamespace("pak", quietly = TRUE)) {
    local_mocked_bindings(
      pkg_download = mock_pkg_download,
      .package = "pak"
    )
  }

  local_mocked_bindings(
    download.packages = mock_download_packages,
    .package = "utils"
  )

  # 1. Explicit relative path "./nonexistent"
  # Should FAIL immediately as path not found, NOT trigger pak logic
  expect_error(
    resolve_pkg_path("./nonexistent"),
    "specified path does not exist"
  )
  expect_false(calls$pak_called)

  # 2. Absolute path "/tmp/nonexistent" (Unix-style) or Windows absolute
  # Should FAIL immediately
  expect_error(
    resolve_pkg_path("/tmp/nonexistent"),
    "specified path does not exist"
  )
  expect_false(calls$pak_called)

  # 3. Path with extension "foo/bar.tar.gz"
  # Should FAIL immediately if it looks like a file path
  expect_error(
    resolve_pkg_path("nonexistent/package.tar.gz"),
    "specified path does not exist"
  )
  expect_false(calls$pak_called)

  # 4. Remote-like string "nonexistent/path" (no leading ./ or /, no ext)
  # SHOULD trigger pak (looks like user/repo)
  # But fails because our mock returns empty (or enters pak block and mock doesn't return valid)
  expect_error(
    resolve_pkg_path("nonexistent/path"),
    "Failed to download package from remote"
  )
  expect_true(calls$pak_called)
})

test_that("missing pak throws specific error for remote specs", {
  # Mock requireNamespace to return FALSE for pak
  mock_requireNamespace <- function(package, ...) {
    if (package == "pak") return(FALSE)
    base::requireNamespace(package, ...)
  }

  local_mocked_bindings(
    requireNamespace = mock_requireNamespace,
    .package = "base"
  )

  expect_error(
    resolve_pkg_path("user/repo"),
    "The 'pak' package is required"
  )
})
