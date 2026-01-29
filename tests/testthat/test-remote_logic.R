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
    # Note: we should not rely on 'tar' from utils being mocked if we call it via namespace in the code,
    # but here we are in the test setup.
    # The actual code calls utils::untar, not utils::tar.

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
})

test_that("resolve_pkg_path does NOT treat non-existent local paths as remotes", {
  # If we pass a path starting with ./ or / that doesn't exist,
  # it should fall through to the CRAN check (and fail there),
  # NOT try to use pak.

  # We mock find.package and download.packages to verify they ARE called (or that pak is NOT called)

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

  # "./nonexistent" should NOT trigger pak (starts with ./)
  expect_error(
    resolve_pkg_path("./nonexistent"),
    "Package not found on CRAN"
  )
  expect_false(calls$pak_called)

  # "nonexistent/path" (no leading ./) SHOULD trigger pak (looks like user/repo)
  # But fails because our mock returns empty
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
