test_that("is_remote_reference detects remote references correctly", {
  # GitHub shorthand
  expect_true(is_remote_reference("user/repo"))
  expect_true(is_remote_reference("tidyverse/ggplot2"))
  expect_true(is_remote_reference("r-lib/rlang"))

  # Explicit types
  expect_true(is_remote_reference("github::user/repo"))
  expect_true(is_remote_reference("gitlab::user/repo"))
  expect_true(is_remote_reference("bitbucket::user/repo"))

  # With refs
  expect_true(is_remote_reference("user/repo@main"))
  expect_true(is_remote_reference("user/repo@v1.0.0"))
  expect_true(is_remote_reference("user/repo@abc123"))

  # With subdirs
  expect_true(is_remote_reference("user/repo/subdir"))
  expect_true(is_remote_reference("user/repo/pkg/subdir"))

  # Combined
  expect_true(is_remote_reference("user/repo/subdir@main"))

  # Not remote references
  expect_false(is_remote_reference("ggplot2"))
  expect_false(is_remote_reference("/path/to/pkg"))
  expect_false(is_remote_reference("./local/pkg"))
  expect_false(is_remote_reference("~/home/pkg"))
  expect_false(is_remote_reference("C:\\Windows\\pkg"))
  expect_false(is_remote_reference(NULL))
  expect_false(is_remote_reference(123))
  expect_false(is_remote_reference(c("a", "b")))
})

test_that("parse_remote_ref parses GitHub shorthand correctly", {
  p <- parse_remote_ref("user/repo")
  expect_equal(p$type, "github")
  expect_equal(p$user, "user")
  expect_equal(p$repo, "repo")
  expect_null(p$ref)
  expect_null(p$subdir)
})

test_that("parse_remote_ref parses explicit GitHub correctly", {
  p <- parse_remote_ref("github::tidyverse/ggplot2")
  expect_equal(p$type, "github")
  expect_equal(p$user, "tidyverse")
  expect_equal(p$repo, "ggplot2")
})

test_that("parse_remote_ref parses GitLab correctly", {
  p <- parse_remote_ref("gitlab::user/repo")
  expect_equal(p$type, "gitlab")
  expect_equal(p$user, "user")
  expect_equal(p$repo, "repo")
})

test_that("parse_remote_ref parses Bitbucket correctly", {
  p <- parse_remote_ref("bitbucket::user/repo")
  expect_equal(p$type, "bitbucket")
  expect_equal(p$user, "user")
  expect_equal(p$repo, "repo")
})

test_that("parse_remote_ref handles refs correctly", {
  p <- parse_remote_ref("user/repo@main")
  expect_equal(p$ref, "main")
  expect_equal(p$repo, "repo")

  p <- parse_remote_ref("user/repo@v1.0.0")
  expect_equal(p$ref, "v1.0.0")

  p <- parse_remote_ref("user/repo@abc123def")
  expect_equal(p$ref, "abc123def")
})

test_that("parse_remote_ref handles subdirectories correctly", {
  p <- parse_remote_ref("user/repo/subdir")
  expect_equal(p$subdir, "subdir")

  p <- parse_remote_ref("user/repo/nested/subdir")
  expect_equal(p$subdir, "nested/subdir")
})

test_that("parse_remote_ref handles combined refs and subdirs", {
  p <- parse_remote_ref("user/repo/subdir@main")
  expect_equal(p$type, "github")
  expect_equal(p$user, "user")
  expect_equal(p$repo, "repo")
  expect_equal(p$subdir, "subdir")
  expect_equal(p$ref, "main")
})

test_that("parse_remote_ref errors on invalid references", {
  expect_error(parse_remote_ref("invalid"), "Invalid remote reference")
  expect_error(parse_remote_ref("user"), "Invalid remote reference")
  # Note: "/user/repo" is not detected as remote by is_remote_reference
  # but parse_remote_ref will error because it only has one part after
  # removing type
})
test_that("get_remote_cache_dir creates correct paths", {
  p <- list(
    type = "github",
    user = "tidyverse",
    repo = "ggplot2",
    ref = NULL,
    subdir = NULL
  )
  path <- get_remote_cache_dir(p, "/cache")
  expect_match(path, "/cache/remotes/github_tidyverse_ggplot2_HEAD$")

  p$ref <- "main"
  path <- get_remote_cache_dir(p, "/cache")
  expect_match(path, "/cache/remotes/github_tidyverse_ggplot2_main$")

  p$subdir <- "pkg/subdir"
  path <- get_remote_cache_dir(p, "/cache")
  expect_match(path, "github_tidyverse_ggplot2_main_pkg_subdir$")
})

test_that("find_pkg_dir finds package directory correctly", {
  # Create temp structure
  temp_dir <- tempfile()
  dir.create(temp_dir)
  pkg_dir <- file.path(temp_dir, "mypackage")
  dir.create(pkg_dir)
  writeLines("Package: mypackage", file.path(pkg_dir, "DESCRIPTION"))

  # Should find the subdirectory
  found <- find_pkg_dir(temp_dir, NULL)
  expect_equal(found, pkg_dir)

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("find_pkg_dir handles specified subdirectory", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  sub_dir <- file.path(temp_dir, "nested", "pkg")
  dir.create(sub_dir, recursive = TRUE)
  writeLines("Package: pkg", file.path(sub_dir, "DESCRIPTION"))

  found <- find_pkg_dir(temp_dir, "nested/pkg")
  expect_equal(found, sub_dir)

  unlink(temp_dir, recursive = TRUE)
})

test_that("is_remote_reference detects web URLs correctly", {
  expect_true(is_remote_reference("https://github.com/user/repo"))
  expect_true(is_remote_reference("http://github.com/user/repo"))
  expect_true(is_remote_reference("https://gitlab.com/user/repo"))
  expect_true(
    is_remote_reference("https://github.com/user/repo/tree/main/subdir")
  )
})

test_that("parse_remote_url parses GitHub URLs correctly", {
  # Simple
  p <- parse_remote_url("https://github.com/user/repo")
  expect_equal(p$type, "github")
  expect_equal(p$user, "user")
  expect_equal(p$repo, "repo")
  expect_null(p$ref)
  expect_null(p$subdir)

  # With branch
  p <- parse_remote_url("https://github.com/user/repo/tree/master")
  expect_equal(p$ref, "master")
  expect_null(p$subdir)

  # With branch and subdir
  p <- parse_remote_url(
    "https://github.com/apache/sedona-db/tree/main/r/sedonadb"
  )
  expect_equal(p$user, "apache")
  expect_equal(p$repo, "sedona-db")
  expect_equal(p$ref, "main")
  expect_equal(p$subdir, "r/sedonadb")

  # With complex branch
  p <- parse_remote_url(
    "https://github.com/user/repo/tree/feature/cool-stuff/pkg"
  )
  expect_equal(p$ref, "feature/cool-stuff")
  expect_equal(p$subdir, "pkg")
})
test_that("parse_remote_url parses GitLab URLs correctly", {
  p <- parse_remote_url("https://gitlab.com/user/repo/-/tree/main/subdir")
  expect_equal(p$type, "gitlab")
  expect_equal(p$user, "user")
  expect_equal(p$repo, "repo")
  expect_equal(p$ref, "main")
  expect_equal(p$subdir, "subdir")
})

test_that("create_remote creates correct remote objects", {
  # GitHub
  p <- list(
    type = "github",
    user = "r-lib",
    repo = "rlang",
    ref = "main",
    subdir = NULL
  )
  remote <- create_remote(p)
  expect_s3_class(remote, "github_remote")

  # GitLab
  p <- list(
    type = "gitlab",
    user = "user",
    repo = "repo",
    ref = "HEAD",
    subdir = NULL
  )
  remote <- create_remote(p)
  expect_s3_class(remote, "gitlab_remote")
  expect_equal(remote$host, "gitlab.com")
  expect_equal(remote$repo, "repo")

  # Bitbucket
  p <- list(
    type = "bitbucket",
    user = "user",
    repo = "repo",
    ref = "HEAD",
    subdir = NULL
  )
  remote <- create_remote(p)
  expect_s3_class(remote, "bitbucket_remote")
  expect_equal(remote$host, "api.bitbucket.org/2.0")
  expect_equal(remote$repo, "repo")

  # Unsupported type
  p <- list(
    type = "unknown",
    user = "user",
    repo = "repo",
    ref = NULL,
    subdir = NULL
  )
  expect_error(create_remote(p), "Unsupported remote type")
})

# Integration tests - only run when online
test_that("resolve_remote_pkg downloads GitHub package correctly", {
  skip_if_offline()
  skip_on_cran()

  cache <- tempfile()
  dir.create(cache)

  # Use a small, stable repo
  info <- resolve_remote_pkg("r-lib/crayon", cache_path = cache)

  expect_true(dir.exists(info$pkg_path))
  expect_true(file.exists(file.path(info$pkg_path, "DESCRIPTION")))
  expect_false(info$is_installed)
  expect_equal(info$remote_info$type, "github")
  expect_equal(info$remote_info$user, "r-lib")
  expect_equal(info$remote_info$repo, "crayon")

  # Cleanup
  unlink(cache, recursive = TRUE)
})

test_that("resolve_remote_pkg handles branches correctly", {
  skip_if_offline()
  skip_on_cran()

  cache <- tempfile()
  dir.create(cache)

  info <- resolve_remote_pkg("r-lib/crayon@main", cache_path = cache)

  expect_true(dir.exists(info$pkg_path))
  expect_equal(info$remote_info$ref, "main")

  unlink(cache, recursive = TRUE)
})

test_that("resolve_remote_pkg caches correctly", {
  skip_if_offline()
  skip_on_cran()

  cache <- tempfile()
  dir.create(cache)

  # First download
  info1 <- resolve_remote_pkg("r-lib/crayon", cache_path = cache)

  # Second download should use cache
  info2 <- resolve_remote_pkg("r-lib/crayon", cache_path = cache)

  expect_equal(info1$pkg_path, info2$pkg_path)
  expect_true(file.exists(info1$tar_path))

  unlink(cache, recursive = TRUE)
})

test_that("resolve_remote_pkg fails on binary repositories", {
  skip_if_offline()
  skip_on_cran()

  # We mock a remote download by creating a directory that looks like a 
  # downloaded repo but contains binary files.
  # Since resolve_remote_pkg uses remotes::remote_download, we can't easily 
  # mock the download without mocking the whole remotes package,
  # but we can test check_if_binary directly to ensure it works for remotes too.
  
  # Create a dummy binary package directory
  bin_pkg <- tempfile("bin_remote_pkg")
  dir.create(bin_pkg)
  writeLines("Package: binpkg\nVersion: 1.0", file.path(bin_pkg, "DESCRIPTION"))
  dir.create(file.path(bin_pkg, "Meta"))
  
  expect_error(
    check_if_binary(bin_pkg),
    "appears to be a pre-built binary"
  )
  
  unlink(bin_pkg, recursive = TRUE)
})
