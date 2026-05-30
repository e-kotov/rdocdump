test_that("is_remote_reference detects remote references correctly", {
  # GitHub shorthand
  expect_true(is_remote_reference("user/repo"))
  expect_true(is_remote_reference("tidyverse/ggplot2"))
  expect_true(is_remote_reference("r-lib/rlang"))

  # Explicit types
  expect_true(is_remote_reference("github::user/repo"))
  expect_true(is_remote_reference("gitlab::user/repo"))
  # Recognized so users get a clear unsupported-remote error.
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

test_that("parse_remote_ref handles refs correctly", {
  p <- parse_remote_ref("user/repo@main")
  expect_equal(p$ref, "main")
  expect_equal(p$repo, "repo")

  p <- parse_remote_ref("user/repo@v1.0.0")
  expect_equal(p$ref, "v1.0.0")

  p <- parse_remote_ref("user/repo@abc123def")
  expect_equal(p$ref, "abc123def")
})

test_that("build_pak_remote_ref formats supported remotes correctly", {
  expect_equal(
    build_pak_remote_ref(parse_remote_ref("user/repo/subdir@main")),
    "github::user/repo/subdir@main"
  )

  expect_equal(
    build_pak_remote_ref(parse_remote_ref("gitlab::user/repo/subdir@main")),
    "gitlab::user/repo/-/subdir@main"
  )
})

test_that("build_pak_remote_ref translates Bitbucket refs for backward compatibility", {
  ref <- build_pak_remote_ref(parse_remote_ref("bitbucket::user/repo"))
  expect_equal(ref, "git::https://bitbucket.org/user/repo.git")
})

test_that("build_pak_remote_ref passes through unknown types transparently", {
  ref <- build_pak_remote_ref(parse_remote_ref("url::https://example.com/pkg.tar.gz"))
  expect_equal(ref, "url::https://example.com/pkg.tar.gz")
})

test_that("build_pak_remote_ref formats Bioconductor refs", {
  expect_equal(
    build_pak_remote_ref(parse_remote_ref("bioc::Biobase")),
    "bioc::Biobase"
  )
})

test_that("build_pak_remote_ref passes git:: URLs through", {
  ref <- build_pak_remote_ref(
    parse_remote_ref("git::https://example.com/user/repo.git")
  )
  expect_equal(ref, "git::https://example.com/user/repo.git")
})

test_that("parse_remote_ref rejects multi-segment bioc refs", {
  expect_error(
    parse_remote_ref("bioc::Bio/Base"),
    "Invalid Bioconductor reference"
  )
})

test_that("is_remote_reference recognizes bioc and git prefixes", {
  expect_true(is_remote_reference("bioc::Biobase"))
  expect_true(is_remote_reference("git::https://example.com/repo.git"))
})

test_that("select_pak_download_archive uses pak target, not cache scans", {
  cache <- tempfile()
  dir.create(cache)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)

  stale_pkg <- file.path(cache, "stale")
  dir.create(stale_pkg)
  writeLines("Package: stale\nVersion: 1.0.0", file.path(stale_pkg, "DESCRIPTION"))
  stale_archive <- file.path(cache, "stale_1.0.0.tar.gz")
  withr::with_dir(cache, {
    utils::tar(stale_archive, "stale", tar = "internal")
  })

  target_tree <- file.path(cache, "src", "contrib", "good_1.0.0.tar.gz-t")
  good_pkg <- file.path(target_tree, "good")
  dir.create(good_pkg, recursive = TRUE)
  writeLines("Package: good\nVersion: 1.0.0", file.path(good_pkg, "DESCRIPTION"))

  dl_info <- data.frame(
    ref = "good",
    direct = TRUE,
    package = "good",
    target = "src/contrib/good_1.0.0.tar.gz",
    fulltarget = file.path(cache, "src", "contrib", "good_1.0.0.tar.gz"),
    stringsAsFactors = FALSE
  )

  archive <- select_pak_download_archive(dl_info, cache, "good")
  extract_dir <- file.path(cache, "extracted")
  dir.create(extract_dir)
  utils::untar(archive, exdir = extract_dir, tar = "internal")

  expect_equal(basename(archive), "good_1.0.0.tar.gz")
  expect_match(
    readLines(file.path(extract_dir, "good", "DESCRIPTION"), n = 1),
    "Package: good"
  )
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
  expect_match(path, "/cache/pak/github_tidyverse_ggplot2_HEAD$")

  p$ref <- "main"
  path <- get_remote_cache_dir(p, "/cache")
  expect_match(path, "/cache/pak/github_tidyverse_ggplot2_main$")

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
  expect_equal(normalizePath(found), normalizePath(pkg_dir))

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
  expect_equal(normalizePath(found), normalizePath(sub_dir))

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

  # Modern conventional prefixes
  p <- parse_remote_url("https://github.com/user/repo/tree/feat/new-thing/pkg")
  expect_equal(p$ref, "feat/new-thing")
  expect_equal(p$subdir, "pkg")

  p <- parse_remote_url(
    "https://github.com/user/repo/tree/renovate/dep-1.x/sub"
  )
  expect_equal(p$ref, "renovate/dep-1.x")
  expect_equal(p$subdir, "sub")

  # Version tags treated as single-segment refs
  p <- parse_remote_url("https://github.com/user/repo/tree/v1.2.3/pkg")
  expect_equal(p$ref, "v1.2.3")
  expect_equal(p$subdir, "pkg")

  p <- parse_remote_url("https://github.com/user/repo/tree/v1.2.3.4/pkg")
  expect_equal(p$ref, "v1.2.3.4")
})

test_that("parse_remote_url parses GitLab URLs correctly", {
  p <- parse_remote_url("https://gitlab.com/user/repo/-/tree/main/subdir")
  expect_equal(p$type, "gitlab")
  expect_equal(p$user, "user")
  expect_equal(p$repo, "repo")
  expect_equal(p$ref, "main")
  expect_equal(p$subdir, "subdir")
})

# Integration tests - only run when online
test_that("resolve_remote_pkg downloads GitHub package correctly", {
  skip_if_offline()
  skip_on_cran()
  if (!requireNamespace("pak", quietly = TRUE)) skip("pak not installed")

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
  if (!requireNamespace("pak", quietly = TRUE)) skip("pak not installed")

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
  if (!requireNamespace("pak", quietly = TRUE)) skip("pak not installed")

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

test_that("resolve_remote_pkg handles invalid arguments", {
  expect_error(resolve_remote_pkg(123), "must be a single character string")
  expect_error(resolve_remote_pkg(c("a", "b")), "must be a single character string")
})

test_that("resolve_remote_pkg handles pak download errors", {
  skip_if_not_installed("pak")
  local_mocked_bindings(
    pkg_download = function(...) stop("Network error"),
    .package = "pak"
  )
  expect_error(resolve_remote_pkg("user/repo"), "Failed to download remote package")
})

test_that("resolve_remote_pkg handles missing DESCRIPTION", {
  skip_if_not_installed("pak")
  cache <- tempfile()
  dir.create(cache)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)

  # Mock a successful download but empty extraction
  local_mocked_bindings(
    pkg_download = function(pkg, dest_dir, ...) {
      tar_path <- file.path(dest_dir, "empty.tar.gz")
      # Create dummy tarball
      empty_dir <- file.path(cache, "empty")
      dir.create(empty_dir)
      withr::with_dir(cache, utils::tar(tar_path, "empty", tar = "internal"))
      data.frame(package="empty", version="1.0", fulltarget=tar_path, stringsAsFactors=FALSE)
    },
    .package = "pak"
  )

  expect_error(resolve_remote_pkg("user/repo", cache_path = cache), "no DESCRIPTION found")
})

test_that("resolve_remote_pkg handles missing subdirectories", {
  skip_if_not_installed("pak")
  cache <- tempfile()
  dir.create(cache)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)

  local_mocked_bindings(
    pkg_download = function(pkg, dest_dir, ...) {
      tar_path <- file.path(dest_dir, "pkg.tar.gz")
      pkg_dir <- file.path(cache, "pkg")
      dir.create(pkg_dir)
      writeLines("Package: pkg", file.path(pkg_dir, "DESCRIPTION"))
      withr::with_dir(cache, utils::tar(tar_path, "pkg", tar = "internal"))
      data.frame(package="pkg", version="1.0", fulltarget=tar_path, stringsAsFactors=FALSE)
    },
    .package = "pak"
  )

  expect_error(resolve_remote_pkg("user/repo/missing", cache_path = cache), "Specified subdirectory 'missing' not found")
})

test_that("pak_row_value handles missing or NULL columns", {
  row <- data.frame(a = 1, stringsAsFactors = FALSE)
  expect_true(is.na(pak_row_value(row, "missing")))
  
  row_null <- list(a = NULL) # data frames can't really have NULL cells easily, but list-cols or malformed ones can
  expect_true(is.na(pak_row_value(as.data.frame(row_null), "a")))
})

test_that("is_remote_reference handles edge cases", {
  expect_false(is_remote_reference(character(0)))
  expect_false(is_remote_reference(c("user/repo", "other/repo")))
})

test_that("parse_remote_url handles generic URLs as git::", {
  p <- parse_remote_url("https://example.com/repo.git")
  expect_equal(p$type, "git")
  expect_equal(p$original, "https://example.com/repo.git")
})

test_that("parse_remote_ref preserves original reference with prefix and ref", {
  p <- parse_remote_ref("bioc::Biobase@RELEASE_3_18")
  expect_equal(p$original, "bioc::Biobase@RELEASE_3_18")
  expect_equal(remote_display_name(p), "bioc::Biobase@RELEASE_3_18")

  p2 <- parse_remote_ref("github::user/repo@main")
  expect_equal(p2$original, "github::user/repo@main")

  p3 <- parse_remote_ref("user/repo/subdir@v1.0.0")
  expect_equal(p3$original, "user/repo/subdir@v1.0.0")
})

test_that("remote_display_name handles NA users and original refs", {
  p <- list(type = "git", original = "https://example.com/repo", user = NA_character_)
  expect_equal(remote_display_name(p), "https://example.com/repo")
  
  p_bioc <- list(type = "bioc", repo = "Biobase", user = NA_character_)
  expect_equal(remote_display_name(p_bioc), "Biobase")
})

test_that("find_pkg_dir finds shallowest DESCRIPTION file", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  
  # Deep DESCRIPTION
  deep_dir <- file.path(tmp, "tests", "testthat")
  dir.create(deep_dir, recursive = TRUE)
  writeLines("Package: test", file.path(deep_dir, "DESCRIPTION"))
  
  # Shallow DESCRIPTION (the real package)
  pkg_dir <- file.path(tmp, "r-package")
  dir.create(pkg_dir)
  writeLines("Package: real", file.path(pkg_dir, "DESCRIPTION"))
  
  found <- find_pkg_dir(tmp)
  expect_equal(normalizePath(found), normalizePath(pkg_dir))
})

test_that("resolve_remote_pkg falls back to full download on pak failure", {
  skip_if_not_installed("pak")
  cache <- tempfile()
  dir.create(cache)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  
  # Mock pak failure
  local_mocked_bindings(
    pkg_download = function(...) stop("Resolution has errors"),
    .package = "pak"
  )
  
  # Mock download_tarball
  local_mocked_bindings(
    download_tarball = function(parsed, dest_dir) {
      tar_path <- file.path(dest_dir, "repo.tar.gz")
      pkg_dir <- file.path(tempdir(), "repo_root")
      dir.create(file.path(pkg_dir, "pkg"), recursive = TRUE)
      writeLines("Package: subpkg", file.path(pkg_dir, "pkg", "DESCRIPTION"))
      withr::with_dir(tempdir(), utils::tar(tar_path, "repo_root", tar = "internal"))
      unlink(pkg_dir, recursive = TRUE)
      tar_path
    },
    .package = "rdocdump"
  )
  
  info <- resolve_remote_pkg("user/repo", cache_path = cache)
  expect_equal(info$pkg_name, "subpkg")
  expect_match(info$pkg_path, "pkg$")
  expect_true(file.exists(file.path(info$pkg_path, "DESCRIPTION")))
})

test_that("resolve_remote_pkg falls back to git clone if tarball fails", {
  skip_if_not_installed("pak")
  if (Sys.which("git") == "") skip("git not available")
  
  cache <- tempfile()
  dir.create(cache)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  
  # Mock pak failure
  local_mocked_bindings(
    pkg_download = function(...) stop("Resolution has errors"),
    .package = "pak"
  )
  
  # Mock download_tarball to fail
  local_mocked_bindings(
    download_tarball = function(...) NULL,
    .package = "rdocdump"
  )
  
  # Mock git_clone_repo
  local_mocked_bindings(
    git_clone_repo = function(parsed, dest_dir) {
      clone_dir <- file.path(dest_dir, "fallback_clone")
      # Create a wrapper and a subdirectory to avoid simple flattening 
      # that would move everything to the root of 'extracted'.
      pkg_dir <- file.path(clone_dir, "repo-root", "r-pkg")
      dir.create(pkg_dir, recursive = TRUE)
      writeLines("Package: clonedpkg", file.path(pkg_dir, "DESCRIPTION"))
      clone_dir
    }
,
    .package = "rdocdump"
  )
  
  info <- resolve_remote_pkg("user/repo", cache_path = cache)
  expect_equal(info$pkg_name, "clonedpkg")
  expect_match(info$pkg_path, "r-pkg$")
})

test_that("resolve_remote_pkg auto-discovers r5r in subdirectory (LIVE)", {
  skip_if_offline()
  skip_on_cran()
  if (!requireNamespace("pak", quietly = TRUE)) skip("pak not installed")

  cache <- tempfile()
  dir.create(cache)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)

  # ipeaGIT/r5r has the R package in 'r-package/'
  info <- resolve_remote_pkg("ipeaGIT/r5r", cache_path = cache)

  expect_equal(info$pkg_name, "r5r")
  expect_match(info$pkg_path, "r-package$")
  expect_true(file.exists(file.path(info$pkg_path, "DESCRIPTION")))
})

test_that("build_pak_remote_ref handles GitLab subdirectories correctly", {
  # Standard format
  expect_equal(
    build_pak_remote_ref(parse_remote_ref("gitlab::user/repo/sub")),
    "gitlab::user/repo/-/sub"
  )
  # pak format (already contains /-/)
  expect_equal(
    build_pak_remote_ref(parse_remote_ref("gitlab::user/repo/-/sub")),
    "gitlab::user/repo/-/sub"
  )
})

test_that("download_tarball constructs correct headers for GitHub", {
  skip_if_not_installed("withr")
  parsed <- list(type = "github", user = "user", repo = "repo", ref = NULL)
  
  withr::with_envvar(c(GITHUB_PAT = "secret"), {
    # We test the internal logic by mocking download.file
    local_mocked_bindings(
      download.file = function(url, destfile, headers, ...) {
        expect_equal(headers[["Authorization"]], "token secret")
        writeLines("dummy", destfile)
        0 # success
      },
      .package = "utils"
    )
    res <- download_tarball(parsed, tempdir())
    expect_true(file.exists(res))
    unlink(res)
  })
})

test_that("find_pkg_dir correctly handles long shallow names vs short deep names", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  
  # Deep DESCRIPTION with short names
  deep_dir <- file.path(tmp, "a", "b", "c")
  dir.create(deep_dir, recursive = TRUE)
  writeLines("Package: deep", file.path(deep_dir, "DESCRIPTION"))
  
  # Shallow DESCRIPTION with a very long name
  pkg_dir <- file.path(tmp, "this-is-a-very-long-package-directory-name")
  dir.create(pkg_dir)
  writeLines("Package: shallow", file.path(pkg_dir, "DESCRIPTION"))
  
  # Ensure the test is actually testing the edge case
  expect_true(nchar(pkg_dir) > nchar(deep_dir))
  
  found <- find_pkg_dir(tmp)
  expect_equal(normalizePath(found), normalizePath(pkg_dir))
})

