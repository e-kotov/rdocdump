test_that("resolve_pkg_path identifies installed packages correctly", {
  # Use a base package, such as "stats", which is always installed.
  pkg_info <- resolve_pkg_path("stats", force_fetch = FALSE)

  expect_true(pkg_info$is_installed)
  expect_equal(pkg_info$pkg_name, "stats")
  expect_equal(pkg_info$pkg_path, find.package("stats"))
  # pkg_version is populated from DESCRIPTION when available
  expect_true(is.null(pkg_info$pkg_version) || nzchar(pkg_info$pkg_version))
})

test_that("resolve_pkg_path identifies a source package directory", {
  # Create a dummy source package directory.
  temp_pkg <- tempfile("dummy_pkg")
  dir.create(temp_pkg)
  writeLines("Package: dummy\nVersion: 1.0", file.path(temp_pkg, "DESCRIPTION"))

  # Create a 'man' directory with a dummy Rd file.
  man_dir <- file.path(temp_pkg, "man")
  dir.create(man_dir)
  dummy_rd <- paste(
    "\\name{dummy}",
    "\\alias{dummy}",
    "\\title{Dummy Function}",
    "\\description{A dummy function.}",
    sep = "\n"
  )
  writeLines(dummy_rd, file.path(man_dir, "dummy.Rd"))

  pkg_info <- resolve_pkg_path(temp_pkg)
  expect_false(pkg_info$is_installed)
  expect_equal(pkg_info$pkg_path, temp_pkg)
  expect_equal(pkg_info$pkg_name, "dummy")
  expect_equal(pkg_info$pkg_version, "1.0")

  unlink(temp_pkg, recursive = TRUE)
})


test_that("resolve_pkg_path handles tar.gz archive file correctly", {
  # Create a temporary dummy package directory with minimal files.
  dummy_pkg <- tempfile("dummy_pkg")
  dir.create(dummy_pkg)
  writeLines(
    "Package: dummy\nVersion: 1.0",
    file.path(dummy_pkg, "DESCRIPTION")
  )
  man_dir <- file.path(dummy_pkg, "man")
  dir.create(man_dir)
  writeLines(
    "\\name{dummy}\n\\alias{dummy}\n\\title{Dummy Function}",
    file.path(man_dir, "dummy.Rd")
  )

  # Create a tar.gz archive of the dummy package.
  tar_path <- tempfile("dummy_pkg", fileext = ".tar.gz")
  withr::with_dir(dirname(dummy_pkg), {
    # Create the archive without extra arguments.
    utils::tar(
      tarfile = tar_path,
      files = basename(dummy_pkg),
      tar = "internal"
    )
  })

  expect_true(file.exists(tar_path)) # Ensure it exists

  # Call the actual function with an explicit cache_path to force extraction.
  pkg_info <- resolve_pkg_path(tar_path, cache_path = tempdir())

  expect_false(pkg_info$is_installed)
  # With cache_path specified, get_extract_dir() should return a directory path.
  expect_true(is.character(pkg_info$pkg_path))
  expect_true(is.character(pkg_info$extracted_path))
  expect_equal(pkg_info$pkg_path, pkg_info$extracted_path)
  expect_true(dir.exists(pkg_info$pkg_path))
  expect_equal(pkg_info$pkg_name, "dummy")
  expect_equal(pkg_info$pkg_version, "1.0")

  # Clean up.
  unlink(pkg_info$pkg_path, recursive = TRUE)
  unlink(tar_path)
})

test_that("resolve_pkg_path handles invalid archive file", {
  tmp_file <- tempfile(fileext = ".txt")
  writeLines("not a tarball", tmp_file)

  expect_error(
    resolve_pkg_path(tmp_file),
    "not a recognized package archive"
  )
  unlink(tmp_file)
})

test_that("resolve_pkg_path handles malformed tarball name", {
  # Create a tar.gz with a name that doesn't follow pkg_version.tar.gz
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  dummy_pkg <- file.path(tmp_dir, "dummy")
  dir.create(dummy_pkg)
  writeLines("Package: dummy", file.path(dummy_pkg, "DESCRIPTION"))

  tar_path <- file.path(tmp_dir, "badname.tar.gz")
  withr::with_dir(tmp_dir, {
    utils::tar(tarfile = tar_path, files = "dummy", tar = "internal")
  })

  expect_error(
    resolve_pkg_path(tar_path),
    "Tarball filename does not conform to the expected pattern"
  )

  unlink(tmp_dir, recursive = TRUE)
})


test_that("resolve_pkg_path passes repos to pak downloads", {
  skip_if_not_installed("pak")

  cache <- tempfile()
  dir.create(cache)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)

  seen_repos <- NULL
  local_mocked_bindings(
    pkg_download = function(pkg, dest_dir, dependencies, platforms) {
      seen_repos <<- getOption("repos")

      pkg_dir <- file.path(cache, "ini")
      dir.create(pkg_dir)
      writeLines("Package: ini\nVersion: 1.0.0", file.path(pkg_dir, "DESCRIPTION"))

      tar_path <- file.path(dest_dir, "src", "contrib", "ini_1.0.0.tar.gz")
      dir.create(dirname(tar_path), recursive = TRUE)
      withr::with_dir(cache, {
        utils::tar(tar_path, "ini", tar = "internal")
      })

      data.frame(
        ref = pkg,
        direct = TRUE,
        package = "ini",
        version = "1.0.0",
        target = "src/contrib/ini_1.0.0.tar.gz",
        fulltarget = tar_path,
        stringsAsFactors = FALSE
      )
    },
    .package = "pak"
  )

  requested_repos <- c(CRAN = "https://example.test/cran")
  old_repos <- getOption("repos")
  options(repos = c(CRAN = "https://wrong.example/cran"))
  on.exit(options(repos = old_repos), add = TRUE)

  pkg_info <- resolve_pkg_path(
    "ini",
    cache_path = cache,
    force_fetch = TRUE,
    repos = requested_repos
  )

  expect_equal(seen_repos, requested_repos)
  expect_true(file.exists(file.path(pkg_info$pkg_path, "DESCRIPTION")))
})


test_that("resolve_pkg_path errors clearly when pak omits package column", {
  skip_if_not_installed("pak")

  cache <- tempfile()
  dir.create(cache)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)

  local_mocked_bindings(
    pkg_download = function(pkg, dest_dir, dependencies, platforms) {
      pkg_dir <- file.path(cache, "ini")
      dir.create(pkg_dir)
      writeLines("Package: ini\nVersion: 1.0.0", file.path(pkg_dir, "DESCRIPTION"))

      tar_path <- file.path(dest_dir, "src", "contrib", "ini_1.0.0.tar.gz")
      dir.create(dirname(tar_path), recursive = TRUE)
      withr::with_dir(cache, {
        utils::tar(tar_path, "ini", tar = "internal")
      })

      # No 'package' column on purpose.
      data.frame(
        ref = pkg,
        direct = TRUE,
        version = "1.0.0",
        target = "src/contrib/ini_1.0.0.tar.gz",
        fulltarget = tar_path,
        stringsAsFactors = FALSE
      )
    },
    .package = "pak"
  )

  expect_error(
    resolve_pkg_path("ini", cache_path = cache, force_fetch = TRUE),
    "pak did not return a package name"
  )
})


test_that("resolve_pkg_path errors when pak omits package version", {
  skip_if_not_installed("pak")

  cache <- tempfile()
  dir.create(cache)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)

  local_mocked_bindings(
    pkg_download = function(pkg, dest_dir, dependencies, platforms) {
      pkg_dir <- file.path(cache, "ini")
      dir.create(pkg_dir)
      writeLines("Package: ini\nVersion: 1.0.0", file.path(pkg_dir, "DESCRIPTION"))

      tar_path <- file.path(dest_dir, "src", "contrib", "ini_1.0.0.tar.gz")
      dir.create(dirname(tar_path), recursive = TRUE)
      withr::with_dir(cache, {
        utils::tar(tar_path, "ini", tar = "internal")
      })

      data.frame(
        ref = pkg,
        direct = TRUE,
        package = "ini",
        version = NA_character_,
        target = "src/contrib/ini_1.0.0.tar.gz",
        fulltarget = tar_path,
        stringsAsFactors = FALSE
      )
    },
    .package = "pak"
  )

  expect_error(
    resolve_pkg_path("ini", cache_path = cache, force_fetch = TRUE),
    "pak did not return a package version"
  )
})


test_that("resolve_pkg_path fetches requested CRAN package with stale cache", {
  skip_on_cran()
  skip_if_offline()

  package <- "ini" # choosing a minimal size stable package
  cache <- tempfile()
  dir.create(cache)
  stale_pkg <- file.path(cache, "stale")
  dir.create(stale_pkg)
  writeLines("Package: stale\nVersion: 1.0.0", file.path(stale_pkg, "DESCRIPTION"))
  stale_archive <- file.path(cache, "stale_1.0.0.tar.gz")
  withr::with_dir(cache, {
    utils::tar(stale_archive, "stale", tar = "internal")
  })

  old_repos <- getOption("repos")
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  pkg_info <- resolve_pkg_path(
    package,
    cache_path = cache,
    force_fetch = TRUE
  )
  options(repos = old_repos)

  expect_true(file.exists(pkg_info$tar_path))
  expect_true(dir.exists(pkg_info$pkg_path))
  expect_false(identical(basename(pkg_info$tar_path), basename(stale_archive)))
  expect_match(
    readLines(file.path(pkg_info$pkg_path, "DESCRIPTION"), n = 1),
    "Package: ini"
  )

  unlink(cache, recursive = TRUE)
})

test_that("is_binary_pkg correctly identifies binary packages", {
  # Create a dummy source package
  src_pkg <- tempfile("src_pkg")
  dir.create(src_pkg)
  dir.create(file.path(src_pkg, "R"))
  writeLines("f <- function() {}", file.path(src_pkg, "R", "f.R"))
  expect_false(is_binary_pkg(src_pkg))

  # Create a dummy binary package (with Meta directory)
  bin_pkg_meta <- tempfile("bin_pkg_meta")
  dir.create(bin_pkg_meta)
  dir.create(file.path(bin_pkg_meta, "Meta"))
  expect_true(is_binary_pkg(bin_pkg_meta))

  # Create a dummy binary package (with .rdx/.rdb in R/)
  bin_pkg_r <- tempfile("bin_pkg_r")
  dir.create(bin_pkg_r)
  dir.create(file.path(bin_pkg_r, "R"))
  writeLines("", file.path(bin_pkg_r, "R", "pkg.rdx"))
  expect_true(is_binary_pkg(bin_pkg_r))

  # Create a dummy binary package (with .rdx/.rdb in help/)
  bin_pkg_help <- tempfile("bin_pkg_help")
  dir.create(bin_pkg_help)
  dir.create(file.path(bin_pkg_help, "help"))
  writeLines("", file.path(bin_pkg_help, "help", "pkg.rdb"))
  expect_true(is_binary_pkg(bin_pkg_help))

  unlink(c(src_pkg, bin_pkg_meta, bin_pkg_r, bin_pkg_help), recursive = TRUE)
})

test_that("resolve_pkg_path fails on binary tarballs", {
  # Create a dummy binary package directory
  bin_pkg <- tempfile("bin_pkg_1.0")
  dir.create(bin_pkg)
  writeLines("Package: binpkg\nVersion: 1.0", file.path(bin_pkg, "DESCRIPTION"))
  dir.create(file.path(bin_pkg, "Meta"))

  # Create a tar.gz archive
  tar_path <- tempfile("binpkg_1.0", fileext = ".tar.gz")
  withr::with_dir(dirname(bin_pkg), {
    utils::tar(tarfile = tar_path, files = basename(bin_pkg), tar = "internal")
  })

  expect_error(
    resolve_pkg_path(tar_path),
    "appears to be a pre-built binary rather than a source package"
  )

  unlink(bin_pkg, recursive = TRUE)
  unlink(tar_path)
})
