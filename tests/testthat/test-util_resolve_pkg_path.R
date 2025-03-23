test_that("resolve_pkg_path identifies installed packages correctly", {
  # Use a base package, such as "stats", which is always installed.
  pkg_info <- resolve_pkg_path("stats", force_fetch = FALSE)

  expect_true(pkg_info$is_installed)
  expect_equal(pkg_info$pkg_name, "stats")
  expect_equal(pkg_info$pkg_path, find.package("stats"))
})

test_that("resolve_pkg_path identifies a source package directory", {
  # Create a dummy source package directory.
  temp_pkg <- tempfile("dummy_pkg")
  dir.create(temp_pkg)
  writeLines("Package: dummy\nVersion: 1.0", file.path(temp_pkg, "DESCRIPTION"))

  # Create a 'man' directory with a dummy Rd file.
  man_dir <- file.path(temp_pkg, "man")
  dir.create(man_dir)
  dummy_rd <- "\\name{dummy}\n\\alias{dummy}\n\\title{Dummy Function}\n\\description{A dummy function.}\n"
  writeLines(dummy_rd, file.path(man_dir, "dummy.Rd"))

  pkg_info <- resolve_pkg_path(temp_pkg)
  expect_false(pkg_info$is_installed)
  expect_equal(pkg_info$pkg_path, temp_pkg)

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
  old_wd <- getwd()
  setwd(dirname(dummy_pkg))
  # Create the archive without extra arguments.
  utils::tar(tarfile = tar_path, files = basename(dummy_pkg), tar = "internal")
  setwd(old_wd)

  expect_true(file.exists(tar_path)) # Ensure archive exists

  # Call the actual function with an explicit cache_path to force extraction.
  pkg_info <- resolve_pkg_path(tar_path, cache_path = tempdir())

  expect_false(pkg_info$is_installed)
  # With cache_path specified, get_extract_dir() should return a directory path.
  expect_true(is.character(pkg_info$pkg_path))
  expect_true(is.character(pkg_info$extracted_path))
  expect_equal(pkg_info$pkg_path, pkg_info$extracted_path)
  expect_true(dir.exists(pkg_info$pkg_path))

  # Clean up.
  unlink(pkg_info$pkg_path, recursive = TRUE)
  unlink(tar_path)
})


test_that("resolve_pkg_path fetches package from CRAN", {
  skip_on_cran()
  skip_if_offline()

  package <- "ini" # choosing a minimal size stable package
  old_repos <- getOption("repos")
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  pkg_info <- resolve_pkg_path(
    package,
    cache_path = tempdir(),
    force_fetch = TRUE
  )
  options(repos = old_repos)

  expect_true(file.exists(pkg_info$tar_path))
  expect_true(dir.exists(pkg_info$pkg_path))
  unlink(pkg_info$pkg_path, recursive = TRUE)
  unlink(pkg_info$tar_path)
})
