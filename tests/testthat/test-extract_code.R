test_that("rdd_extract_code handles missing pkg_name for installed package", {
  local_mocked_bindings(
    resolve_pkg_path = function(...) {
      list(is_installed = TRUE, pkg_name = NULL)
    },
    .package = "rdocdump"
  )
  expect_error(
    rdd_extract_code("anypkg"),
    "Installed package does not provide pkg_name information"
  )
})

test_that("rdd_extract_code works with file argument", {
  tmp_file <- tempfile(fileext = ".txt")
  # Use a small package for testing
  code_path <- rdd_extract_code("splines", file = tmp_file)

  expect_equal(code_path, tmp_file)
  expect_true(file.exists(tmp_file))

  content <- readLines(tmp_file)
  expect_true(any(grepl("Function: ", content)))

  unlink(tmp_file)
})

test_that("rdd_extract_code includes tests and roxygen", {
  # Create a dummy source package directory.
  tmp_pkg <- tempfile("dummy_pkg")
  dir.create(tmp_pkg)
  writeLines("Package: dummy\nVersion: 1.0", file.path(tmp_pkg, "DESCRIPTION"))

  # Create an R directory with a file containing roxygen.
  r_dir <- file.path(tmp_pkg, "R")
  dir.create(r_dir)
  writeLines(
    c("#' My function", "f <- function() { 1 }"),
    file.path(r_dir, "f.R")
  )

  # Create a tests directory with an R file.
  tests_dir <- file.path(tmp_pkg, "tests")
  dir.create(tests_dir)
  writeLines(
    "test_that('f works', { expect_equal(f(), 1) })",
    file.path(tests_dir, "test-f.R")
  )

  code <- rdd_extract_code(
    tmp_pkg,
    include_tests = TRUE,
    include_roxygen = TRUE
  )

  expect_true(is.character(code))
  expect_true(any(grepl("^#'", strsplit(code, "\n")[[1]])))
  expect_true(any(grepl("Test File: test-f.R", strsplit(code, "\n")[[1]])))

  unlink(tmp_pkg, recursive = TRUE)
})

test_that("rdd_extract_code handles missing R directory", {
  tmp_pkg <- tempdir()
  pkg_dir <- file.path(tmp_pkg, "no_r_pkg")
  dir.create(pkg_dir)
  writeLines("Package: noRPkg\nVersion: 0.1.0", file.path(pkg_dir, "DESCRIPTION"))

  expect_warning(
    rdd_extract_code(pkg_dir),
    "R directory not found in the package source"
  )

  unlink(pkg_dir, recursive = TRUE)
})
