test_that("rdd_to_txt works for installed packages", {
  # Use an installed package such as "stats".
  txt <- rdd_to_txt("stats", keep_files = "none")

  expect_true(nchar(txt) > 0)
  expect_match(txt, "Function:")
})

test_that("rdd_to_txt with invalid keep_files value errors", {
  expect_error(
    rdd_to_txt("stats", keep_files = "invalid"),
    "Invalid value for keep_files"
  )
})

test_that("rdd_to_txt writes output to file when file parameter is provided", {
  temp_file <- tempfile("rdoc_output", fileext = ".txt")
  on.exit(unlink(temp_file))

  txt <- rdd_to_txt("stats", file = temp_file, keep_files = "none")
  expect_true(file.exists(temp_file))
  content <- readLines(temp_file)
  expect_true(length(content) > 0)
})

test_that("rdd_to_txt combines DESCRIPTION, Rd documentation and vignettes", {
  pkg_dir <- tempfile("pkg_")
  dir.create(pkg_dir)

  # Create a minimal DESCRIPTION file so that combine_rd() works.
  desc_file <- file.path(pkg_dir, "DESCRIPTION")
  writeLines(c("Package: testpkg", "Version: 0.1"), desc_file)

  # Create a dummy 'man' directory with an Rd file (for documentation)
  man_dir <- file.path(pkg_dir, "man")
  dir.create(man_dir)
  rd_file <- file.path(man_dir, "test.Rd")
  writeLines("\\name{test}\n\\alias{test}\n\\title{Test Function}", rd_file)

  # Create a 'vignettes' directory with a sample vignette file
  vignette_dir <- file.path(pkg_dir, "vignettes")
  dir.create(vignette_dir)
  vign_file <- file.path(vignette_dir, "example.md")
  writeLines("This is a vignette", vign_file)

  # Stub for resolve_pkg_path() to simply return our fake package directory.
  resolve_pkg_path <- function(pkg, cache_path, force_fetch) {
    list(
      pkg_path = pkg,
      is_installed = FALSE,
      pkg_name = "testpkg",
      tar_path = NULL,
      extracted_path = NULL
    )
  }

  # Attach the stubbed function to the global environment for testing.
  assign("resolve_pkg_path", resolve_pkg_path, envir = .GlobalEnv)

  out <- rdd_to_txt(pkg_dir, keep_files = "none")

  # Adjusted expectations:
  # Check that the output includes the DESCRIPTION content.
  expect_match(
    out,
    "Package: testpkg",
    info = "Output should include DESCRIPTION header with package name"
  )
  # Check that the output includes the Rd documentation header for the Rd file.
  expect_match(
    out,
    "Function: test\\(\\)",
    info = "Output should include Rd documentation header"
  )
  # Check that the output includes the vignette header.
  expect_match(
    out,
    "Vignette: example\\.md",
    info = "Output should include vignette header"
  )
  # Check that the output includes the vignette content.
  expect_match(
    out,
    "This is a vignette",
    info = "Output should include vignette content"
  )
})
