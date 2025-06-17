# Tests for the rdd_to_txt function in the rdocdump package
fake_resolve_pkg_path <- function(pkg, cache_path, force_fetch, repos) {
  list(
    pkg_path = pkg,
    is_installed = FALSE,
    pkg_name = "testpkg",
    tar_path = NULL,
    extracted_path = NULL
  )
}

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

  local_mocked_bindings(
    resolve_pkg_path = fake_resolve_pkg_path,
    .package = "rdocdump"
  )

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


test_that("rdd_to_txt outputs only documentation when content is 'docs'", {
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

  local_mocked_bindings(
    resolve_pkg_path = fake_resolve_pkg_path,
    .package = "rdocdump"
  )

  out_docs <- rdd_to_txt(pkg_dir, keep_files = "none", content = "docs")

  # Check that the output includes the DESCRIPTION and Rd documentation headers...
  expect_match(
    out_docs,
    "Package: testpkg",
    info = "Output should include DESCRIPTION header"
  )
  expect_match(
    out_docs,
    "Function: test\\(\\)",
    info = "Output should include Rd documentation header"
  )

  # ...but it should NOT include the vignette header or content.
  expect_false(
    grepl("Vignette: example\\.md", out_docs),
    info = "Output should not include vignette header"
  )
  expect_false(
    grepl("This is a vignette", out_docs),
    info = "Output should not include vignette content"
  )
})

test_that("rdd_to_txt outputs only vignettes when content is 'vignettes'", {
  pkg_dir <- tempfile("pkg_")
  dir.create(pkg_dir)

  # Create a minimal DESCRIPTION file.
  desc_file <- file.path(pkg_dir, "DESCRIPTION")
  writeLines(c("Package: testpkg", "Version: 0.1"), desc_file)

  # Create a dummy 'man' directory with an Rd file.
  man_dir <- file.path(pkg_dir, "man")
  dir.create(man_dir)
  rd_file <- file.path(man_dir, "test.Rd")
  writeLines("\\name{test}\n\\alias{test}\n\\title{Test Function}", rd_file)

  # Create a 'vignettes' directory with a sample vignette file.
  vignette_dir <- file.path(pkg_dir, "vignettes")
  dir.create(vignette_dir)
  vign_file <- file.path(vignette_dir, "example.md")
  writeLines("This is a vignette", vign_file)

  local_mocked_bindings(
    resolve_pkg_path = fake_resolve_pkg_path,
    .package = "rdocdump"
  )

  out_vignettes <- rdd_to_txt(
    pkg_dir,
    keep_files = "none",
    content = "vignettes"
  )

  # Check that the output includes the vignette header and content.
  expect_match(
    out_vignettes,
    "Vignette: example\\.md",
    info = "Output should include vignette header"
  )
  expect_match(
    out_vignettes,
    "This is a vignette",
    info = "Output should include vignette content"
  )

  # But it should NOT include the documentation header.
  expect_false(
    grepl("Function: test\\(\\)", out_vignettes),
    info = "Output should not include Rd documentation header"
  )
})

test_that("rdd_to_txt keeps tar.gz archive when keep_files is 'tgz'", {
  skip_on_cran()
  skip_if_offline()

  # Create a unique cache directory for this test.
  cache_dir <- tempfile("cache_tgz")
  dir.create(cache_dir)

  # Call rdd_to_txt on the "ini" package.
  # force_fetch = TRUE ensures it downloads from CRAN,
  # and keep_files = "tgz" should leave the tar.gz archive in the cache.
  old_repos <- getOption("repos")
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  out <- suppressWarnings(rdd_to_txt(
    "ini",
    force_fetch = TRUE,
    keep_files = "tgz",
    cache_path = cache_dir
  ))
  options(repos = old_repos)

  # Look for any tar.gz file in the cache directory.
  tar_files <- list.files(
    cache_dir,
    pattern = "\\.tar\\.gz$",
    full.names = TRUE
  )
  expect_true(length(tar_files) > 0)

  # Clean up the cache directory.
  unlink(cache_dir, recursive = TRUE)
})

test_that("rdd_to_txt keeps both tar.gz archive and extracted files when keep_files is 'both'", {
  skip_on_cran()
  skip_if_offline()

  # Create a unique cache directory.
  cache_dir <- tempfile("cache_both")
  dir.create(cache_dir)

  # Call rdd_to_txt on the "ini" package with keep_files = "both".
  old_repos <- getOption("repos")
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  out <- suppressWarnings(rdd_to_txt(
    "ini",
    force_fetch = TRUE,
    keep_files = "both",
    cache_path = cache_dir
  ))
  options(repos = old_repos)

  # Check that a tar.gz file exists in the cache.
  tar_files <- list.files(
    cache_dir,
    pattern = "\\.tar\\.gz$",
    full.names = TRUE
  )
  expect_true(length(tar_files) > 0)

  # For the extracted package: resolve_pkg_path creates an extraction directory
  # as file.path(cache_dir, <pkgname>, <version>). For package "ini", we expect a subdirectory
  # named "ini" to exist under cache_dir.
  extracted_dir <- file.path(cache_dir, "ini")
  expect_true(dir.exists(extracted_dir))

  # There should be at least one directory inside the "ini" folder.
  subdirs <- list.dirs(extracted_dir, recursive = FALSE, full.names = TRUE)
  expect_true(length(subdirs) > 0)

  # Clean up the cache directory.
  unlink(cache_dir, recursive = TRUE)
})
