test_that("combine_vignettes works with a 'vignettes' directory", {
  pkg_dir <- tempfile("pkg_")
  dir.create(pkg_dir)
  vignette_dir <- file.path(pkg_dir, "vignettes")
  dir.create(vignette_dir)

  # Create a sample vignette file in vignettes directory
  vign_file <- file.path(vignette_dir, "example.Rmd")
  writeLines("This is an Rmd vignette", vign_file)

  out <- combine_vignettes(pkg_dir)
  expect_match(
    out,
    "Vignette: example\\.Rmd",
    info = "Should include the header with file name"
  )
  expect_match(
    out,
    "This is an Rmd vignette",
    info = "Should include the content of the vignette"
  )
})

test_that("combine_vignettes falls back to 'doc' directory if 'vignettes' is absent", {
  pkg_dir <- tempfile("pkg_")
  dir.create(pkg_dir)
  doc_dir <- file.path(pkg_dir, "doc")
  dir.create(doc_dir)

  # Create a sample vignette file in doc directory
  vign_file <- file.path(doc_dir, "example.qmd")
  writeLines("This is a qmd vignette", vign_file)

  out <- combine_vignettes(pkg_dir)
  expect_match(
    out,
    "Vignette: example\\.qmd",
    info = "Should include the header with file name from the doc folder"
  )
  expect_match(
    out,
    "This is a qmd vignette",
    info = "Should include the content of the vignette"
  )
})

test_that("combine_vignettes returns empty string if neither 'vignettes' nor 'doc' exists", {
  pkg_dir <- tempfile("pkg_")
  dir.create(pkg_dir)

  out <- suppressWarnings(combine_vignettes(pkg_dir))
  expect_equal(
    out,
    "",
    info = "Should return an empty string when no relevant directory is found"
  )
})
