test_that("combine_rd works for installed packages", {
  # For installed packages, pass the installed package path and package name.
  rd_text <- combine_rd(
    find.package("stats"),
    is_installed = TRUE,
    pkg_name = "stats"
  )

  expect_true(nchar(rd_text) > 0)
  expect_match(rd_text, "Function:")
})

test_that("combine_rd works for source packages", {
  # Create a dummy source package with minimal Rd documentation.
  temp_pkg <- tempfile("dummy_pkg")
  dir.create(temp_pkg)
  writeLines("Package: dummy\nVersion: 1.0", file.path(temp_pkg, "DESCRIPTION"))

  man_dir <- file.path(temp_pkg, "man")
  dir.create(man_dir)
  dummy_rd <- "\\name{dummy}\n\\alias{dummy}\n\\title{Dummy Function}\n\\description{A dummy function.}\n"
  writeLines(dummy_rd, file.path(man_dir, "dummy.Rd"))

  rd_text <- combine_rd(temp_pkg, is_installed = FALSE)

  expect_true(nchar(rd_text) > 0)
  expect_match(rd_text, "DESCRIPTION:")

  unlink(temp_pkg, recursive = TRUE)
})

test_that("combine_rd errors when DESCRIPTION file is missing", {
  temp_pkg <- tempfile("pkg_no_desc")
  dir.create(temp_pkg)

  # Do not create a DESCRIPTION file.
  man_dir <- file.path(temp_pkg, "man")
  dir.create(man_dir)
  writeLines(
    "\\name{nodef}\n\\alias{nodef}\n\\title{No Def Function}",
    file.path(man_dir, "nodef.Rd")
  )

  expect_error(
    combine_rd(temp_pkg, is_installed = FALSE),
    "Files 'DESCRIPTION' and 'DESCRIPTION.in' are missing."
  )

  unlink(temp_pkg, recursive = TRUE)
})
