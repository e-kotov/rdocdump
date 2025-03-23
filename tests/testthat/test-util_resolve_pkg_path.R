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
