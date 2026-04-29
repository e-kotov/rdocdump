test_that("cleanup_files respects keep_files policy", {
  # Create dummy files
  tar_path <- tempfile(fileext = ".tar.gz")
  writeLines("dummy tar", tar_path)

  extract_parent <- tempfile("extract_parent")
  dir.create(extract_parent)
  extracted_path <- file.path(extract_parent, "pkg")
  dir.create(extracted_path)
  writeLines("dummy file", file.path(extracted_path, "DESCRIPTION"))

  pkg_info <- list(
    tar_path = tar_path,
    extracted_path = extracted_path
  )

  # keep_files = "none" (default)
  # Actually, cleanup_files is called with the policy.
  # Let's test "none"
  # I'll use copies for each test case
  t1 <- tempfile(fileext = ".tar.gz")
  file.copy(tar_path, t1)
  e1_p <- tempfile()
  dir.create(e1_p)
  e1 <- file.path(e1_p, "pkg")
  dir.create(e1)
  file.copy(file.path(extracted_path, "DESCRIPTION"), e1)

  cleanup_files(list(tar_path = t1, extracted_path = e1), "none")
  expect_false(file.exists(t1))
  expect_false(dir.exists(e1_p))

  # keep_files = "tgz"
  t2 <- tempfile(fileext = ".tar.gz")
  file.copy(tar_path, t2)
  e2_p <- tempfile()
  dir.create(e2_p)
  e2 <- file.path(e2_p, "pkg")
  dir.create(e2)

  cleanup_files(list(tar_path = t2, extracted_path = e2), "tgz")
  expect_true(file.exists(t2))
  expect_false(dir.exists(e2_p))
  unlink(t2)

  # keep_files = "extracted"
  t3 <- tempfile(fileext = ".tar.gz")
  file.copy(tar_path, t3)
  e3_p <- tempfile()
  dir.create(e3_p)
  e3 <- file.path(e3_p, "pkg")
  dir.create(e3)

  cleanup_files(list(tar_path = t3, extracted_path = e3), "extracted")
  expect_false(file.exists(t3))
  expect_true(dir.exists(e3_p))
  unlink(e3_p, recursive = TRUE)

  # keep_files = "both"
  t4 <- tempfile(fileext = ".tar.gz")
  file.copy(tar_path, t4)
  e4_p <- tempfile()
  dir.create(e4_p)
  e4 <- file.path(e4_p, "pkg")
  dir.create(e4)

  cleanup_files(list(tar_path = t4, extracted_path = e4), "both")
  expect_true(file.exists(t4))
  expect_true(dir.exists(e4_p))
  unlink(t4)
  unlink(e4_p, recursive = TRUE)

  # Cleanup base dummy files
  unlink(tar_path)
  unlink(extract_parent, recursive = TRUE)
})
