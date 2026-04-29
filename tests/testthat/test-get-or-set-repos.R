test_that("rdd_set_repos and rdd_get_repos work", {
  old_repos <- getOption("rdocdump.repos")

  new_repos <- c(CRAN = "https://cloud.r-project.org", CUSTOM = "https://my.repo.org")
  rdd_set_repos(new_repos)

  expect_equal(getOption("rdocdump.repos"), new_repos)
  expect_equal(rdd_get_repos(), new_repos)

  # Test error on invalid input
  expect_error(rdd_set_repos(123), "repos must be a character vector")

  # Restore
  options(rdocdump.repos = old_repos)
})
