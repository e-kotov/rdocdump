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
