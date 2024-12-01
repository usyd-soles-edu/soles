test_that("find_gradescope_file finds the correct file", {
  # Create temporary directory
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Create mock Gradescope file with required columns
  gradescope_content <- paste(
    "First Name,Last Name,SID,Email,Submission ID,Submission Time,Status",
    "John,Doe,1234567,john@example.com,98765,2023-10-01 14:30:00 +1100,Active",
    sep = "\n"
  )
  gradescope_file <- file.path(temp_dir, "gradescope_export.csv")
  writeLines(gradescope_content, gradescope_file)

  # Test finding the file
  result <- find_gradescope_file(temp_dir)
  expect_equal(result, gradescope_file)
})

test_that("find_gradescope_file selects most recent file by submission time", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Create two mock Gradescope files with different submission times
  older_content <- paste(
    "First Name,Last Name,SID,Email,Submission ID,Submission Time,Status",
    "John,Doe,1234567,john@example.com,98765,2023-10-01 14:30:00 +1100,Active",
    sep = "\n"
  )
  newer_content <- paste(
    "First Name,Last Name,SID,Email,Submission ID,Submission Time,Status",
    "John,Doe,1234567,john@example.com,98765,2023-10-02 14:30:00 +1100,Active",
    sep = "\n"
  )

  older_file <- file.path(temp_dir, "gradescope_export_1.csv")
  newer_file <- file.path(temp_dir, "gradescope_export_2.csv")
  writeLines(older_content, older_file)
  writeLines(newer_content, newer_file)

  # Test that file with most recent submission is selected
  result <- find_gradescope_file(temp_dir)
  expect_equal(result, newer_file)
})

test_that("find_gradescope_file errors on missing files", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  expect_error(
    find_gradescope_file(temp_dir),
    "No files found in directory"
  )
})

test_that("find_gradescope_file errors on invalid files", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Create file without required columns
  invalid_content <- "Column1,Column2,Column3"
  invalid_file <- file.path(temp_dir, "invalid.csv")
  writeLines(invalid_content, invalid_file)

  expect_error(
    find_gradescope_file(temp_dir),
    "No Gradescope file found \\(missing expected columns\\)"
  )
})

test_that("parse_gradescope handles file correctly", {
  # Create temporary Gradescope file
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))

  gradescope_content <- paste(
    "First Name,Last Name,SID,Email,Submission ID,Submission Time,Status,Extra",
    "John,Doe,1234567,john@example.com,98765,2023-10-01 14:30:00 +1100,Active,123",
    sep = "\n"
  )
  writeLines(gradescope_content, temp_file)

  # Test parsing
  result <- parse_gradescope(temp_file)

  expect_s3_class(result, "data.frame")
  # Check that only columns from First Name to Status are included
  expect_equal(
    colnames(result),
    c(
      "First Name", "Last Name", "SID", "Email",
      "Submission ID", "Submission Time", "Status"
    )
  )
  expect_false("Extra" %in% colnames(result))
})

