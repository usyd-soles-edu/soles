test_that("find_canvas_file finds the correct file", {
  # Create temporary directory
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Create mock Canvas file with required columns
  canvas_content <- paste(
    "SIS User ID,SIS Login ID,Student,Assignment 1",
    "1234567,abcd1234,John Doe,85",
    sep = "\n"
  )
  canvas_file <- file.path(temp_dir, "2023-10-01T1200_Marks.csv")
  writeLines(canvas_content, canvas_file)

  # Test finding the file
  result <- find_canvas_file(temp_dir)
  expect_equal(result, canvas_file)
})

test_that("find_canvas_file selects most recent file", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Create multiple mock Canvas files
  canvas_content <- "SIS User ID,SIS Login ID,Student,Assignment 1"
  older_file <- file.path(temp_dir, "2023-10-01T1200_Marks.csv")
  newer_file <- file.path(temp_dir, "2023-10-02T1200_Marks.csv")
  writeLines(canvas_content, older_file)
  writeLines(canvas_content, newer_file)

  # Test that most recent file is selected
  result <- find_canvas_file(temp_dir)
  expect_equal(result, newer_file)
})

test_that("find_canvas_file errors on missing files", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  expect_error(
    find_canvas_file(temp_dir),
    "No Canvas export files found in directory"
  )
})

test_that("find_canvas_file errors on invalid files", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Create file without required columns
  invalid_content <- "Column1,Column2,Column3"
  invalid_file <- file.path(temp_dir, "2023-10-01T1200_Marks.csv")
  writeLines(invalid_content, invalid_file)

  expect_error(
    find_canvas_file(temp_dir),
    "File does not appear to be a Canvas export"
  )
})

test_that("parse_canvas handles specific columns correctly", {
  # Create temporary Canvas file
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))

  canvas_content <- paste(
    "Student,SIS User ID,SIS Login ID,Assignment 1,Assignment 2,Current Score",
    "John Doe,1234567,abcd1234,85,90,87.5",
    sep = "\n"
  )
  writeLines(canvas_content, temp_file)

  # Test parsing with specific columns
  result <- parse_canvas(temp_file, cols = 1) # Select Assignment 1

  expect_s3_class(result, "data.frame")
  expect_equal(colnames(result), c("Student", "SID", "Unikey", "Assignment 1"))
  expect_false("Current Score" %in% colnames(result))
})

test_that("parse_canvas handles interactive column selection", {
  # Create temporary Canvas file
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))

  canvas_content <- paste(
    "Student,SIS User ID,SIS Login ID,Assignment 1,Assignment 2",
    "John Doe,1234567,abcd1234,85,90",
    sep = "\n"
  )
  writeLines(canvas_content, temp_file)

  # Mock user input for interactive selection
  mockery::stub(
    parse_canvas, "prompt_user_for_columns",
    function(x) x[1]
  ) # Select first assignment

  result <- parse_canvas(temp_file)

  expect_s3_class(result, "data.frame")
  expect_equal(colnames(result), c("Student", "SID", "Unikey", "Assignment 1"))
})

test_that("parse_canvas correctly renames columns", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))

  canvas_content <- paste(
    "Student,SIS User ID,SIS Login ID,Assignment 1",
    "John Doe,1234567,abcd1234,85",
    sep = "\n"
  )
  writeLines(canvas_content, temp_file)

  result <- parse_canvas(temp_file, cols = 1)

  expect_true(all(c("SID", "Unikey") %in% colnames(result)))
  expect_false(any(c("SIS User ID", "SIS Login ID") %in% colnames(result)))
})
