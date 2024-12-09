test_that("find_spec_cons_file finds the correct file", {
  # Create temporary directory
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Create mock special considerations file with required columns
  spec_cons_content <- paste(
    "extension_in_calendar_days,u_outcome_type,sys_updated_on,state,availability",
    "7,Replacement exam,01/10/2023 14:30:00,Approved,MATH1001_S2C_2023",
    sep = "\n"
  )
  spec_cons_file <- file.path(temp_dir, "special_arrangements.csv")
  writeLines(spec_cons_content, spec_cons_file)

  # Test finding the file
  result <- find_spec_cons_file(temp_dir)
  expect_equal(result, spec_cons_file)
})

test_that("find_spec_cons_file selects most recent file", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Create two mock files with different update times
  older_content <- paste(
    "extension_in_calendar_days,u_outcome_type,sys_updated_on,state,availability",
    "7,Replacement exam,01/10/2023 14:30:00,Approved,MATH1001_S2C_2023",
    sep = "\n"
  )
  newer_content <- paste(
    "extension_in_calendar_days,u_outcome_type,sys_updated_on,state,availability",
    "7,Replacement exam,02/10/2023 14:30:00,Approved,MATH1001_S2C_2023",
    sep = "\n"
  )

  older_file <- file.path(temp_dir, "special_arrangements_1.csv")
  newer_file <- file.path(temp_dir, "special_arrangements_2.csv")
  writeLines(older_content, older_file)
  writeLines(newer_content, newer_file)

  # Test that most recent file is selected
  result <- find_spec_cons_file(temp_dir)
  expect_equal(result, newer_file)
})

test_that("find_spec_cons_file errors on missing files", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  expect_error(
    find_spec_cons_file(temp_dir),
    "No CSV files found in directory"
  )
})

test_that("find_spec_cons_file errors on invalid files", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Create file without required columns
  invalid_content <- "Column1,Column2,Column3"
  invalid_file <- file.path(temp_dir, "invalid.csv")
  writeLines(invalid_content, invalid_file)

  expect_error(
    find_spec_cons_file(temp_dir),
    "No special considerations file found \\(missing expected columns\\)"
  )
})
