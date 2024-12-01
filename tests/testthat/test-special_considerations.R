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

test_that("validate_inputs checks for required parameters", {
  expect_error(
    validate_inputs(NULL, 2023),
    "UOS parameter must be provided"
  )
  expect_error(
    validate_inputs("MATH1001", NULL),
    "Year parameter must be provided"
  )
  expect_no_error(validate_inputs("MATH1001", 2023))
})

test_that("read_spec_cons_data filters by UOS and year", {
  # Create temporary file
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))

  content <- paste(
    "extension_in_calendar_days,u_outcome_type,availability,state",
    "7,Replacement exam,MATH1001_S2C_2023,Approved",
    "7,Replacement exam,MATH1002_S2C_2023,Approved",
    "7,Replacement exam,MATH1001_S2C_2022,Approved",
    sep = "\n"
  )
  writeLines(content, temp_file)

  result <- read_spec_cons_data(temp_file, "MATH1001", "2023")
  expect_equal(nrow(result), 1)
  expect_true(all(grepl("MATH1001.*2023", result$availability)))
})

test_that("select_outcome_type handles user selection", {
  df <- data.frame(
    u_outcome_type = c("Replacement exam", "Extension", "Replacement exam")
  )

  # Mock user selecting first option
  mockery::stub(select_outcome_type, "readline", function(...) "1")
  result <- select_outcome_type(df)
  expect_equal(result, "Replacement exam")
})

test_that("filter_spec_cons filters correctly", {
  df <- data.frame(
    state = c("Approved", "Pending", "Approved", "Rejected"),
    u_outcome_type = c("Replacement exam", NA, "Extension", "Replacement exam")
  )

  result <- filter_spec_cons(df, "Replacement exam")
  expect_equal(nrow(result), 2) # One approved replacement + one pending
  expect_true(all(result$state %in% c("Approved", "Pending")))
})

test_that("display_summary shows correct counts", {
  df <- data.frame(
    state = c("Approved", "Pending", "Approved"),
    u_outcome_type = c("Replacement exam", NA, "Replacement exam")
  )

  output <- capture.output(display_summary(df))
  expect_match(output[2], "Number of students with approved replacement exams: 2")
  expect_match(output[3], "Number of students with pending special considerations: 1")
  expect_match(output[4], "Total: 3")
})

test_that("parse_spec_cons integrates all components", {
  # Create temporary file
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))

  content <- paste(
    "extension_in_calendar_days,u_outcome_type,availability,state,sys_updated_on",
    "7,Replacement exam,MATH1001_S2C_2023,Approved,01/10/2023 14:30:00",
    "7,Extension,MATH1001_S2C_2023,Approved,01/10/2023 14:30:00",
    "7,NA,MATH1001_S2C_2023,Pending,01/10/2023 14:30:00",
    sep = "\n"
  )
  writeLines(content, temp_file)

  # Mock user selecting "Replacement exam"
  mockery::stub(
    parse_spec_cons, "select_outcome_type",
    function(...) "Replacement exam"
  )

  result <- parse_spec_cons(temp_file, "MATH1001", "2023")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2) # One approved replacement + one pending
})
