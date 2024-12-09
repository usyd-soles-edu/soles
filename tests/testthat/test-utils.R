test_that("read_column_names reads CSV headers correctly", {
  # Create a temporary CSV file
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(data.frame(col1 = 1:3, col2 = letters[1:3]), temp_csv, row.names = FALSE)
  on.exit(unlink(temp_csv))

  # Test reading column names
  expect_equal(
    soles:::read_column_names(temp_csv),
    c("col1", "col2")
  )
})

test_that("read_canvas_data reads CSV data correctly", {
  # Create a temporary Canvas-like CSV file
  temp_csv <- tempfile(fileext = ".csv")
  header_rows <- c(
    "Some Canvas header",
    "Another header line",
    "Yet another header"
  )
  data_rows <- c(
    "1,a",
    "2,b",
    "N/A,c"
  )
  writeLines(c(header_rows, data_rows), temp_csv)
  on.exit(unlink(temp_csv))

  # Test reading data
  result <- soles:::read_canvas_data(temp_csv, c("col1", "col2"))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 2)
  expect_true(is.na(result$col1[3])) # Test N/A handling
})
