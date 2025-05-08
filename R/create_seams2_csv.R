#' Create a CSV file formatted for SEAMS2 section uploads
#'
#' This function takes a data frame containing extension details and generates a
#' CSV file suitable for uploading to SEAMS2 to create Canvas sections based on
#' assessment, revised due date, and outcome type.
#'
#' @param data A data frame, typically the output from `merge_ap_extensions` or
#'   `process_extensions`. It must contain at least the columns: `assessment`,
#'   `revised_due_date`, `outcome_type`, and `unikey`.
#' @param output_filename The desired name for the output CSV file. Defaults to
#'   `"[YYYYMMDD]-seams2_upload.csv"`, where `[YYYYMMDD]` is the current date.
#' @param write_csv Logical, whether to write the output data frame to a CSV file.
#'   Defaults to `TRUE`.
#'
#' @return A `tibble` containing the data formatted for SEAMS2 upload.
#'   The tibble includes columns "Section Id" (NA), "Section name",
#'   "Student name" (NA), and "UniKey". Returns `NULL` if input validation fails
#'   (e.g., missing columns, non-data frame input, date conversion issues).
#'   If `write_csv` is `TRUE`, the function also attempts to write this data to
#'   the specified CSV file (writing `NA` values as blank strings), logging
#'   success or failure messages.
#' @importFrom readr write_csv
#' @importFrom logger log_info log_error
#' @importFrom tibble as_tibble
#'
#' @export
#' @examples
#' \dontrun{
#' # Assuming 'processed_data' is the output from process_extensions()
#' # or merge_ap_extensions()
#' if (requireNamespace("logger", quietly = TRUE)) {
#'   logger::log_threshold(logger::INFO) # Set log level if using logger
#' }
#'
#' # Create a dummy data frame for the example
#' dummy_data <- data.frame(
#'   assessment = c("Quiz1", "Assignment1", "Quiz1"),
#'   revised_due_date = as.Date(c("2024-05-15", "2024-06-01", NA)),
#'   outcome_type = c("Approved", NA, "Approved"),
#'   unikey = c("user123", "user456", "user789"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Generate the SEAMS2 tibble and attempt to write the CSV
#' # (default filename with date)
#' seams2_tibble_write <- create_seams2_csv(dummy_data)
#' print("Generated tibble (CSV write attempted):")
#' print(head(seams2_tibble_write))
#'
#' # Generate the tibble without writing to CSV
#' seams2_tibble_no_write <- create_seams2_csv(dummy_data, write_csv = FALSE)
#' print("Generated tibble (CSV write skipped):")
#' print(head(seams2_tibble_no_write))
#' }
create_seams2_csv <- function(data,
                              output_filename = paste0(format(Sys.Date(), "%Y%m%d"), "-seams2_upload.csv"),
                              write_csv = TRUE) {
  # --- Input Validation ---
  if (!is.data.frame(data)) {
    logger::log_error("Input 'data' must be a data frame.")
    return(NULL)
  }

  required_cols <- c("assessment", "revised_due_date", "outcome_type", "unikey")
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    logger::log_error(
      "Input data frame is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
    return(NULL)
  }

  # --- Data Preparation ---

  # Handle NA outcome_type
  data$outcome_type[is.na(data$outcome_type)] <- "Extension"

  # Format revised_due_date
  # Ensure it's Date type first
  if (!inherits(data$revised_due_date, "Date")) {
    data$revised_due_date <- tryCatch(
      {
        as.Date(data$revised_due_date)
      },
      error = function(e) {
        logger::log_error("Could not convert 'revised_due_date' to Date objects. ", e$message)
        return(rep(NA, nrow(data))) # Return NA vector on error
      }
    )
    if (all(is.na(data$revised_due_date))) {
      return(NULL)
    } # Stop if conversion failed entirely
  }

  # Format to MM-DD, handle NAs after formatting
  formatted_dates <- format(data$revised_due_date, "%m-%d")
  formatted_dates[is.na(data$revised_due_date)] <- "" # Replace NA dates with empty string

  # Construct Section name
  section_name <- paste(data$assessment, formatted_dates, data$outcome_type)
  # Trim potential leading/trailing whitespace if date was NA
  section_name <- trimws(gsub("\\s+", " ", section_name))

  # --- Prepare Output Data Frame ---
  output_df <- data.frame(
    "Section Id" = rep(NA, nrow(data)), # Use NA for the data frame
    "Section name" = section_name,
    "Student name" = rep(NA, nrow(data)), # Use NA for the data frame
    "UniKey" = data$unikey,
    stringsAsFactors = FALSE,
    check.names = FALSE # Prevent R from changing column names like "Section Id"
  )

  # --- Write CSV or Return Data Frame ---
  if (write_csv) {
    tryCatch(
      {
        output_path <- normalizePath(output_filename, mustWork = FALSE)
        readr::write_csv(output_df, file = output_path, na = "")
        logger::log_info("SEAMS2 CSV file successfully written to: ", output_path)
        # Return value handled after the if/else block
      },
      error = function(e) {
        logger::log_error("Failed to write SEAMS2 CSV file: ", e$message)
        # Return value handled after the if/else block
      }
    )
  } else {
    logger::log_info("Returning SEAMS2 data frame directly (write_csv = FALSE).")
    # Return value handled after the if/else block
  }

  # Always return the processed data frame as a tibble
  return(tibble::as_tibble(output_df))
}
