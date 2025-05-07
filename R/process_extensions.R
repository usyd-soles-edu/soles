#' Process Approved Extensions for a Specific Assessment
#'
#' Validates and filters a data frame of extension requests to include only
#' approved requests for a specific assessment, selecting relevant columns.
#'
#' @param data A data frame containing extension request data. Must contain
#'   columns: `state`, `uo_s_availability`, `uos`, `session`, `year`, `mode`,
#'   `location`, `assessment`, `assessment_due_date`, `student_id`,
#'   `outcome_type`, `revised_due_date`.
#' @param assessment A character string specifying the assessment name
#'   to filter by (e.g., "Quiz 1", "Assignment 2"). This comparison ignores case
#'   and leading/trailing whitespace.
#'
#' @return A data frame containing the selected columns for approved extension
#'   requests matching the specified assessment. Returns an empty data frame
#'   if validation fails or no matching records are found.
#' @importFrom logger log_debug log_error
#' @importFrom tibble as_tibble
#' @export
process_extensions <- function(data, ap = NULL, assessment) {
  logger::log_debug("Entering process_extensions function.")
  logger::log_debug("Input data dimensions: ", paste(dim(data), collapse = "x"))
  logger::log_debug("Assessment filter: ", assessment)

  # Required columns for processing
  required_cols <- c(
    "state", "uo_s_availability", "uos", "session", "year", "mode",
    "location", "assessment", "assessment_due_date", "student",
    "outcome_type", "revised_due_date"
  )

  # --- Validation ---
  if (!is.data.frame(data)) {
    logger::log_error("Input 'data' is not a data frame.")
    return(data.frame()) # Return empty data frame
  }

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    logger::log_error("Input data frame is missing required columns: ", paste(missing_cols, collapse = ", "))
    return(data.frame()) # Return empty data frame
  }

  # --- Dependency Checks ---
  if (!requireNamespace("logger", quietly = TRUE)) {
    # Cannot log error if logger is missing, so just stop
    stop("Package 'logger' needed for this function to work. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("tibble", quietly = TRUE)) {
    logger::log_error("Package 'tibble' needed but not installed.")
    stop("Package 'tibble' needed for this function to work. Please install it.", call. = FALSE)
  }

  # --- Processing ---
  logger::log_debug("Selecting required columns.")
  processed_data <- data[, required_cols, drop = FALSE]

  logger::log_debug("Filtering data for Approved state and assessment: ", assessment)
  # Prepare the assessment string once for comparison
  clean_assessment <- tolower(trimws(assessment))

  filtered_data <- processed_data[
    processed_data$state == "Approved" &
      tolower(trimws(processed_data$assessment)) == clean_assessment, ,
    # Keep all columns
    drop = FALSE # Ensure it remains a data frame even if one row results
  ]

  # Separate student column into name and unikey
  logger::log_debug("Separating student column into name and unikey.")
  # Split the 'student' column
  split_student <- strsplit(filtered_data$student, " - ", fixed = TRUE)
  # Handle cases where split might not produce 2 parts (though unlikely based on format)
  split_matrix <- do.call(rbind, lapply(split_student, function(x) if (length(x) == 2) x else c(NA, NA)))
  colnames(split_matrix) <- c("name", "unikey")

  # Combine the new columns with the original data (excluding the old 'student' column)
  final_data <- cbind(filtered_data[, setdiff(names(filtered_data), "student"), drop = FALSE], split_matrix)

  logger::log_debug("Final data dimensions after processing: ", paste(dim(final_data), collapse = "x"))
  logger::log_debug("Exiting process_extensions function.")

  return(tibble::as_tibble(final_data))
}
