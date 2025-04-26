#' Parse spec cons file
#'
#' Reads and standardizes data from a special consideration file (CSV or XLSX).
#'
#' @param x Path to the special consideration file (CSV or XLSX format).
#' @return A data frame containing the standardized special consideration data.
#' @importFrom readr read_csv
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate
#' @importFrom tools file_ext
#' @importFrom lubridate parse_date_time
#' @importFrom rlang .data :=
#' @export
parse_sc <- function(x) {
  # Get file extension
  file_ext <- tolower(tools::file_ext(x))

  # Read file based on extension
  df <- switch(file_ext,
    "csv" = suppressWarnings(readr::read_csv(x, show_col_types = FALSE)),
    "xlsx" = suppressWarnings(readxl::read_xlsx(x)),
    stop("Unsupported file format. Only CSV and XLSX files are supported.")
  )

  # --- Standardise column names ---
  expected_cols <- 22
  if (ncol(df) != expected_cols) {
    stop(paste("Expected", expected_cols, "columns, but found", ncol(df), "in file:", x))
  }

  names(df) <- c(
    "number", "state", "classification", "school", "uo_s_availability",
    "assessment_category", "assessment_type", "assessment_title", "assessment",
    "alternate_consideration", "closing_date", "assessment_due_date",
    "teacher_coordinator", "student", "student_id", "outcome_type",
    "revised_due_date", "affected_end", "affected_start", "created",
    "updated_by", "updated"
  )


  if (is.character(df$revised_due_date)) {
    df <- dplyr::mutate(df, revised_due_date = lubridate::dmy(revised_due_date))
  }
  if (is.character(df$assessment_due_date)) {
    df <- dplyr::mutate(df, assessment_due_date = lubridate::dmy_hms(assessment_due_date))
  }

  return(df)
}

#' Filter Special Consideration Data
#'
#' Filters a data frame containing special consideration data based on components
#' extracted from the availability column.
#'
#' @param data A data frame, typically the output of \code{\link{parse_sc}}.
#' @param uos Optional. A character string specifying the Unit of Study code to filter by (e.g., "ENVX2001").
#' @param session Optional. A character string specifying the session code to filter by (e.g., "S1C").
#' @param year Optional. A numeric or character string specifying the year to filter by (e.g., 2025).
#' @param mode Optional. A character string specifying the mode code to filter by (e.g., "ND").
#' @param location Optional. A character string specifying the location code to filter by (e.g., "CC").
#'
#' @return A data frame filtered according to the provided criteria. Returns the original data frame
#'   if no filtering arguments are provided or if the availability column is not found or malformed.
#'
#' @importFrom dplyr filter select all_of
#' @importFrom stringr str_split_fixed
#' @importFrom rlang .data
#' @importFrom logger log_info log_debug
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'sc_data' is the output from parse_sc()
#' filtered_data <- filter_sc(sc_data, uos = "ENVX2001", session = "S1C")
#' specific_year_mode <- filter_sc(sc_data, year = 2025, mode = "ND")
#' }
filter_sc <- function(data, uos = NULL, session = NULL, year = NULL, mode = NULL, location = NULL) {
  logger::log_info("Starting filter_sc function.")
  logger::log_info(paste("Initial row count:", nrow(data)))
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }

  # --- Simplified availability column check ---
  # Since parse_sc standardizes names, we expect "uo_s_availability"
  availability_col <- "uo_s_availability"
  if (!(availability_col %in% names(data))) {
    logger::log_warn(paste("Could not find the expected column:", availability_col, ". Returning original data frame."))
    # Consider if this should be an error depending on requirements
    # stop(paste("Essential column not found:", availability_col))
    return(data)
  }
  logger::log_info(paste("Using availability column:", availability_col))
  # --- End simplification ---


  # Check if any filtering arguments are provided
  filter_args <- list(uos = uos, session = session, year = year, mode = mode, location = location)
  active_filters <- names(filter_args)[!sapply(filter_args, is.null)]
  if (length(active_filters) == 0) {
    logger::log_info("No filters provided. Returning original data frame.")
    return(data) # Return original data if no filters are applied
  } else {
    logger::log_info(paste("Applying filters for:", paste(active_filters, collapse = ", ")))
  }

  # Split the availability string
  logger::log_debug("Splitting availability column.")
  # Suppress warnings temporarily for cases where split doesn't yield 5 parts
  split_data <- suppressWarnings(
    stringr::str_split_fixed(data[[availability_col]], "-", 5)
  )

  # Check if splitting resulted in 5 columns
  if (ncol(split_data) != 5) {
    logger::log_warn("Availability column format is inconsistent. Expected 5 parts separated by hyphens. Filtering may be unreliable. Returning original data frame.")
    return(data)
  }

  # Add temporary columns for filtering
  temp_cols <- c(".uos_temp", ".session_temp", ".year_temp", ".mode_temp", ".location_temp")
  data[, temp_cols] <- split_data
  # Ensure year is numeric for comparison if year filter is numeric
  if (!is.null(year) && is.numeric(year)) {
    data$.year_temp <- suppressWarnings(as.numeric(data$.year_temp))
  } else if (!is.null(year)) { # Ensure year filter is character if provided as character
    year <- as.character(year)
  }


  # Apply filters iteratively
  filtered_data <- data
  if (!is.null(uos)) {
    filtered_data <- dplyr::filter(filtered_data, .data$.uos_temp == uos)
  }
  if (!is.null(session)) {
    filtered_data <- dplyr::filter(filtered_data, .data$.session_temp == session)
  }
  if (!is.null(year)) {
    # Handle potential NA from conversion if year filter was numeric but column wasn't
    if (is.numeric(year)) {
      filtered_data <- dplyr::filter(filtered_data, !is.na(.data$.year_temp) && .data$.year_temp == year)
    } else {
      filtered_data <- dplyr::filter(filtered_data, .data$.year_temp == year)
    }
  }
  if (!is.null(mode)) {
    filtered_data <- dplyr::filter(filtered_data, .data$.mode_temp == mode)
  }
  if (!is.null(location)) {
    filtered_data <- dplyr::filter(filtered_data, .data$.location_temp == location)
  }

  # Remove temporary columns
  filtered_data <- dplyr::select(filtered_data, -dplyr::all_of(temp_cols))

  logger::log_info(paste("Final row count:", nrow(filtered_data)))
  logger::log_info("Finished filter_sc function.")
  return(filtered_data)
}
