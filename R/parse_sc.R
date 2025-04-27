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
#' @param uos_filter Optional. A character string specifying the Unit of Study code to filter by (e.g., "ENVX2001"). Renamed from `uos` to avoid conflict with the new column.
#' @param session_filter Optional. A character string specifying the session code to filter by (e.g., "S1C"). Renamed from `session`.
#' @param year_filter Optional. A numeric or character string specifying the year to filter by (e.g., 2025). Renamed from `year`.
#' @param mode_filter Optional. A character string specifying the mode code to filter by (e.g., "ND"). Renamed from `mode`.
#' @param location_filter Optional. A character string specifying the location code to filter by (e.g., "CC"). Renamed from `location`.
#'
#' @return A data frame containing the special consideration data, potentially filtered,
#'   with the `uo_s_availability` column split into `uos`, `session`, `year`, `mode`, and `location`.
#'   Returns the original data frame (with split columns) if no filtering arguments are provided.
#'   Returns the original unmodified data frame if the availability column is not found.
#'
#' @importFrom dplyr filter select all_of relocate if_else mutate
#' @importFrom tidyr separate
#' @importFrom rlang .data
#' @importFrom logger log_info log_debug log_warn
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'sc_data' is the output from parse_sc()
#' # Split columns and filter by UoS and Session
#' filtered_data <- filter_sc(sc_data, uos_filter = "ENVX2001", session_filter = "S1C")
#' # Split columns and filter by Year and Mode
#' specific_year_mode <- filter_sc(sc_data, year_filter = 2025, mode_filter = "ND")
#' # Just split columns without filtering
#' split_only_data <- filter_sc(sc_data)
#' }
filter_sc <- function(data, uos_filter = NULL, session_filter = NULL, year_filter = NULL, mode_filter = NULL, location_filter = NULL) {
  logger::log_info("Starting filter_sc function.")
  logger::log_debug(paste("Initial dimensions:", paste(dim(data), collapse = "x")))

  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }

  availability_col <- "uo_s_availability"
  if (!(availability_col %in% names(data))) {
    logger::log_warn(paste("Could not find the expected column:", availability_col, ". Returning original data frame."))
    return(data)
  }
  logger::log_info(paste("Found availability column:", availability_col))

  # --- Split the availability column ---
  new_cols <- c("uos", "session", "year", "mode", "location")
  logger::log_debug(paste("Splitting", availability_col, "into:", paste(new_cols, collapse = ", ")))

  # Use tryCatch to handle potential errors during separation gracefully
  data_split <- tryCatch(
    {
      tidyr::separate(
        data,
        col = !!rlang::sym(availability_col),
        into = new_cols,
        sep = "-",
        remove = FALSE, # Keep the original column
        convert = TRUE, # Try converting year to numeric, etc.
        fill = "right" # Handle cases with fewer than 5 parts
      )
    },
    error = function(e) {
      logger::log_warn(paste("Error during tidyr::separate:", e$message, ". Returning original data frame."))
      return(data) # Return original data on error
    }
  )

  # Check if the separation returned the original data due to an error
  if (identical(data_split, data) && !(availability_col %in% names(data))) {
    # This condition means an error occurred in tryCatch and original data was returned
    # but the original data didn't have the column to begin with (already handled above, but double-check)
    return(data)
  } else if (identical(data_split, data) && (availability_col %in% names(data))) {
    # This condition means an error occurred in tryCatch and original data was returned
    logger::log_warn("Separation failed, returning original data frame.")
    return(data)
  } else {
    # Separation was successful (or partially successful with fill='right')
    data <- data_split
    logger::log_debug(paste("Dimensions after splitting:", paste(dim(data), collapse = "x")))
  }

  # --- Reorder columns ---
  # Place new columns right after the original availability column
  logger::log_debug("Relocating new columns.")
  data <- dplyr::relocate(data, dplyr::all_of(new_cols), .after = !!rlang::sym(availability_col))

  # --- Apply Filters ---
  filter_args <- list(uos = uos_filter, session = session_filter, year = year_filter, mode = mode_filter, location = location_filter)
  active_filters <- names(filter_args)[!sapply(filter_args, is.null)]

  if (length(active_filters) == 0) {
    logger::log_info("No filters provided. Returning data frame with split columns.")
  } else {
    logger::log_info(paste("Applying filters for:", paste(active_filters, collapse = ", ")))

    # Ensure year filter is handled correctly (numeric vs character)
    # Convert year column to character for robust comparison if year_filter is character
    if (!is.null(year_filter) && !is.numeric(year_filter)) {
      data <- dplyr::mutate(data, year = as.character(.data$year))
      year_filter <- as.character(year_filter)
      logger::log_debug("Converted year column and filter to character for comparison.")
    } else if (!is.null(year_filter) && is.numeric(year_filter)) {
      # Ensure the column is numeric if the filter is numeric
      # suppressWarnings handles cases where conversion might fail
      data <- dplyr::mutate(data, year = suppressWarnings(as.numeric(.data$year)))
      logger::log_debug("Ensured year column is numeric for comparison.")
    }

    initial_filter_rows <- nrow(data)

    if (!is.null(uos_filter)) {
      data <- dplyr::filter(data, .data$uos == uos_filter)
    }
    if (!is.null(session_filter)) {
      data <- dplyr::filter(data, .data$session == session_filter)
    }
    if (!is.null(year_filter)) {
      # Filter, handling potential NAs introduced by conversion
      data <- dplyr::filter(data, !is.na(.data$year) & .data$year == year_filter)
    }
    if (!is.null(mode_filter)) {
      data <- dplyr::filter(data, .data$mode == mode_filter)
    }
    if (!is.null(location_filter)) {
      data <- dplyr::filter(data, .data$location == location_filter)
    }
    logger::log_debug(paste("Rows before filtering:", initial_filter_rows, "- Rows after filtering:", nrow(data)))
  }

  logger::log_info(paste("Final row count:", nrow(data)))
  logger::log_debug(paste("Final dimensions:", paste(dim(data), collapse = "x")))
  logger::log_info("Finished filter_sc function.")

  # Return only the modified data frame
  return(data)
}
