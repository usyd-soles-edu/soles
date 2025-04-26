#' Parse spec cons file
#'
#' @param x Path to sc file (CSV or XLSX format)
#' @param uos Unit of study code to filter by (optional)
#' @param semester Semester code to filter by (e.g., "S1C", "S2C") (optional)
#' @param year Year to filter by (optional)
#' @return A data frame containing filtered sc data
#' @importFrom readr read_csv
#' @importFrom readxl read_xlsx
#' @importFrom dplyr filter mutate rename select any_of
#' @importFrom tools file_ext
#' @importFrom lubridate dmy_hms dmy parse_date_time
#' @importFrom rlang .data :=
#' @export
parse_sc <- function(x, uos = NULL, semester = NULL, year = NULL) {
  # Get file extension
  file_ext <- tolower(tools::file_ext(x))

  # Read file based on extension
  df <- switch(file_ext,
    "csv" = suppressWarnings(readr::read_csv(x, show_col_types = FALSE)),
    "xlsx" = suppressWarnings(readxl::read_xlsx(x)),
    stop("Unsupported file format. Only CSV and XLSX files are supported.")
  )

  # --- Date Parsing and Column Renaming ---
  due_date_col_csv <- "due_date"
  revised_date_col_csv <- "extension_in_calendar_days" # Misleading name in CSV
  due_date_col_xlsx <- "Assessment Due Date"
  revised_date_col_xlsx <- "Revised Due Date"

  # Parse dates in place
  if (file_ext == "csv") {
    if (due_date_col_csv %in% names(df)) {
      df <- df |>
        dplyr::mutate(
          # Parse CSV date string "dd-mm-yyyy HH:MM:SS" and overwrite
          "{due_date_col_csv}" := suppressWarnings(lubridate::dmy_hms(.data[[due_date_col_csv]], quiet = TRUE))
        )
    }
    if (revised_date_col_csv %in% names(df)) {
      df <- df |>
        dplyr::mutate(
          # Parse CSV date string "dd-mm-yyyy" and overwrite
          "{revised_date_col_csv}" := suppressWarnings(lubridate::dmy(.data[[revised_date_col_csv]], quiet = TRUE))
        )
    }
  } else if (file_ext == "xlsx") {
    if (due_date_col_xlsx %in% names(df)) {
      # Excel dates are often read as POSIXct already, but ensure consistency and overwrite
      df <- df |>
        dplyr::mutate(
          "{due_date_col_xlsx}" := suppressWarnings(lubridate::parse_date_time(.data[[due_date_col_xlsx]], orders = c("Ymd HMS", "Ymd")))
        )
    }
    if (revised_date_col_xlsx %in% names(df)) {
      # Ensure consistency, parse as Date and overwrite
      df <- df |>
        dplyr::mutate(
          "{revised_date_col_xlsx}" := suppressWarnings(as.Date(lubridate::parse_date_time(.data[[revised_date_col_xlsx]], orders = c("Ymd HMS", "Ymd"))))
        )
    }
  }

  # Column names are kept as original after in-place parsing above.

  # --- Filtering ---
  # Set availability column name based on file type
  availability_col <- if (file_ext == "xlsx") {
    "UoS (availability)"
  } else {
    "availability"
  }

  # Apply filters based on provided parameters (if availability column exists)
  if (availability_col %in% names(df)) {
    if (!is.null(uos)) {
      df <- dplyr::filter(df, grepl(uos, .data[[availability_col]]))
    }

    if (!is.null(semester)) {
      # Filter by semester code which appears between unit code and year
      # Example: ENVX2001-S1C-2025-ND-CC
      df <- dplyr::filter(df, grepl(paste0("-", semester, "-"), .data[[availability_col]]))
    }

    if (!is.null(year)) {
      df <- dplyr::filter(df, grepl(year, .data[[availability_col]]))
    }
  } else {
    # Optionally add a warning if the availability column is missing
    warning("Column '", availability_col, "' not found. Skipping availability filters.")
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

  # Identify the availability column
  availability_col <- NULL
  if ("availability" %in% names(data)) {
    availability_col <- "availability"
  } else if ("UoS (availability)" %in% names(data)) {
    availability_col <- "UoS (availability)"
  } else {
    logger::log_warn("Could not find 'availability' or 'UoS (availability)' column. Returning original data frame.")
    # Consider using log_error and stop() if this column is essential and its absence is truly an error
    # stop("Essential availability column not found.")
    return(data)
  }
  logger::log_info(paste("Using availability column:", availability_col))

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
