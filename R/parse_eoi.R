#' Parse Expression of Interest (EOI) CSV File
#'
#' Reads an EOI CSV file, extracts relevant columns, and renames them
#' according to a predefined schema. The function expects a specific CSV
#' structure where actual headers are on the second row and data starts
#' from the fourth row. The first 17 columns (based on the headers from
#' the second row) are discarded.
#'
#' @param path Character string; the file path to the EOI CSV file.
#' @return A tibble containing the parsed and cleaned EOI data, with
#'   selected columns renamed for clarity.
#' @importFrom readr read_csv
#' @importFrom dplyr select all_of any_of
#' @export
#' @examples
#' \dontrun{
#' # Assuming an EOI_data.csv file exists in the working directory
#' # with the expected structure.
#' # eoi_data <- parse_eoi("EOI_data.csv")
#' # print(head(eoi_data))
#' }
parse_eoi <- function(path) {
  # Read the actual column headers from the second row of the CSV
  actual_headers <- readr::read_csv(path, skip = 1, n_max = 0, show_col_types = FALSE) |>
    names()

  # Read the data starting from the fourth row, using the actual headers
  raw_data <- readr::read_csv(
    path,
    skip = 3,
    col_names = actual_headers,
    show_col_types = FALSE,
    name_repair = "minimal" # Avoids renaming issues if headers are problematic
  )

  # Determine columns to keep (from the 18th to the end)
  if (length(actual_headers) < 18) {
    stop("The CSV file does not have enough columns (expected at least 18 based on header row).")
  }
  columns_to_keep_original_names <- actual_headers[18:length(actual_headers)]

  data_subset <- dplyr::select(raw_data, dplyr::all_of(columns_to_keep_original_names))

  # Define the new names for the selected columns
  new_names <- c(
    "worked_at_usyd", "staff_id", "title", "surname", "given_name",
    "preferred_email", "preferred_contact", "suburb_postcode",
    "valid_visa", "phd_conferred", "previous_demonstrator", "previous_units",
    "preferred_units", "availability_monday", "availability_tuesday",
    "availability_wednesday", "availability_thursday", "availability_friday",
    "lead_demonstrator_interest", "lead_demonstrator_other",
    "completed_training", "desired_hours_per_week",
    "planned_years", "expertise_area", "higher_education_degrees",
    "teaching_philosophy", "experience_benefit", "blockout_dates",
    "cv_file_id", "cv_file_name", "cv_file_size", "cv_file_type",
    "info_acknowledgment", "info_amendment_acknowledgment"
  )

  # Ensure the number of selected columns matches the number of new names
  if (ncol(data_subset) != length(new_names)) {
    # Add a more informative warning/error if counts don't match
    warning_message <- paste0(
      "Mismatch after selecting columns to rename. Expected ", length(new_names),
      " columns based on 'new_names' definition, but got ", ncol(data_subset),
      " columns (from original columns: '",
      paste(columns_to_keep_original_names, collapse = "', '"), "'). ",
      "Please check CSV structure and column selection logic (currently 18th to end)."
    )
    # Depending on strictness, this could be a stop()
    warning(warning_message)
    # If we proceed, names() will recycle or truncate, which is bad.
    # It's safer to stop if the column count for renaming is not exact.
    if (ncol(data_subset) != length(new_names)) { # Re-check for stop
      stop("Critical mismatch in column count for renaming. Halting. ", warning_message)
    }
  }
  names(data_subset) <- new_names

  # Remove further unnecessary columns by their new names
  columns_to_remove <- c(
    "valid_visa", "suburb_postcode", "planned_years", "cv_file_id",
    "cv_file_name", "cv_file_size", "cv_file_type", "info_acknowledgment",
    "info_amendment_acknowledgment"
  )

  out <- dplyr::select(data_subset, -dplyr::any_of(columns_to_remove))

  return(out)
}

#' Extract Unique Preferred Units of Study
#'
#' Extracts and lists all unique preferred units of study from a data frame
#' column. It processes a column (typically 'preferred_units') where each entry
#' can be a comma-separated string of unit codes.
#'
#' @param df A data frame or tibble containing a column with preferred units.
#'   This column should be named 'preferred_units'.
#' @return A character vector of unique, trimmed unit codes.
#'   May include \code{NA} or empty strings (\code{""}) if these result from
#'   processing (e.g., from \code{NA} inputs or sequences like \code{",,"}).
#'   Returns \code{NULL} if the input column is empty or results in no units after
#'   splitting (e.g. \code{df$preferred_units} is \code{character(0)}).
#' @export
#' @examples
#' \dontrun{
#' # mock_data <- data.frame(
#' #   surname = c("Smith", "Doe", "King"),
#' #   preferred_units = c("INFO1111, COMP2222", "INFO1111, DATA3333, ", NA_character_),
#' #   stringsAsFactors = FALSE
#' # )
#' # eoi_extract(mock_data)
#' # # Expected: c("INFO1111", "COMP2222", "DATA3333", "", NA) or similar order
#'
#' # mock_data_empty <- data.frame(surname="Solo", preferred_units=character(0))
#' # eoi_extract(mock_data_empty) # Expected: NULL
#' }
eoi_extract <- function(df) {
  if (!"preferred_units" %in% colnames(df)) {
    stop("The data frame does not contain a 'preferred_units' column.")
  }
  preferred_units_col <- df$preferred_units

  if (length(preferred_units_col) == 0) {
    # strsplit(character(0), ",") is list(), unlist is NULL
    return(unique(unlist(strsplit(preferred_units_col, ",")))) # Returns NULL
  }

  all_units_list <- unlist(strsplit(preferred_units_col, ","))
  trimmed_units <- trimws(all_units_list)

  return(unique(trimmed_units))
}


#' Process EOI Data by Preferred Units
#'
#' Filters Expression of Interest (EOI) data based on a list of unique
#' preferred units of study. For each unit, it creates a subset of the EOI
#' data containing applicants who listed that unit. Each subset is then saved
#' as a CSV file in a unit-specific subdirectory within a main output directory.
#'
#' @param df A data frame or tibble; the EOI data to be processed. Must contain
#'   a column named 'preferred_units'.
#' @param unit_list A character vector of unique unit codes to filter by.
#'   Typically generated by \code{\link{eoi_extract}}.
#' @param save_path A character string; the path to the main output directory
#'   where unit-specific subdirectories and CSV files will be created.
#' @return Invisibly returns a list of data frames, where each element is named
#'   after a unit code and contains the filtered data for that unit.
#' @importFrom readr write_csv
#' @export
#' @examples
#' \dontrun{
#' # mock_eoi_data <- data.frame(
#' #   surname = c("Smith", "Doe", "Chan"),
#' #   preferred_units = c("INFO1111, COMP2222", "INFO1111, DATA3333", "COMP2222"),
#' #   stringsAsFactors = FALSE
#' # )
#' # mock_units <- c("INFO1111", "COMP2222", "DATA3333")
#' # temp_output_dir <- tempfile("eoi_output_")
#' # # dir.create(temp_output_dir) # process_eoi_data creates it
#' #
#' # filtered_results <- process_eoi_data(mock_eoi_data, mock_units, temp_output_dir)
#' #
#' # list.files(temp_output_dir, recursive = TRUE)
#' # # Should show files like:
#' # # INFO1111/INFO1111_data.csv
#' # # COMP2222/COMP2222_data.csv
#' # # DATA3333/DATA3333_data.csv
#' #
#' # unlink(temp_output_dir, recursive = TRUE) # Clean up
#' }
process_eoi_data <- function(df, unit_list, save_path) {
  # Input validation
  if (!is.data.frame(df)) stop("'df' must be a data frame.")
  if (!"preferred_units" %in% colnames(df)) {
    stop("The data frame 'df' does not contain a 'preferred_units' column.")
  }
  if (!is.character(unit_list) && !is.null(unit_list)) { # Allow NULL unit_list
    stop("'unit_list' must be a character vector or NULL.")
  }
  if (!is.character(save_path) || length(save_path) != 1) {
    stop("'save_path' must be a single character string.")
  }

  filtered_data_frames_by_unit <- list()

  if (is.null(unit_list) || length(unit_list) == 0 || nrow(df) == 0) {
    if (nrow(df) == 0) {
      warning("The input data frame ('df') is empty. No filtering performed.")
    }
    if (is.null(unit_list) || length(unit_list) == 0) {
      warning("The 'unit_list' vector is NULL or empty. No units to filter by.")
    }
    return(invisible(filtered_data_frames_by_unit))
  }

  filtered_data_frames_by_unit <- lapply(unit_list, function(unit_name) {
    preferred_units_no_na <- ifelse(is.na(df$preferred_units), "", df$preferred_units)
    matching_rows_indices <- grepl(unit_name, preferred_units_no_na, fixed = TRUE)
    df[matching_rows_indices, , drop = FALSE]
  })
  names(filtered_data_frames_by_unit) <- unit_list

  if (!dir.exists(save_path)) {
    dir.create(save_path, recursive = TRUE, showWarnings = FALSE)
    if (interactive()) cat("Created main output directory:", save_path, "\n")
  }

  if (length(filtered_data_frames_by_unit) > 0) {
    if (interactive()) cat("\nStarting to save filtered data frames...\n")

    for (unit_name in names(filtered_data_frames_by_unit)) {
      df_to_save <- filtered_data_frames_by_unit[[unit_name]]

      # Sanitize unit_name for directory/file naming (original logic)
      sanitized_unit_name <- gsub("[^A-Za-z0-9_.-]+", "_", unit_name)
      sanitized_unit_name <- gsub("^_+|_+$", "", sanitized_unit_name) # Remove leading/trailing underscores

      if (nchar(sanitized_unit_name) == 0) {
        sanitized_unit_name <- "unnamed_unit"
      }

      unit_specific_dir <- file.path(save_path, sanitized_unit_name)

      if (!dir.exists(unit_specific_dir)) {
        dir.create(unit_specific_dir, recursive = TRUE, showWarnings = FALSE)
        if (interactive()) cat("Created subdirectory:", unit_specific_dir, "\n")
      }

      csv_file_path <- file.path(unit_specific_dir, paste0(sanitized_unit_name, "_data.csv"))

      if (nrow(df_to_save) > 0) {
        readr::write_csv(df_to_save, csv_file_path)
        if (interactive()) {
          cat("Saved data for unit '", unit_name, "' (as '", sanitized_unit_name, "') to: ", csv_file_path, "\n")
        }
      } else {
        if (interactive()) {
          cat("No data to save for unit '", unit_name, "' (as '", sanitized_unit_name, "') - data frame is empty.\n")
        }
      }
    }
    if (interactive()) cat("\nAll data frames have been processed for saving.\n")
  } else {
    warning("The 'filtered_data_frames_by_unit' list is empty. No files will be saved.")
  }
  return(invisible(filtered_data_frames_by_unit))
}


#' Check if a Value is Empty or NA
#'
#' Internal helper function to determine if a given value is NA or an empty string.
#' Designed for scalar inputs.
#'
#' @param value The value to check.
#' @return \code{TRUE} if the value is \code{NA} or \code{""}, \code{FALSE} otherwise.
#' @keywords internal
.is_empty_or_na <- function(value) {
  if (length(value) != 1) {
    # This warning can be noisy if called many times with vectors;
    # for internal use, ensure scalar input or adapt function.
    # warning(".is_empty_or_na designed for scalar input.")
    # For robustness with potential vector input from `ad$field` if it's not always scalar:
    # return(is.na(value) | value == "") # This would be vectorized
    # However, sticking to original intent for scalar:
    return(is.na(value[1]) || identical(value[1], ""))
  }
  is.na(value) || identical(value, "")
}

#' Get Value or Default
#'
#' Internal helper function to return a value if it's not empty or NA,
#' otherwise returns a specified default value.
#' Designed for scalar inputs.
#'
#' @param value The value to retrieve.
#' @param default_val The default value to return if 'value' is empty or NA.
#'   Defaults to "N/A".
#' @return The original value, or 'default_val' if the original is empty/NA.
#' @keywords internal
.get_val_or_default <- function(value, default_val = "N/A") {
  if (length(value) != 1) {
    # warning(".get_val_or_default designed for scalar input.")
    # Sticking to original intent for scalar:
    if (.is_empty_or_na(value[1])) {
      return(default_val)
    } else {
      return(value[1])
    }
  }
  if (.is_empty_or_na(value)) default_val else value
}

#' Create EOI Applicant Profile String
#'
#' Generates a formatted string summarizing an applicant's Expression of
#' Interest (EOI) details. This is typically used for display or textual reports.
#'
#' @param applicant_data A list or a single-row data frame/tibble containing
#'   the EOI data for one applicant. Expected to have specific named elements
#'   corresponding to EOI form fields (e.g., `given_name`, `surname`,
#'   `preferred_units`, etc.).
#' @return A character string representing the formatted applicant profile.
#'   The profile is also printed to the console using `cat()`.
#' @export
#' @examples
#' \dontrun{
#' # mock_applicant <- list(
#' #   given_name = "John", surname = "Doe", title = "Mr",
#' #   worked_at_usyd = "Yes", staff_id = "12345",
#' #   preferred_email = "john.doe@example.com", preferred_contact = "0400123456",
#' #   phd_conferred = "Yes",
#' #   previous_demonstrator = "Yes", previous_units = "INFO101, COMP201",
#' #   preferred_units = "DATA301, INFO500",
#' #   desired_hours_per_week = "10-15",
#' #   availability_monday = "All day", availability_tuesday = "AM",
#' #   availability_wednesday = "PM", availability_thursday = "Not available",
#' #   availability_friday = "All day",
#' #   blockout_dates = "Dec 20-Jan 5",
#' #   completed_training = "Yes",
#' #   lead_demonstrator_interest = "Yes - for specific units",
#' #   lead_demonstrator_other = NA,
#' #   expertise_area = "Data Science, Machine Learning",
#' #   higher_education_degrees = "PhD in CS, MSc in Stats",
#' #   teaching_philosophy = "Interactive and engaging.",
#' #   experience_benefit = "Real-world project experience."
#' # )
#' # profile <- create_eoi_profile(mock_applicant)
#' # # The profile string is also printed to the console.
#' }
create_eoi_profile <- function(applicant_data) {
  ad <- applicant_data

  if (is.data.frame(ad) && nrow(ad) == 1) {
    ad <- as.list(ad)
  } else if (!is.list(ad)) {
    stop("applicant_data must be a list or a single-row data frame/tibble.")
  }

  staff_id_val <- ad$staff_id
  staff_id_display <- if (!.is_empty_or_na(ad$worked_at_usyd) && ad$worked_at_usyd == "Yes" &&
    !.is_empty_or_na(staff_id_val)) {
    staff_id_val
  } else {
    "N/A"
  }

  previous_units_val <- ad$previous_units
  previous_units_display <- if (!.is_empty_or_na(ad$previous_demonstrator) && ad$previous_demonstrator == "Yes" &&
    !.is_empty_or_na(previous_units_val)) {
    previous_units_val
  } else {
    "N/A"
  }

  blockout_dates_display <- .get_val_or_default(ad$blockout_dates, "None specified")

  lead_demonstrator_other_line <- ""
  lead_interest_val <- ad$lead_demonstrator_interest
  if (!.is_empty_or_na(lead_interest_val) &&
    grepl("other", tolower(lead_interest_val), fixed = TRUE)) {
    lead_demonstrator_other_line <- sprintf(
      "  Other Details: %s\n",
      .get_val_or_default(ad$lead_demonstrator_other)
    )
  }

  profile_string <- sprintf(
    "Name: %s %s (Title: %s)\nStaff ID: %s\nPreferred Contact: %s, %s\n\nPhD Conferred: %s\nPreviously Demonstrated: %s\nPrevious Units Taught: %s\nPreferred Units to Teach: %s\nDesired Hours per Week: %s\n\nAvailability:\n  Monday: %s\n  Tuesday: %s\n  Wednesday: %s\n  Thursday: %s\n  Friday: %s\nBlockout Dates: %s\n\nCompleted Demonstrator Training: %s\nInterested in Lead Demonstrator Role: %s\n%sExpertise Area:\n%s\n\nHigher Education Degrees:\n%s\n\nTeaching Philosophy:\n%s\n\nHow My Experience Will Benefit Student Learning:\n%s",
    .get_val_or_default(ad$given_name),
    .get_val_or_default(ad$surname),
    .get_val_or_default(ad$title),
    staff_id_display,
    .get_val_or_default(ad$preferred_email),
    .get_val_or_default(ad$preferred_contact),
    .get_val_or_default(ad$phd_conferred),
    .get_val_or_default(ad$previous_demonstrator),
    previous_units_display,
    .get_val_or_default(ad$preferred_units),
    .get_val_or_default(ad$desired_hours_per_week),
    .get_val_or_default(ad$availability_monday),
    .get_val_or_default(ad$availability_tuesday),
    .get_val_or_default(ad$availability_wednesday),
    .get_val_or_default(ad$availability_thursday),
    .get_val_or_default(ad$availability_friday),
    blockout_dates_display,
    .get_val_or_default(ad$completed_training),
    .get_val_or_default(ad$lead_demonstrator_interest),
    lead_demonstrator_other_line,
    .get_val_or_default(ad$expertise_area), # Uses default "N/A"
    .get_val_or_default(ad$higher_education_degrees), # Uses default "N/A"
    .get_val_or_default(ad$teaching_philosophy), # Uses default "N/A"
    .get_val_or_default(ad$experience_benefit) # Uses default "N/A"
  )

  cat(profile_string)
  cat("\n")
  return(profile_string)
}
