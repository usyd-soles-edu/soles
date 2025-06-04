# Alternative approach with more flexible keyword matching
rename_soles_columns_flexible <- function(data) {
  # Define mapping with more specific patterns to avoid conflicts
  column_mapping <- list(
    "worked_at_usyd" = c("worked.*university.*sydney.*previously"),
    "staff_id" = c("staff.*id.*number.*7.*digits"),
    "title" = c("^what.*title"),
    "surname" = c("surname.*family.*name"),
    "given_name" = c("given.*name"),
    "preferred_email" = c("preferred.*email.*address"),
    "preferred_contact" = c("preferred.*contact.*number"),
    "suburb_postcode" = c("suburb.*post.*code"),
    "valid_visa" = c("valid.*working.*visa.*australia"),
    "phd_conferred" = c("phd.*conferred"),
    "previous_demonstrator" = c("demonstrator.*with.*soles.*before"),
    "previous_units" = c("previous.*units.*taught.*year"),
    "preferred_units" = c("select.*units.*want.*considered"),
    "availability_monday" = c("availability.*monday"),
    "availability_tuesday" = c("availability.*tuesday"),
    "availability_wednesday" = c("availability.*wednesday"),
    "availability_thursday" = c("availability.*thursday"),
    "availability_friday" = c("availability.*friday"),
    "lead_demonstrator_interest" = c("lead.*demonstrator.*selected.*choice"),
    "lead_demonstrator_other" = c("lead.*demonstrator.*other.*text"),
    "completed_training" = c("completed.*faculty.*science.*tutor.*demonstrator.*training"),
    "desired_hours_per_week" = c("hours.*allocated.*per.*week"),
    "planned_years" = c("years.*planning.*work"),
    "expertise_area" = c("area.*expertise"),
    "higher_education_degrees" = c("higher.*education.*degree.*major"),
    "teaching_philosophy" = c("teaching.*philosophy"),
    "experience_benefit" = c("experience.*benefit.*school"),
    "blockout_dates" = c("blockout.*dates.*not.*available"),
    "cv_file_id" = c("cv.*id$"),
    "cv_file_name" = c("cv.*name$"),
    "cv_file_size" = c("cv.*size$"),
    "cv_file_type" = c("cv.*type$"),
    "info_acknowledgment" = c("acknowledge.*information.*true.*correct"),
    "info_amendment_acknowledgment" = c("circumstances.*change.*amend.*email")
  )

  # Get current column names (excluding the ones we want to remove)
  current_cols <- names(data)
  cols_to_keep <- current_cols[!current_cols %in% c(
    "Start Date", "End Date", "Response Type",
    "IP Address", "Progress", "Duration (in seconds)",
    "Finished", "Recorded Date", "Response ID",
    "Recipient Last Name", "Recipient First Name",
    "Recipient Email", "External Data Reference",
    "Location Latitude", "Location Longitude",
    "Distribution Channel", "User Language"
  )]

  # Find matches using regex patterns
  rename_vector <- c()
  unmatched_columns <- c()

  for (new_name in names(column_mapping)) {
    pattern <- column_mapping[[new_name]]

    # Find which current column matches the pattern
    matches <- sapply(cols_to_keep, function(col) grepl(pattern, tolower(col), perl = TRUE))
    matching_cols <- cols_to_keep[matches]

    if (length(matching_cols) == 1) {
      # Correct way: new_name = old_name for rename()
      rename_vector[new_name] <- matching_cols
    } else if (length(matching_cols) > 1) {
      # If multiple matches, take the first one and warn
      rename_vector[new_name] <- matching_cols[1]
      message(sprintf(
        "Multiple matches found for '%s': %s. Using '%s'",
        new_name, paste(matching_cols, collapse = ", "), matching_cols[1]
      ))
    } else {
      # No matches found
      unmatched_columns <- c(unmatched_columns, new_name)
    }
  }

  # Report unmatched columns
  if (length(unmatched_columns) > 0) {
    message("The following expected columns were not found:")
    for (col in unmatched_columns) {
      message(sprintf("  - %s not found", col))
    }
  }

  # Define the final columns we want to keep
  final_columns <- c(
    "worked_at_usyd", "staff_id", "title", "surname", "given_name",
    "preferred_email", "preferred_contact", "phd_conferred",
    "previous_demonstrator", "previous_units", "preferred_units",
    "availability_monday", "availability_tuesday", "availability_wednesday",
    "availability_thursday", "availability_friday", "lead_demonstrator_interest",
    "lead_demonstrator_other", "completed_training", "desired_hours_per_week",
    "expertise_area", "higher_education_degrees", "teaching_philosophy",
    "experience_benefit", "blockout_dates"
    # Note: CV columns and acknowledgment columns are not in final_columns,
    # so they will be effectively dropped by the select(any_of(final_columns))
  )

  # Apply renaming and return cleaned data with only specified columns
  # The initial select(-c("Start Date":"User Language")) is handled by how `cols_to_keep` is defined.
  data_cleaned <- data |>
    dplyr::rename(!!!rename_vector) |> # Ensure dplyr::rename is used
    dplyr::select(dplyr::any_of(final_columns)) # Ensure dplyr::select and dplyr::any_of are used

  # Report which final columns are missing
  missing_final_cols <- setdiff(final_columns, names(data_cleaned))
  if (length(missing_final_cols) > 0) {
    message("The following final columns are missing from the cleaned data:")
    for (col in missing_final_cols) {
      message(sprintf("  - %s", col))
    }
  }

  return(data_cleaned)
}

#' Parse Expression of Interest (EOI) CSV File
#'
#' Reads an EOI CSV file, then uses a flexible renaming function to process columns.
#' The function expects a specific CSV structure where actual headers are on the
#' second row and data starts from the fourth row.
#'
#' @param path Character string; the file path to the EOI CSV file.
#' @return A tibble containing the parsed and cleaned EOI data.
#' @importFrom readr read_csv
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

  # Pass raw_data to the flexible renaming function
  out <- rename_soles_columns_flexible(data = raw_data)

  if ("preferred_units" %in% names(out)) {
    out <- out |>
      dplyr::filter(!(is.na(preferred_units) | preferred_units == ""))
  }
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


#' Prepare EOI Data for Archiving
#'
#' Processes a list of data frames (EOI data), generating in-memory CSV content
#' for each. This is suitable for creating archives without disk I/O.
#'
#' @param processed_eoi_data A named list of data frames. Each name is a unit
#'   code, and each data frame contains the processed EOI data for that unit.
#'   Typically the output from \code{\link{process_eoi_data}}.
#' @param uos Optional. A character vector of unit of study codes. If provided,
#'   only data for these units will be processed. If NULL (default), all data is processed.
#' @return A list of lists. Each inner list has:
#'   \code{path}: Intended relative path in an archive (e.g., "UNIT_CODE/UNIT_CODE_data.csv").
#'   \code{content}: CSV content as a character string.
#' @importFrom readr format_csv
#' @importFrom logger log_info log_debug log_warn log_error
#' @export
prepare_eoi <- function(processed_eoi_data, uos = NULL) {
  logger::log_debug(sprintf(
    "Entering prepare_eoi. Number of data frames in list: %d",
    length(processed_eoi_data)
  ))

  output_files <- list() # Initialize the list to store file path and content

  # Filter processed_eoi_data if uos is provided and not empty
  if (!is.null(uos) && length(uos) > 0) {
    uos_filter <- as.character(uos) # Ensure character vector
    logger::log_info(sprintf("Filtering EOI data. Requested UOS: %s", paste(uos_filter, collapse = ", ")))

    original_names <- names(processed_eoi_data)

    # Identify which of the requested UOS are actually present in the data
    valid_uos_to_keep <- intersect(original_names, uos_filter)

    # Filter the list
    processed_eoi_data <- processed_eoi_data[valid_uos_to_keep]

    # Log outcomes
    kept_names <- names(processed_eoi_data)

    if (length(kept_names) > 0) {
      logger::log_debug(sprintf(
        "Successfully filtered. Kept data for UOS: %s. Original count before filtering: %d, Count after filtering: %d.",
        paste(kept_names, collapse = ", "),
        length(original_names),
        length(kept_names)
      ))
    } else {
      logger::log_warn(sprintf(
        "After filtering for requested UOS (%s), no matching data was found or kept. Original data had UOS: %s.",
        paste(uos_filter, collapse = ", "),
        if (length(original_names) > 0) paste(original_names, collapse = ", ") else "none"
      ))
    }

    # Log any requested UOS that were not found in the original data
    requested_but_not_in_original <- setdiff(uos_filter, original_names)
    if (length(requested_but_not_in_original) > 0) {
      logger::log_warn(sprintf(
        "The following requested UOS were not found in the original 'processed_eoi_data' list: %s",
        paste(requested_but_not_in_original, collapse = ", ")
      ))
    }
  }

  if (length(processed_eoi_data) > 0) {
    logger::log_info(sprintf("Starting to process %d data frame(s) for in-memory representation.", length(processed_eoi_data)))

    for (unit_name in names(processed_eoi_data)) {
      df_to_save <- processed_eoi_data[[unit_name]]
      logger::log_debug(sprintf("Processing unit for in-memory representation: '%s'", unit_name))

      sanitized_unit_name <- gsub("[^A-Za-z0-9_.-]+", "_", unit_name)
      sanitized_unit_name <- gsub("^_+|_+$", "", sanitized_unit_name)
      logger::log_debug(sprintf("Sanitized unit name: '%s' (from original: '%s')", sanitized_unit_name, unit_name))

      if (nchar(sanitized_unit_name) == 0) {
        logger::log_warn(sprintf("Sanitized unit name for '%s' is empty. Using 'unnamed_unit'.", unit_name))
        sanitized_unit_name <- "unnamed_unit"
      }

      if (nrow(df_to_save) > 0) {
        # Construct the relative path for the zip archive
        relative_file_path <- paste(sanitized_unit_name, paste0(sanitized_unit_name, "_data.csv"), sep = "/")
        logger::log_debug(sprintf("Intended relative path in archive: %s", relative_file_path))

        tryCatch(
          {
            csv_content <- readr::format_csv(df_to_save)
            output_files[[length(output_files) + 1]] <- list(path = relative_file_path, content = csv_content)
            logger::log_info(sprintf("Generated CSV content for unit '%s' (%d rows) for path: %s", unit_name, nrow(df_to_save), relative_file_path))
          },
          error = function(e) {
            logger::log_error(sprintf("Failed to generate CSV content for unit %s for path %s: %s", unit_name, relative_file_path, e$message))
            warning(sprintf("Failed to generate CSV content for unit %s for path %s. Error: %s", unit_name, relative_file_path, e$message))
            # Continue to next unit if an error occurs for the current one
          }
        )
      } else {
        logger::log_info(sprintf("No data to process for unit '%s' (as '%s') - data frame is empty. Skipping content generation.", unit_name, sanitized_unit_name))
      }
      # Generate and add unit summary file
      logger::log_debug(sprintf("Generating summary for unit: '%s'", unit_name))
      # Define path structure first, using sanitized_unit_name for the folder.
      # The filename for the summary should be "summary.md".
      summary_file_path <- paste(sanitized_unit_name, "summary.md", sep = "/")

      tryCatch(
        {
          # Call soles::generate_unit_summary with the full processed_eoi_data (as elist)
          # and the current unit_name from the loop.
          summary_content <- soles::generate_unit_summary(elist = processed_eoi_data, unit_name = unit_name)

          # Append the new item (list with path and content) to output_files.
          output_files[[length(output_files) + 1]] <- list(path = summary_file_path, content = summary_content)
          logger::log_info(sprintf("Successfully generated and added summary.md for unit '%s' at path: %s", unit_name, summary_file_path))
        },
        error = function(e) {
          logger::log_error(sprintf("Failed to generate summary.md for unit '%s' (intended path: %s): %s", unit_name, summary_file_path, e$message))
          warning(sprintf("Failed to generate summary.md for unit '%s'. Error: %s. Intended path: %s. Skipping summary for this unit.", unit_name, e$message, summary_file_path))
          # Continue to the next unit/iteration of the loop.
        }
      )
    }
    logger::log_info("All data frames have been processed for in-memory representation.")
  } else {
    logger::log_warn("The 'processed_eoi_data' input is empty. No file contents will be generated.")
  }
  logger::log_debug("Exiting prepare_eoi function.")
  return(output_files)
}

#' Process EOI Data by Preferred Units
#'
#' Filters Expression of Interest (EOI) data based on a list of unique
#' preferred units of study. For each unit, it creates a subset of the EOI
#' data containing applicants who listed that unit.
#'
#' @param df A data frame or tibble; the EOI data to be processed. Must contain
#'   a column named 'preferred_units'.
#' @param unit_list A character vector of unique unit codes to filter by.
#'   Typically generated by \code{\link{eoi_extract}}.
#' @return A named list of data frames, where each element is named
#'   after a unit code and contains the filtered EOI data for that unit.
#'   The list can be passed to \code{\link{prepare_eoi}} for archiving.
#' @importFrom logger log_info log_debug log_warn log_error
#' @export
#' @examples
#' \dontrun{
#' # mock_eoi_data <- data.frame(
#' #   surname = c("Smith", "Doe", "Chan"),
#' #   preferred_units = c("INFO1111, COMP2222", "INFO1111, DATA3333", "COMP2222"),
#' #   stringsAsFactors = FALSE
#' # )
#' # mock_units <- c("INFO1111", "COMP2222", "DATA3333")
#' #
#' # # Process data
#' # processed_data <- process_eoi_data(mock_eoi_data, mock_units)
#' # print(names(processed_data))
#' #
#' # # Optionally, save the processed data
#' # # temp_output_dir <- tempfile("eoi_output_")
#' # # prepare_eoi(processed_data) # Renamed from download_eoi_data
#' # # list.files(temp_output_dir, recursive = TRUE)
#' # # unlink(temp_output_dir, recursive = TRUE) # Clean up
#' }
process_eoi_data <- function(df, unit_list) {
  logger::log_debug("Starting process_eoi_data function.")
  logger::log_debug(sprintf("Input df has %d rows and %d columns.", nrow(df), ncol(df)))
  logger::log_debug(sprintf("Input unit_list contains: %s", paste(unit_list, collapse = ", ")))

  # Input validation
  if (!is.data.frame(df)) {
    logger::log_error("'df' must be a data frame.")
    stop("'df' must be a data frame.")
  }
  if (!"preferred_units" %in% colnames(df)) {
    logger::log_error("The data frame 'df' does not contain a 'preferred_units' column.")
    stop("The data frame 'df' does not contain a 'preferred_units' column.")
  }
  if (!is.character(unit_list) && !is.null(unit_list)) { # Allow NULL unit_list
    logger::log_error("'unit_list' must be a character vector or NULL.")
    stop("'unit_list' must be a character vector or NULL.")
  }

  filtered_data_frames_by_unit <- list()

  if (is.null(unit_list) || length(unit_list) == 0 || nrow(df) == 0) {
    if (nrow(df) == 0) {
      logger::log_warn("The input data frame ('df') is empty. No filtering performed.")
      warning("The input data frame ('df') is empty. No filtering performed.")
    }
    if (is.null(unit_list) || length(unit_list) == 0) {
      logger::log_warn("The 'unit_list' vector is NULL or empty. No units to filter by.")
      warning("The 'unit_list' vector is NULL or empty. No units to filter by.")
    }
    logger::log_info("Returning empty list as no filtering can be done.")
    return(filtered_data_frames_by_unit) # Return directly, not invisibly
  }

  logger::log_info("Starting to filter data by unit list.")
  filtered_data_frames_by_unit <- lapply(unit_list, function(unit_name) {
    logger::log_debug(sprintf("Filtering for unit: %s", unit_name))
    preferred_units_no_na <- ifelse(is.na(df$preferred_units), "", df$preferred_units)
    matching_rows_indices <- grepl(unit_name, preferred_units_no_na, fixed = TRUE)
    filtered_df_for_unit <- df[matching_rows_indices, , drop = FALSE]
    logger::log_debug(sprintf("Found %d applicants for unit: %s", nrow(filtered_df_for_unit), unit_name))
    return(filtered_df_for_unit)
  })
  names(filtered_data_frames_by_unit) <- unit_list
  logger::log_info("Finished filtering data by unit list.")

  logger::log_debug("Exiting process_eoi_data function.")
  return(filtered_data_frames_by_unit) # Return the processed data directly
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

  # Process preferred_units for display
  preferred_units_raw_val <- .get_val_or_default(ad$preferred_units)
  if (identical(preferred_units_raw_val, "N/A") || identical(preferred_units_raw_val, "")) {
    preferred_units_display <- "N/A"
  } else {
    units_vec <- trimws(strsplit(preferred_units_raw_val, ",")[[1]])
    units_vec <- units_vec[units_vec != ""] # Remove empty strings after split

    if (length(units_vec) == 0) {
      preferred_units_display <- "N/A"
    } else {
      # Each unit starts with a newline, then indentation and bullet
      formatted_units <- paste0("\n  - ", units_vec)
      # Collapse them into a single string
      preferred_units_display <- paste(formatted_units, collapse = "")
    }
  }

  profile_string <- sprintf(
    "Name: %s %s (Title: %s)\nStaff ID: %s\nPreferred Contact: %s, %s\n\nPhD Conferred: %s\nPreviously Demonstrated: %s\nPrevious Units Taught: %s\nPreferred Units to Teach: %s\n\nAvailability:\n  Monday: %s\n  Tuesday: %s\n  Wednesday: %s\n  Thursday: %s\n  Friday: %s\nBlockout Dates: %s\n\nCompleted Demonstrator Training: %s\nInterested in Lead Demonstrator Role: %s\n%sExpertise Area:\n%s\n\nHigher Education Degrees:\n%s\n\nTeaching Philosophy:\n%s\n\nHow My Experience Will Benefit Student Learning:\n%s",
    .get_val_or_default(ad$given_name),
    .get_val_or_default(ad$surname),
    .get_val_or_default(ad$title),
    staff_id_display,
    .get_val_or_default(ad$preferred_email),
    .get_val_or_default(ad$preferred_contact),
    .get_val_or_default(ad$phd_conferred),
    .get_val_or_default(ad$previous_demonstrator),
    previous_units_display,
    preferred_units_display,
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
  return(invisible(profile_string))
}
