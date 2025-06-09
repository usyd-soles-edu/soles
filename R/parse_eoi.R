# Alternative approach with more flexible keyword matching
#' Rename Columns Flexibly for SOLES EOI Data
#'
#' This helper function renames columns of a data frame based on a predefined
#' mapping using regular expressions. It's designed to handle variations in
#' column names from EOI (Expression of Interest) forms. It also selects a
#' specific set of final columns.
#'
#' @param data A data frame or tibble; the raw EOI data.
#' @return A tibble with renamed and selected columns. Columns that are expected
#'   but not found, or columns that have multiple matches, will generate messages.
#' @importFrom dplyr rename select any_of
#' @noRd
#' @examples
#' \dontrun{
#' # mock_raw_data <- data.frame(
#' #   "What is your surname or family name?" = "Test",
#' #   "What is your given name(s)?" = "User",
#' #   "Which units would you like to be considered for?" = "INFO1111",
#' #   "Start Date" = "2023-01-01", # This column will be dropped
#' #   check.names = FALSE
#' # )
#' # cleaned_data <- rename_soles_columns_flexible(mock_raw_data)
#' # print(names(cleaned_data))
#' }
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
#' second row and data starts from the fourth row. It also filters out rows
#' where 'preferred_units' is NA or an empty string.
#'
#' @param path Character string; the file path to the EOI CSV file.
#' @return A tibble containing the parsed, cleaned, and filtered EOI data.
#' @importFrom readr read_csv
#' @importFrom dplyr filter
#' @export
#' @examples
#' \dontrun{
#' # Create a dummy CSV file for the example
#' temp_csv_content <- paste(
#'   "Col1,Col2,Col3",
#'   "Question Text,What is your surname or family name?,Which units would you like to be considered for?",
#'   "ResponseID,Q1,Q2",
#'   "1,Smith,INFO1111",
#'   "2,Doe,COMP2222",
#'   "3,Jones,", # Empty preferred_units
#'   "4,Brown,NA", # NA preferred_units
#'   sep = "\n"
#' )
#' temp_file <- tempfile(fileext = ".csv")
#' writeLines(temp_csv_content, temp_file)
#'
#' # Parse the dummy EOI data
#' eoi_data <- parse_eoi(temp_file)
#' print(head(eoi_data))
#' # Expected output should not contain Jones or Brown if filter is applied correctly.
#'
#' # Clean up the dummy file
#' unlink(temp_file)
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
#' mock_data <- data.frame(
#'   surname = c("Smith", "Doe", "King"),
#'   preferred_units = c("INFO1111, COMP2222", "INFO1111, DATA3333, ", NA_character_),
#'   stringsAsFactors = FALSE
#' )
#' eoi_extract(mock_data)
#' # Expected: c("INFO1111", "COMP2222", "DATA3333", "", NA) or similar order
#'
#' mock_data_empty_col <- data.frame(surname = "Solo", preferred_units = character(0))
#' eoi_extract(mock_data_empty_col) # Expected: NULL
#'
#' mock_data_no_col <- data.frame(surname = "NoUnitsField")
#' # eoi_extract(mock_data_no_col) # This would error, as expected.
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
#' for each, along with a summary markdown file. This is suitable for creating
#' archives without disk I/O.
#'
#' @param processed_eoi_data A named list of data frames. Each name is a unit
#'   code, and each data frame contains the processed EOI data for that unit.
#'   Typically the output from \code{\link{process_eoi_data}}.
#' @param uos Optional. A character vector of unit of study codes. If provided,
#'   only data for these units will be processed. If NULL (default), all data is processed.
#' @return A list of lists. Each inner list has:
#'   \code{path}: Intended relative path in an archive (e.g., "UNIT_CODE/UNIT_CODE_data.csv" or "UNIT_CODE/summary.md").
#'   \code{content}: CSV content as a character string for data files, or Markdown content for summary files.
#' @importFrom readr format_csv
#' @importFrom logger log_info log_debug log_warn log_error
#' @export
#' @examples
#' \dontrun{
#' # Mock processed EOI data
#' mock_df_info1111 <- data.frame(surname = "Smith", preferred_units = "INFO1111")
#' mock_df_comp2222 <- data.frame(surname = "Doe", preferred_units = "COMP2222")
#' mock_processed_data <- list(
#'   INFO1111 = mock_df_info1111,
#'   COMP2222 = mock_df_comp2222
#' )
#'
#' # Prepare data for all units
#' archive_content_all <- prepare_eoi(mock_processed_data)
#' print(length(archive_content_all)) # Expected: 4 (2 CSVs, 2 summaries)
#' print(sapply(archive_content_all, function(x) x$path))
#'
#' # Prepare data for a specific unit
#' archive_content_info <- prepare_eoi(mock_processed_data, uos = "INFO1111")
#' print(length(archive_content_info)) # Expected: 2 (1 CSV, 1 summary for INFO1111)
#' print(sapply(archive_content_info, function(x) x$path))
#'
#' # Example with a unit not present
#' archive_content_missing <- prepare_eoi(mock_processed_data, uos = "PHYS1001")
#' print(length(archive_content_missing)) # Expected: 0
#' }
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
#'   Typically generated by \code{\link{eoi_extract}}. Can be NULL or empty,
#'   in which case an empty list is returned.
#' @return A named list of data frames, where each element is named
#'   after a unit code and contains the filtered EOI data for that unit.
#'   The list can be passed to \code{\link{prepare_eoi}} for archiving.
#'   Returns an empty list if `df` is empty, or if `unit_list` is NULL or empty.
#' @importFrom logger log_info log_debug log_warn log_error
#' @export
#' @examples
#' \dontrun{
#' mock_eoi_data <- data.frame(
#'   surname = c("Smith", "Doe", "Chan", "Lee"),
#'   preferred_units = c("INFO1111, COMP2222", "INFO1111, DATA3333", "COMP2222", NA_character_),
#'   stringsAsFactors = FALSE
#' )
#' mock_units <- c("INFO1111", "COMP2222", "DATA3333", "PHYS1001") # PHYS1001 not in data
#'
#' # Process data
#' processed_data <- process_eoi_data(mock_eoi_data, mock_units)
#' print(names(processed_data)) # Expected: "INFO1111" "COMP2222" "DATA3333" "PHYS1001"
#' print(processed_data$INFO1111) # Smith, Doe
#' print(processed_data$COMP2222) # Smith, Chan
#' print(processed_data$DATA3333) # Doe
#' print(processed_data$PHYS1001) # Empty data frame
#'
#' # Example with empty unit list
#' processed_empty_units <- process_eoi_data(mock_eoi_data, character(0))
#' print(length(processed_empty_units)) # Expected: 0
#'
#' # Example with empty data frame
#' processed_empty_df <- process_eoi_data(mock_eoi_data[0, ], mock_units)
#' print(length(processed_empty_df)) # Expected: 0
#' }
process_eoi_data <- function(df, unit_list) {
  logger::log_debug("Starting process_eoi_data function.")
  logger::log_debug(sprintf("Input df has %d rows and %d columns.", nrow(df), ncol(df)))
  if (!is.null(unit_list) && length(unit_list) > 0) {
    logger::log_debug(sprintf("Input unit_list contains: %s", paste(unit_list, collapse = ", ")))
  } else {
    logger::log_debug("Input unit_list is NULL or empty.")
  }


  # Input validation
  if (!is.data.frame(df)) {
    logger::log_error("'df' must be a data frame.")
    stop("'df' must be a data frame.")
  }
  if (!"preferred_units" %in% colnames(df) && nrow(df) > 0) { # Check only if df not empty
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
      # warning("The input data frame ('df') is empty. No filtering performed.") # Redundant with log
    }
    if (is.null(unit_list) || length(unit_list) == 0) {
      logger::log_warn("The 'unit_list' vector is NULL or empty. No units to filter by.")
      # warning("The 'unit_list' vector is NULL or empty. No units to filter by.") # Redundant with log
    }
    logger::log_info("Returning empty list as no filtering can be done.")
    return(filtered_data_frames_by_unit) # Return directly, not invisibly
  }

  logger::log_info("Starting to filter data by unit list.")
  filtered_data_frames_by_unit <- lapply(unit_list, function(unit_name) {
    logger::log_debug(sprintf("Filtering for unit: %s", unit_name))
    # Ensure preferred_units exists before trying to access it
    if ("preferred_units" %in% names(df)) {
      preferred_units_no_na <- ifelse(is.na(df$preferred_units), "", df$preferred_units)
      matching_rows_indices <- grepl(unit_name, preferred_units_no_na, fixed = TRUE)
      filtered_df_for_unit <- df[matching_rows_indices, , drop = FALSE]
      logger::log_debug(sprintf("Found %d applicants for unit: %s", nrow(filtered_df_for_unit), unit_name))
      return(filtered_df_for_unit)
    } else {
      # Should not happen if initial checks are robust, but as a safeguard
      logger::log_warn(sprintf("Column 'preferred_units' not found while filtering for unit %s. Returning empty data frame for this unit.", unit_name))
      return(df[0, , drop = FALSE]) # Return empty df structure
    }
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
#' @return \code{TRUE} if the value is \code{NULL}, \code{NA}, or an empty string (\code{""}), \code{FALSE} otherwise.
#' @keywords internal
.is_empty_or_na <- function(value) {
  if (is.null(value)) { # Check for NULL first
    return(TRUE)
  }
  # Original logic for non-NULL values:
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
#' @return A character string representing the formatted applicant profile in Markdown.
#'   Empty or "N/A" fields are generally omitted.
#' @export
#' @examples
#' \dontrun{
#' mock_applicant <- list(
#'   given_name = "Jane", surname = "Doe", title = "Dr",
#'   worked_at_usyd = "Yes", staff_id = "9876543",
#'   preferred_email = "jane.doe@example.com", preferred_contact = "0412345678",
#'   phd_conferred = "Yes",
#'   previous_demonstrator = "No", previous_units = NA, # or ""
#'   preferred_units = "DATA4001, COMP5002",
#'   desired_hours_per_week = "Up to 20",
#'   availability_monday = "Not available", availability_tuesday = "Full Day",
#'   availability_wednesday = "AM only", availability_thursday = "PM only",
#'   availability_friday = "Full Day",
#'   blockout_dates = "None",
#'   completed_training = "Yes, in 2022",
#'   lead_demonstrator_interest = "Maybe",
#'   lead_demonstrator_other = "Interested in curriculum development support",
#'   expertise_area = "Statistical Modeling, Bioinformatics",
#'   higher_education_degrees = "PhD (Statistics), BSc (Biology)",
#'   teaching_philosophy = "Student-centered and practical.",
#'   experience_benefit = "Industry experience in data analysis."
#' )
#' profile_string <- create_eoi_profile(mock_applicant)
#' # cat(profile_string) # To see the output as it would be printed
#'
#' # Example with a data frame row
#' mock_applicant_df <- as.data.frame(mock_applicant)
#' profile_string_df <- create_eoi_profile(mock_applicant_df)
#' # cat(profile_string_df)
#' }
create_eoi_profile <- function(applicant_data) {
  # Ensure applicant_data is a list-like structure
  if (is.data.frame(applicant_data)) {
    if (nrow(applicant_data) != 1) {
      stop("If applicant_data is a data frame, it must contain only one row.")
    }
    ad <- as.list(applicant_data)
  } else if (is.list(applicant_data)) {
    ad <- applicant_data
  } else {
    stop("applicant_data must be a list or a single-row data frame.")
  }

  # Helper to safely get values, using .get_val_or_default
  get_val <- function(field_name, default_val = "N/A") {
    val <- ad[[field_name]]
    .get_val_or_default(val, default_val)
  }

  # Helper function to map availability text to AM/PM symbols or text
  map_availability_to_symbols <- function(avail_text) {
    norm_text <- tolower(trimws(avail_text))

    # Handle clear "unavailable" cases first
    if (norm_text %in% c("", "n/a", "not available", "unavailable") ||
      grepl("unavailable", norm_text, fixed = TRUE)) {
      return(list(am = "x", pm = "x"))
    }

    is_full_day <- norm_text == "full day"
    has_am_mention <- grepl("am|morning", norm_text)
    has_pm_mention <- grepl("pm|afternoon", norm_text)

    # Initialize symbols assuming not available, then prove availability
    am_symbol <- "x"
    pm_symbol <- "x"

    if (is_full_day || (has_am_mention && has_pm_mention)) {
      # Covers "full day" or cases like "AM and PM", "Morning, Afternoon"
      am_symbol <- "✓"
      pm_symbol <- "✓"
    } else if (has_am_mention) {
      # AM mentioned, and not PM (because previous condition was false)
      am_symbol <- "✓"
      # pm_symbol remains "x"
    } else if (has_pm_mention) {
      # PM mentioned, and not AM (because previous conditions were false)
      # am_symbol remains "x"
      pm_symbol <- "✓"
    } else {
      # Not explicitly unavailable, not full day, no clear AM/PM only.
      # This is for other non-empty strings like "flexible", "by appointment".
      return(list(am = "?", pm = "?"))
    }

    return(list(am = am_symbol, pm = pm_symbol))
  }

  # Helper to format multi-line text for table cells (escape pipes, then replace newline with <br>)
  format_for_table_cell <- function(text) {
    if (text == "N/A") {
      return("")
    }
    if (!is.character(text)) text <- as.character(text)
    # Order of escaping matters:
    # 1. Escape backslashes first to prevent them from interfering with subsequent escapes
    text <- gsub("\\\\", "\\\\\\\\", text) # Replace \ with \\
    # 2. Escape pipe characters
    text <- gsub("\\|", "\\\\|", text) # Replace | with \|
    # 3. Replace newlines with <br> for HTML display in Markdown tables
    text <- gsub("\n", "<br>", text)
    return(text)
  }

  profile_parts <- c()

  # 1. Applicant Name (H1)
  title_val <- get_val("title")
  given_name_val <- get_val("given_name")
  surname_val <- get_val("surname")

  name_parts_vec <- c()
  if (title_val != "N/A") name_parts_vec <- c(name_parts_vec, title_val)
  if (given_name_val != "N/A") name_parts_vec <- c(name_parts_vec, given_name_val)
  if (surname_val != "N/A") name_parts_vec <- c(name_parts_vec, surname_val)

  if (length(name_parts_vec) > 0) {
    profile_parts <- c(profile_parts, paste0("# ", paste(name_parts_vec, collapse = " "), "\n\n"))
  } else {
    profile_parts <- c(profile_parts, "# Applicant Profile\n\n") # Fallback
  }

  # 2. Key Information Table
  key_info_rows <- c()

  email_val <- get_val("preferred_email")
  if (email_val != "N/A") {
    key_info_rows <- c(key_info_rows, sprintf("|**Email**|%s|", email_val))
  }

  contact_val <- get_val("preferred_contact")
  if (contact_val != "N/A") {
    key_info_rows <- c(key_info_rows, sprintf("|**Phone**|%s|", contact_val))
  }

  worked_usyd_val <- get_val("worked_at_usyd")
  staff_id_val <- get_val("staff_id")
  usyd_staff_info <- worked_usyd_val
  if (tolower(worked_usyd_val) == "yes" && staff_id_val != "N/A") {
    usyd_staff_info <- sprintf("Yes (ID: %s)", staff_id_val)
  }
  if (worked_usyd_val != "N/A") {
    key_info_rows <- c(key_info_rows, sprintf("|**USYD Staff**|%s|", usyd_staff_info))
  }

  phd_val <- get_val("phd_conferred")
  if (phd_val != "N/A") {
    key_info_rows <- c(key_info_rows, sprintf("|**PhD Conferred**|%s|", phd_val))
  }

  training_val <- get_val("completed_training")
  if (training_val != "N/A") {
    key_info_rows <- c(key_info_rows, sprintf("|**Completed Faculty Training**|%s|", training_val))
  }

  lead_interest_val <- get_val("lead_demonstrator_interest")
  lead_other_val <- get_val("lead_demonstrator_other")
  lead_info <- lead_interest_val
  if (lead_interest_val != "N/A" && tolower(lead_interest_val) != "no" && lead_other_val != "N/A") {
    lead_info <- sprintf("%s (Details: %s)", lead_interest_val, lead_other_val)
  }
  if (lead_interest_val != "N/A") {
    key_info_rows <- c(key_info_rows, sprintf("|**Interest in Lead Demonstrator Role**|%s|", lead_info))
  }

  desired_hours_val <- get_val("desired_hours_per_week")
  if (desired_hours_val != "N/A") {
    key_info_rows <- c(key_info_rows, sprintf("|**Desired Hours per Week:**|%s|", desired_hours_val))
  }

  blockout_val <- get_val("blockout_dates")
  if (blockout_val != "N/A") {
    key_info_rows <- c(key_info_rows, sprintf("|**Blockout Dates:**|%s|", blockout_val))
  }

  if (length(key_info_rows) > 0) {
    key_info_header <- c(
      "|Key Information|Details|",
      "|---|---|"
    )
    profile_parts <- c(profile_parts, paste(c(key_info_header, key_info_rows), collapse = "\n"), "\n\n")
  }

  # 3. Availability Table
  availability_rows <- c()
  days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  day_fields <- paste0("availability_", tolower(days))

  has_availability_info <- FALSE
  for (i in seq_along(days)) {
    avail_val_text <- get_val(day_fields[i])
    # Check if any availability info exists, even if it's "N/A" which will be mapped to 'x'
    # The goal is to show the table if any day has a non-default (empty) value from source.
    # get_val returns "N/A" if field is missing or empty.
    # So, if avail_val_text is ever different from the default "N/A" for a missing field,
    # or if it's explicitly set, we consider it info.
    # A simpler check: if any day's value is not the default "N/A" from get_val for a truly absent field.
    # However, the original data might contain "N/A" as a string.
    # Let's assume if get_val returns something other than its own default_val for a missing field,
    # or if the field exists, it's info.
    # The current get_val returns "N/A" for empty/missing.
    # So, if any avail_val_text is *ever* populated (even with "N/A" from data), we show table.
    # This means we always show the table if the fields are expected.
    # Let's refine: only show table if at least one day has an actual stated availability
    # (i.e., not just "N/A" from get_val because the field was missing).
    # The current logic of map_availability_to_symbols will produce 'x' for "N/A".
    # We need to know if the "N/A" was from actual data or from get_val default.
    # For simplicity, if any of the availability fields exist in `ad`, we show the table.
    # A pragmatic approach: if any `ad[[day_field]]` is not NULL.
    raw_avail_values <- sapply(day_fields, function(field) !is.null(ad[[field]]))
    if (any(raw_avail_values)) {
      has_availability_info <- TRUE
    }

    symbols <- map_availability_to_symbols(avail_val_text) # avail_val_text can be "N/A"
    availability_rows <- c(availability_rows, sprintf("|%s|%s|%s|", days[i], symbols$am, symbols$pm))
  }

  if (has_availability_info) {
    availability_header <- c(
      "## Availability\n",
      "|Day|AM (8am - 1pm)|PM (1pm - 6pm)|",
      "|---|:---:|:---:|"
    )
    profile_parts <- c(profile_parts, paste(c(availability_header, availability_rows), collapse = "\n"), "\n\n")
  }

  # 4. Teaching Experience Table
  prev_demo_val <- get_val("previous_demonstrator", "")
  prev_units_val <- get_val("previous_units", "")
  if (prev_units_val != "") {
    prev_units_val <- gsub("\n", " ", prev_units_val) # Replace newlines with spaces
    prev_units_val <- trimws(gsub("\\s+", " ", prev_units_val)) # Replace multiple spaces with single and trim
  }
  pref_units_val <- get_val("preferred_units", "")

  if (prev_demo_val != "" || prev_units_val != "" || pref_units_val != "") {
    teaching_exp_header <- c(
      "## Teaching Experience\n",
      "|**Previous SOLES Demonstrator**|**Previous Units Taught**|**Preferred Units for Consideration**|",
      "|---|---|---|"
    )
    teaching_exp_row <- sprintf(
      "|%s|%s|%s|",
      prev_demo_val, prev_units_val, pref_units_val
    )
    profile_parts <- c(profile_parts, paste(c(teaching_exp_header, teaching_exp_row), collapse = "\n"), "\n\n")
  }

  # 5. Background Table
  background_rows <- c()

  expertise_val <- get_val("expertise_area")
  if (expertise_val != "N/A") {
    background_rows <- c(background_rows, sprintf("|**Area(s) of Expertise**|%s|", format_for_table_cell(expertise_val)))
  }

  degrees_val <- get_val("higher_education_degrees")
  if (degrees_val != "N/A") {
    background_rows <- c(background_rows, sprintf("|**Higher Education Degrees & Majors**|%s|", format_for_table_cell(degrees_val)))
  }

  philosophy_val <- get_val("teaching_philosophy")
  if (philosophy_val != "N/A") {
    background_rows <- c(background_rows, sprintf("|**Teaching Philosophy**|%s|", format_for_table_cell(philosophy_val)))
  }

  experience_val <- get_val("experience_benefit")
  if (experience_val != "N/A") {
    background_rows <- c(background_rows, sprintf("|**How Experience Benefits School**|%s|", format_for_table_cell(experience_val)))
  }

  if (length(background_rows) > 0) {
    background_header <- c(
      "## Background\n",
      "|**Additional Information**|**Description**|",
      "|---|---|"
    )
    profile_parts <- c(profile_parts, paste(c(background_header, background_rows), collapse = "\n"), "\n\n")
  }

  # Combine all parts
  final_profile_string <- paste(profile_parts, collapse = "")

  # Cleanup multiple newlines:
  # 1. Reduce sequences of 3 or more newlines to 2 newlines.
  final_profile_string <- gsub("\n{3,}", "\n\n", final_profile_string)

  # 2. Trim whitespace from the end of the string.
  final_profile_string <- trimws(final_profile_string, which = "right")

  # 3. Ensure a single trailing newline if the string is not empty.
  if (nzchar(final_profile_string)) {
    final_profile_string <- paste0(final_profile_string, "\n")
  }

  return(final_profile_string)
}
