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
  # Patterns handle both old and new EOI form versions
  column_mapping <- list(
    "worked_at_usyd" = paste0(
      "worked.*university.*sydney.*previously|",
      "employed.*casual.*academic.*soles.*before"
    ),
    "hdr_student" = "current.*hdr.*student.*soles",
    "staff_id" = "staff.*id.*number.*7.*digits",
    "title" = "^what.*title",
    "surname" = "surname.*family.*name",
    "given_name" = "given.*name|first.*name.*given.*name",
    "preferred_email" = "preferred.*email.*address",
    "preferred_contact" = "preferred.*contact.*number",
    "suburb_postcode" = "suburb.*post.*code",
    "valid_visa" = "valid.*working.*visa.*australia",
    "phd_conferred" = "phd.*conferred",
    "previous_demonstrator" = paste0(
      "demonstrator.*with.*soles.*before|",
      "current.*hdr.*student.*soles"
    ),
    "previous_units" = "previous.*units.*taught.*year",
    "preferred_units" = "select.*units.*want.*considered",
    "availability_monday" = "availability.*s1.*2026.*monday",
    "availability_tuesday" = "availability.*s1.*2026.*tuesday",
    "availability_wednesday" = "availability.*s1.*2026.*wednesday",
    "availability_thursday" = "availability.*s1.*2026.*thursday",
    "availability_friday" = "availability.*s1.*2026.*friday",
    "lead_demonstrator_interest" = "lead.*demonstrator.*selected.*choice",
    "lead_demonstrator_other" = "lead.*demonstrator.*other.*text",
    "completed_training" = paste0(
      "completed.*faculty.*science.*tutor.*demonstrator.*training|",
      "previously.*completed.*faculty.*science.*tutor.*",
      "demonstrator.*training"
    ),
    "expertise_area" = "area.*expertise",
    "higher_education_degrees" = "higher.*education.*degree.*major",
    "teaching_philosophy" = paste0(
      "teaching.*philosophy|",
      "200.*words.*less.*qualifications.*experience.*skills"
    ),
    "experience_benefit" = "experience.*benefit.*school",
    "blockout_dates" = "blockout.*dates.*not.*available",
    "cv_file_id" = "cv.*id$|pdf.*cv.*id$",
    "cv_file_name" = "cv.*name$|pdf.*cv.*name$",
    "cv_file_size" = "cv.*size$|pdf.*cv.*size$",
    "cv_file_type" = "cv.*type$|pdf.*cv.*type$",
    "info_acknowledgment" = "acknowledge.*information.*true.*correct",
    "info_amendment_acknowledgment" = "circumstances.*change.*amend.*email"
  )

  # Define columns to exclude (vectorised operation)
  cols_to_exclude <- c(
    "Start Date", "End Date", "Response Type",
    "IP Address", "Progress", "Duration (in seconds)",
    "Finished", "Recorded Date", "Response ID",
    "Recipient Last Name", "Recipient First Name",
    "Recipient Email", "External Data Reference",
    "Location Latitude", "Location Longitude",
    "Distribution Channel", "User Language"
  )

  # Get current column names (excluding unwanted ones)
  current_cols <- names(data)
  cols_to_keep <- current_cols[!current_cols %in% cols_to_exclude]

  # Pre-compute lowercased column names ONCE (major optimisation)
  cols_to_keep_lower <- tolower(cols_to_keep)

  # Vectorised pattern matching
  rename_vector <- character()
  unmatched_columns <- character()

  for (new_name in names(column_mapping)) {
    pattern <- column_mapping[[new_name]]

    # Use pre-lowercased columns (avoid repeated tolower calls)
    matches <- grepl(pattern, cols_to_keep_lower, perl = TRUE)
    matching_cols <- cols_to_keep[matches]

    if (length(matching_cols) == 1) {
      rename_vector[new_name] <- matching_cols
    } else if (length(matching_cols) > 1) {
      # Take first match and warn
      rename_vector[new_name] <- matching_cols[1]
      message(sprintf(
        "Multiple matches found for '%s': %s. Using '%s'",
        new_name, paste(matching_cols, collapse = ", "), matching_cols[1]
      ))
    } else {
      unmatched_columns <- c(unmatched_columns, new_name)
    }
  }

  # Report unmatched columns (only if any exist)
  if (length(unmatched_columns) > 0) {
    message("The following expected columns were not found:")
    message(paste("  -", unmatched_columns, collapse = "\n"))
  }

  # Define the final columns we want to keep
  final_columns <- c(
    "worked_at_usyd", "hdr_student", "staff_id", "title", "surname",
    "given_name", "preferred_email", "preferred_contact", "phd_conferred",
    "previous_demonstrator", "previous_units", "preferred_units",
    "availability_monday", "availability_tuesday", "availability_wednesday",
    "availability_thursday", "availability_friday", "lead_demonstrator_interest",
    "lead_demonstrator_other", "completed_training",
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
    return(character(0))
  }

  # Remove NAs and empty strings upfront (vectorised)
  valid_entries <- preferred_units_col[!is.na(preferred_units_col) & preferred_units_col != ""]

  if (length(valid_entries) == 0) {
    return(character(0))
  }

  # Vectorised approach: extract unit codes using regex pattern
  # Unit code pattern: 4 letters followed by 4 alphanumeric characters
  # Examples: BIOL1009, BIOL1XX7, BIOL2X11, MIMI2X02
  # This handles units with commas in names like "MIMI2X02 Microbes, Infection"

  # Combine all valid entries into single string
  all_text <- paste(valid_entries, collapse = " ")

  # Extract all unit codes using the pattern
  # Pattern explanation: \b = word boundary, [A-Z]{4} = 4 uppercase letters,
  # [A-Z0-9]{4} = 4 alphanumeric characters (letters or numbers)
  unit_pattern <- "\\b[A-Z]{4}[A-Z0-9]{4}\\b"
  units_extracted <- stringr::str_extract_all(all_text, unit_pattern)[[1]]

  # Get unique unit codes
  units_final <- unique(units_extracted)

  return(units_final)
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
  # Lazy logging for debug messages
  if (logger::log_threshold() <= logger::DEBUG) {
    logger::log_debug(sprintf(
      "Entering prepare_eoi. Number of data frames in list: %d",
      length(processed_eoi_data)
    ))
  }

  # Pre-allocate output list
  output_files <- list()

  # Filter processed_eoi_data if uos is provided and not empty
  if (!is.null(uos) && length(uos) > 0) {
    uos_filter <- as.character(uos)
    logger::log_info(sprintf(
      "Filtering EOI data. Requested UOS: %s",
      paste(uos_filter, collapse = ", ")
    ))

    original_names <- names(processed_eoi_data)

    # Identify which requested UOS are actually present in the data
    valid_uos_to_keep <- intersect(original_names, uos_filter)

    # Filter the list
    processed_eoi_data <- processed_eoi_data[valid_uos_to_keep]

    # Log outcomes
    kept_names <- names(processed_eoi_data)

    if (length(kept_names) > 0) {
      if (logger::log_threshold() <= logger::DEBUG) {
        logger::log_debug(sprintf(
          "Filtered successfully. Kept UOS: %s (original: %d, after: %d)",
          paste(kept_names, collapse = ", "),
          length(original_names),
          length(kept_names)
        ))
      }
    } else {
      logger::log_warn(sprintf(
        "After filtering for UOS (%s), no matching data found. Original: %s",
        paste(uos_filter, collapse = ", "),
        if (length(original_names) > 0) {
          paste(original_names, collapse = ", ")
        } else {
          "none"
        }
      ))
    }

    # Log any requested UOS not found in original data
    requested_but_not_in_original <- setdiff(uos_filter, original_names)
    if (length(requested_but_not_in_original) > 0) {
      logger::log_warn(sprintf(
        "Requested UOS not found: %s",
        paste(requested_but_not_in_original, collapse = ", ")
      ))
    }
  }

  if (length(processed_eoi_data) > 0) {
    logger::log_info(sprintf(
      "Processing %d data frame(s) for in-memory representation",
      length(processed_eoi_data)
    ))

    for (unit_name in names(processed_eoi_data)) {
      df_to_save <- processed_eoi_data[[unit_name]]

      if (logger::log_threshold() <= logger::DEBUG) {
        logger::log_debug(sprintf(
          "Processing unit for in-memory representation: '%s'",
          unit_name
        ))
      }

      # Sanitise unit name for file paths
      sanitized_unit_name <- gsub("[^A-Za-z0-9_.-]+", "_", unit_name)
      sanitized_unit_name <- gsub("^_+|_+$", "", sanitized_unit_name)

      if (logger::log_threshold() <= logger::DEBUG) {
        logger::log_debug(sprintf(
          "Sanitised unit name: '%s' (from: '%s')",
          sanitized_unit_name, unit_name
        ))
      }

      if (nchar(sanitized_unit_name) == 0) {
        logger::log_warn(sprintf("Sanitized unit name for '%s' is empty. Using 'unnamed_unit'.", unit_name))
        sanitized_unit_name <- "unnamed_unit"
      }

      if (nrow(df_to_save) > 0) {
        # Construct the relative path for the zip archive
        relative_file_path <- paste(
          sanitized_unit_name,
          paste0(sanitized_unit_name, "_data.csv"),
          sep = "/"
        )

        if (logger::log_threshold() <= logger::DEBUG) {
          logger::log_debug(sprintf(
            "Intended relative path in archive: %s",
            relative_file_path
          ))
        }

        tryCatch(
          {
            csv_content <- readr::format_csv(df_to_save)
            output_files[[length(output_files) + 1]] <- list(
              path = relative_file_path,
              content = csv_content
            )

            logger::log_info(sprintf(
              "Generated CSV for unit '%s' (%d rows): %s",
              unit_name, nrow(df_to_save), relative_file_path
            ))
          },
          error = function(e) {
            logger::log_error(sprintf(
              "Failed to generate CSV for unit %s: %s",
              unit_name, e$message
            ))
            warning(sprintf(
              "Failed to generate CSV for unit %s. Error: %s",
              unit_name, e$message
            ))
          }
        )
      } else {
        logger::log_info(sprintf(
          "No data for unit '%s' - data frame is empty. Skipping.",
          unit_name
        ))
      }
      # Generate and add unit summary file
      if (logger::log_threshold() <= logger::DEBUG) {
        logger::log_debug(sprintf(
          "Generating summary for unit: '%s'",
          unit_name
        ))
      }

      # Define path structure first, using sanitized_unit_name for folder
      summary_file_path <- paste(sanitized_unit_name, "summary.md", sep = "/")

      tryCatch(
        {
          # OPTIMISATION: Pass only the current unit's data instead of full list
          # generate_unit_summary can work with a single-element list
          unit_data_list <- list()
          unit_data_list[[unit_name]] <- df_to_save

          summary_content <- soles::generate_unit_summary(
            elist = unit_data_list,
            unit_name = unit_name
          )

          # Append the new item to output_files
          output_files[[length(output_files) + 1]] <- list(
            path = summary_file_path,
            content = summary_content
          )

          logger::log_info(sprintf(
            "Successfully generated summary.md for unit '%s' at path: %s",
            unit_name, summary_file_path
          ))
        },
        error = function(e) {
          logger::log_error(sprintf(
            "Failed to generate summary.md for unit '%s' (path: %s): %s",
            unit_name, summary_file_path, e$message
          ))
          warning(sprintf(
            "Failed to generate summary.md for unit '%s'. Error: %s",
            unit_name, e$message
          ))
        }
      )
    }
    logger::log_info(
      "All data frames have been processed for in-memory representation."
    )
  } else {
    logger::log_warn(
      "The 'processed_eoi_data' input is empty. No file contents generated."
    )
  }

  if (logger::log_threshold() <= logger::DEBUG) {
    logger::log_debug("Exiting prepare_eoi function.")
  }

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
  # Lazy logging - only evaluate if logger threshold allows
  if (logger::log_threshold() <= logger::DEBUG) {
    logger::log_debug("Starting process_eoi_data function.")
    logger::log_debug(sprintf(
      "Input df has %d rows and %d columns.",
      nrow(df), ncol(df)
    ))
    if (!is.null(unit_list) && length(unit_list) > 0) {
      logger::log_debug(sprintf(
        "Input unit_list contains: %s",
        paste(unit_list, collapse = ", ")
      ))
    } else {
      logger::log_debug("Input unit_list is NULL or empty.")
    }
  }

  # Input validation
  if (!is.data.frame(df)) {
    logger::log_error("'df' must be a data frame.")
    stop("'df' must be a data frame.")
  }
  if (!"preferred_units" %in% colnames(df) && nrow(df) > 0) {
    logger::log_error(
      "The data frame 'df' does not contain a 'preferred_units' column."
    )
    stop("The data frame 'df' does not contain a 'preferred_units' column.")
  }
  if (!is.character(unit_list) && !is.null(unit_list)) {
    logger::log_error("'unit_list' must be a character vector or NULL.")
    stop("'unit_list' must be a character vector or NULL.")
  }

  # Early return for edge cases
  if (is.null(unit_list) || length(unit_list) == 0 || nrow(df) == 0) {
    if (nrow(df) == 0) {
      logger::log_warn(
        "The input data frame ('df') is empty. No filtering performed."
      )
    }
    if (is.null(unit_list) || length(unit_list) == 0) {
      logger::log_warn(
        "The 'unit_list' vector is NULL or empty. No units to filter by."
      )
    }
    logger::log_info("Returning empty list as no filtering can be done.")
    return(list())
  }

  # MAJOR OPTIMISATION: Pre-process preferred_units ONCE
  # Convert NAs to empty strings (vectorised, done once not per-unit)
  preferred_units_clean <- ifelse(
    is.na(df$preferred_units),
    "",
    df$preferred_units
  )

  logger::log_info("Starting to filter data by unit list.")

  # Use vectorised approach with named list pre-allocation
  filtered_data_frames_by_unit <- vector("list", length(unit_list))
  names(filtered_data_frames_by_unit) <- unit_list

  for (i in seq_along(unit_list)) {
    unit_name <- unit_list[i]

    if (logger::log_threshold() <= logger::DEBUG) {
      logger::log_debug(sprintf("Filtering for unit: %s", unit_name))
    }

    # Use pre-cleaned preferred_units (avoid repeated ifelse calls)
    matching_rows <- grepl(unit_name, preferred_units_clean, fixed = TRUE)
    filtered_df <- df[matching_rows, , drop = FALSE]

    if (logger::log_threshold() <= logger::DEBUG) {
      logger::log_debug(sprintf(
        "Found %d applicants for unit: %s",
        nrow(filtered_df), unit_name
      ))
    }

    filtered_data_frames_by_unit[[i]] <- filtered_df
  }

  logger::log_info("Finished filtering data by unit list.")

  if (logger::log_threshold() <= logger::DEBUG) {
    logger::log_debug("Exiting process_eoi_data function.")
  }

  return(filtered_data_frames_by_unit)
}
