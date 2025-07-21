#' Read column names from a CSV file
#'
#' @param x Path to CSV file
#' @return Vector of column names
#' @importFrom readr read_csv
#' @importFrom dplyr select filter mutate rename_with everything bind_cols slice
#' @importFrom stringr str_detect
#' @importFrom readxl read_excel cell_cols
#' @keywords internal
read_column_names <- function(x) {
  readr::read_csv(x, n_max = 0, show_col_types = FALSE) |> names()
}


#' Prompt user to select columns
#'
#' @param choose Vector of column names to choose from
#' @return Vector of selected column names
#' @keywords internal
prompt_user_for_columns <- function(choose) {
  cat("Available columns:\n")
  for (i in seq_along(choose)) {
    cat(sprintf("%d: %s\n", i, choose[i]))
  }

  selected <- readline("Enter column numbers (separated by spaces): ")
  selected_indices <- as.numeric(strsplit(selected, " ")[[1]])
  choose[selected_indices]
}


#' Display selected columns
#'
#' @param selected_cols Vector of selected column names
#' @keywords internal
display_selected_columns <- function(selected_cols) {
  cat("\nSelected columns:\n")
  for (i in seq_along(selected_cols)) {
    cat(sprintf("%d: %s\n", i, selected_cols[i]))
  }
}


#' Match assessment records between master list and marks
#'
#' @param master Data frame with Type, Description, Weight columns
#' @param marks Data frame with assessment and mark columns
#' @return Integer vector matching marks to master list rows
#' @importFrom stringr str_extract
#' @keywords internal
match_assessments <- function(master, marks) {
  # Extract weights from marks assessment descriptions
  weights <- stringr::str_extract(marks$assessment, "\\[(\\d+)%\\]")
  weights <- as.numeric(stringr::str_extract(weights, "\\d+"))

  # Filter out 0% weighted items from marks
  valid_marks <- marks[!is.na(weights) & weights > 0, ]

  # Initialize results vector
  n_master <- nrow(master)
  n_marks <- nrow(valid_marks)
  matches <- integer(n_master)

  # Helper function to clean strings for matching
  clean_string <- function(x) {
    tolower(gsub("[[:punct:]]|\\s+", " ", x))
  }

  # For each master assessment
  for (i in seq_len(n_master)) {
    master_weight <- as.numeric(stringr::str_extract(master$Weight[i], "\\d+"))
    if (is.na(master_weight) || master_weight == 0) next

    master_desc <- clean_string(master$Description[i])
    best_match <- NA
    best_score <- 0

    # Look for matching mark entry
    for (j in seq_len(n_marks)) {
      mark_desc <- clean_string(valid_marks$assessment[j])
      mark_weight <- weights[j]

      # Skip if weights don't match
      if (is.na(mark_weight) || mark_weight != master_weight) next

      # Calculate match score based on shared words
      master_words <- strsplit(master_desc, " ")[[1]]
      mark_words <- strsplit(mark_desc, " ")[[1]]
      shared_words <- sum(master_words %in% mark_words)

      if (shared_words > best_score) {
        best_score <- shared_words
        best_match <- j
      }
    }

    if (!is.na(best_match)) {
      matches[i] <- best_match
    }
  }

  matches
}


#' Set logging level
#'
#' @param verbose If `TRUE`, set log level to INFO, otherwise set to SUCCESS.
#' @return Nothing
#' @importFrom logger log_threshold INFO SUCCESS
#' @export
set_log_level <- function(verbose = FALSE) {
  if (verbose) {
    logger::log_threshold(logger::INFO)
  } else {
    logger::log_threshold(logger::SUCCESS)
  }
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

#' Compare two data frames for equality
#'
#' This internal helper function prepares two data frames for comparison by:
#' 1. Converting all columns to character type.
#' 2. Sorting columns by name.
#' 3. Arranging rows based on all columns.
#' It then checks for identity using `identical()`.
#'
#' @param df1 The first data frame.
#' @param df2 The second data frame.
#' @return `TRUE` if the data frames are identical after preparation, `FALSE` otherwise.
#' @importFrom dplyr mutate across everything arrange select
#' @keywords internal
are_dfs_equal <- function(df1, df2) {
  # Prepare df1
  df1_prep <- df1 |> dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  df1_prep <- df1_prep[, sort(names(df1_prep)), drop = FALSE]
  df1_prep <- df1_prep |> dplyr::arrange(dplyr::across(dplyr::everything()))

  # Prepare df2
  df2_prep <- df2 |> dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  df2_prep <- df2_prep[, sort(names(df2_prep)), drop = FALSE]
  df2_prep <- df2_prep |> dplyr::arrange(dplyr::across(dplyr::everything()))

  # Compare
  return(identical(df1_prep, df2_prep))
}
