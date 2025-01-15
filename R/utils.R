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
