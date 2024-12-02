#' Find special considerations/arrangements file
#'
#' @param dir Character string specifying the directory path to search. If NULL
#'   (default), uses current working directory.
#' @return Character string with full file path to most recent arrangements file
#'
#' @importFrom readr read_csv
#' @importFrom lubridate dmy_hms
#' @importFrom dplyr pull select across mutate
#'
#' @export
find_spec_cons_file <- function(dir = NULL) {
  current_dir <- if (is.null(dir)) getwd() else dir

  # Find all CSV files
  files <- list.files(path = current_dir, pattern = "*.csv")
  if (length(files) == 0) {
    stop("No CSV files found in directory")
  }

  # Check which files have required columns
  required_cols <- c("extension_in_calendar_days", "u_outcome_type")
  matching_files <- character(0)

  for (file in files) {
    cols <- suppressMessages(
      readr::read_csv(
        file.path(current_dir, file),
        n_max = 0,
        show_col_types = FALSE
      ) |> names()
    )
    if (all(required_cols %in% cols)) {
      matching_files <- c(matching_files, file)
    }
  }

  if (length(matching_files) == 0) {
    stop("No special considerations file found (missing expected columns)")
  }

  # Get most recent file if multiple found
  if (length(matching_files) > 1) {
    message("Multiple special considerations files found. Using most recent file.")
    file_dates <- sapply(matching_files, function(file) {
      read_csv(
        file.path(current_dir, file),
        show_col_types = FALSE
      ) |>
        pull("sys_updated_on") |>
        dmy_hms() |>
        max()
    })
    matching_files <- matching_files[which.max(file_dates)]
  }

  return(file.path(current_dir, matching_files[1]))
}

#' Parse special considerations file
#'
#' @param x Path to special considerations file
#' @param uos Unit of study code
#' @param year Year of study
#' @return A data frame containing parsed special considerations data
#' @importFrom readr read_csv
#' @importFrom dplyr filter
#' @export
parse_spec_cons <- function(x, uos = NULL, year = NULL) {
  validate_inputs(uos, year)
  df <- read_spec_cons_data(x, uos, year)
  selected_outcome <- select_outcome_type(df)
  out <- filter_spec_cons(df, selected_outcome)
  display_summary(out)
  return(invisible(out))
}

#' Validate special considerations inputs
#' @keywords internal
validate_inputs <- function(uos, year) {
  if (is.null(uos)) stop("UOS parameter must be provided")
  if (is.null(year)) stop("Year parameter must be provided")
}

#' Read special considerations data
#' @keywords internal
read_spec_cons_data <- function(x, uos, year) {
  df <- suppressWarnings(readr::read_csv(x, show_col_types = FALSE))
  pattern <- paste0(uos, ".*", year)
  dplyr::filter(df, grepl(pattern, availability))
}

#' Select outcome type for special considerations
#' @keywords internal
select_outcome_type <- function(df) {
  unique_outcomes <- unique(df$u_outcome_type)
  replacement_outcomes <- unique_outcomes[grepl("Replacement exam", unique_outcomes, ignore.case = TRUE)]
  if (length(replacement_outcomes) == 0) {
    stop("No replacement exam outcomes found")
  }
  replacement_outcomes
}

#' Filter special considerations data
#' @keywords internal
filter_spec_cons <- function(df, outcomes) {
  dplyr::filter(
    df,
    state == "Approved" & u_outcome_type %in% outcomes |
      (state == "Pending" & is.na(u_outcome_type))
  )
}

#' Display summary of special considerations
#' @keywords internal
display_summary <- function(df) {
  n_approved <- sum(df$state == "Approved")
  n_pending <- sum(df$state == "Pending")
  cat(sprintf("\nNumber of students with approved replacement exams: %d", n_approved))
  cat(sprintf("\nNumber of students with pending special considerations: %d", n_pending))
  cat(sprintf("\nTotal: %d\n", nrow(df)))
}
