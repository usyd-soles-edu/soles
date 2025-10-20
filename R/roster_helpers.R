#' Assign Rate Codes Based on Role and Qualifications
#'
#' This function assigns standardized rate codes based on staff role and PhD status.
#'
#' @param data A data frame with role, phd, and rate columns.
#' @return The input data frame with an additional rate_code column.
#' @importFrom dplyr mutate case_when
#' @export
assign_rate_codes <- function(data) {
  data |>
    mutate(
      rate_code = case_when(
        phd == 1 & rate == "tutorial" ~ "TU1",
        phd == 1 & rate == "r_tutorial" ~ "TU3",
        is.na(phd) & rate == "tutorial" ~ "TU2",
        is.na(phd) & rate == "r_tutorial" ~ "TU4",
        phd == 1 & rate == "demo" ~ "DE1",
        is.na(phd) & rate == "demo" ~ "DE2",
        TRUE ~ NA_character_
      )
    )
}

#' Validate Roster Format
#'
#' This function validates that the parsed roster data conforms to the expected format.
#'
#' @param data A data frame with roster data.
#' @return TRUE if valid, otherwise stops with an error message.
#' @export
validate_roster_format <- function(data) {
  required_cols <- c("week", "date", "lab", "day_of_week", "start_time", "role", "name", "rate_code")
  
  if (!all(required_cols %in% names(data))) {
    missing_cols <- setdiff(required_cols, names(data))
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Check data types
  if (!inherits(data$date, "Date")) {
    stop("Column 'date' must be of class Date")
  }
  
  # Check that rate codes are valid
  valid_rate_codes <- c("TU1", "TU2", "TU3", "TU4", "DE1", "DE2")
  invalid_codes <- setdiff(unique(data$rate_code[!is.na(data$rate_code)]), valid_rate_codes)
  if (length(invalid_codes) > 0) {
    stop("Invalid rate codes found: ", paste(invalid_codes, collapse = ", "))
  }
  
  return(TRUE)
}

#' Calculate Weekly Hours from Roster Data
#'
#' This function calculates total weekly hours for each staff member based on roster data.
#'
#' @param data A data frame with roster data.
#' @return A data frame with weekly hours for each staff member.
#' @importFrom dplyr group_by summarise
#' @export
calculate_weekly_hours <- function(data) {
  # Assuming each session is 3 hours as in the existing process_paycodes function
  data |>
    group_by(name, week) |>
    summarise(
      total_hours = n() * 3,
      .groups = "drop"
    )
}

#' Count Sessions by Role and Staff Member
#'
#' This function counts the number of sessions for each staff member by role.
#'
#' @param data A data frame with roster data.
#' @return A data frame with session counts for each staff member by role.
#' @importFrom dplyr group_by count
#' @export
count_sessions_by_role <- function(data) {
  data |>
    group_by(name, role) |>
    count(name = "session_count")
}