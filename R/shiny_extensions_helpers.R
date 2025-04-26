library(stringr)

#' Extract UoS Codes from Availability Column
#'
#' Parses the availability string (e.g., "CODE-SEM-YEAR") to extract the UoS code.
#' Handles potential variations in the column name ("UoS (availability)" or "availability").
#'
#' @param df Data frame containing the availability column.
#' @return A sorted character vector of unique UoS codes.
#' @noRd
extract_uos_codes <- function(df) {
  availability_col <- if ("UoS (availability)" %in% names(df)) {
    "UoS (availability)"
  } else {
    "availability"
  }

  codes <- vapply(df[[availability_col]], function(x) {
    parts <- unlist(str_split(x, "-"))
    if (length(parts) >= 1) parts[1] else NA_character_
  }, character(1))
  sort(unique(codes[!is.na(codes)]))
}

#' Extract Semesters from Availability Column
#'
#' Parses the availability string (e.g., "CODE-SEM-YEAR") to extract the semester.
#' Handles potential variations in the column name ("UoS (availability)" or "availability").
#'
#' @param df Data frame containing the availability column.
#' @param selected_uos Optional UoS code to filter semesters for. If NULL, extracts all semesters.
#' @return A sorted character vector of unique semesters.
#' @noRd
extract_semesters <- function(df, selected_uos = NULL) {
  availability_col <- if ("UoS (availability)" %in% names(df)) {
    "UoS (availability)"
  } else {
    "availability"
  }

  semesters <- vapply(df[[availability_col]], function(x) {
    parts <- unlist(str_split(x, "-"))
    if (length(parts) >= 2) {
      if (is.null(selected_uos) || (length(parts) >= 1 && parts[1] == selected_uos)) {
        parts[2]
      } else {
        NA_character_
      }
    } else {
      NA_character_
    }
  }, character(1))
  sort(unique(semesters[!is.na(semesters)]))
}


#' Extract Years from Availability Column
#'
#' Parses the availability string (e.g., "CODE-SEM-YEAR") to extract the year.
#' Handles potential variations in the column name ("UoS (availability)" or "availability").
#'
#' @param df Data frame containing the availability column.
#' @param selected_uos UoS code to filter years for.
#' @param selected_semester Semester to filter years for.
#' @return A sorted character vector of unique years.
#' @noRd
extract_years <- function(df, selected_uos, selected_semester) {
  availability_col <- if ("UoS (availability)" %in% names(df)) {
    "UoS (availability)"
  } else {
    "availability"
  }

  years <- vapply(df[[availability_col]], function(x) {
    parts <- unlist(str_split(x, "-"))
    if (length(parts) >= 3 &&
      parts[1] == selected_uos &&
      parts[2] == selected_semester) {
      parts[3]
    } else {
      NA_character_
    }
  }, character(1))
  sort(unique(years[!is.na(years)]))
}

#' Extract Assessments from Assessment Column
#'
#' Extracts unique assessment names from the 'assessment' column.
#'
#' @param df Data frame potentially containing the assessment column.
#' @return A sorted character vector of unique assessment names, or an empty vector if the column doesn't exist.
#' @noRd
extract_assessments <- function(df) {
  if (!"assessment" %in% names(df)) {
    return(character(0))
  }
  sort(unique(df$assessment[!is.na(df$assessment)]))
}
