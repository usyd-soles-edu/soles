#' Detect Canvas file format
#'
#' @param cols Character vector of column names
#' @return Logical indicating if columns match Canvas pattern
#' @keywords internal
detect_canvas <- function(cols) {
  required_cols <- c("SIS User ID", "SIS Login ID", "Section")
  all(required_cols %in% cols)
}

#' Detect Gradescope file format
#'
#' @param cols Character vector of column names
#' @return Logical indicating if columns match Gradescope pattern
#' @keywords internal
detect_gradescope <- function(cols) {
  required_cols <- c("First Name", "Last Name", "SID", "Email", "Sections")
  all(required_cols %in% cols)
}

#' Detect Special Considerations file format
#'
#' @param cols Character vector of column names
#' @return Logical indicating if columns match Special Considerations pattern
#' @keywords internal
detect_spec_cons <- function(cols) {
  # clean up column names
  cols <- tolower(cols)
  required_cols <- c("number", "state", "classification")
  all(required_cols %in% cols)
}

#' Detect Academic Plans file format
#'
#' @param cols Character vector of column names
#' @return Logical indicating if columns match Academic Plans pattern
#' @keywords internal
detect_academic_plans <- function(cols) {
  patterns <- c(
    ".*Category.*",
    ".*Assessment Adjustment.*",
    ".*Exam Adjustment.*"
  )
  all(sapply(patterns, function(pattern) {
    any(grepl(pattern, cols, ignore.case = TRUE))
  }))
}

#' Detect SRES file format
#'
#' @param cols Character vector of column names
#' @return Logical indicating if columns match SRES pattern
#' @keywords internal
detect_sres <- function(cols) {
  required_cols <- c(
    "Preferred name", "Given names", "Surname", "SID",
    "Email", "Username"
  )
  all(required_cols %in% cols)
}
