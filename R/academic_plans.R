#' Find Matching Students in Academic Plans
#'
#' Searches for and returns matching student entries from academic plans data.
#'
#' @import dplyr
#' @import stringr
#'
#' @param ap Dataframe from ap_read().
#' @param student Character string. Search term to match with student
#'   identifiers.
#' @param uos Character string. Optional UoS code to filter by.
#' @param year Integer. Optional year to filter by.
#'
#' @return A dataframe of matching student entries, grouped by SID, UoS Code,
#'   and Year, with an added Entry number column.
#'
#' @export
find_student <- function(ap, student, uos = NULL, year = NULL) {
  # Input validation
  if (!is.data.frame(ap)) {
    stop("Input 'ap' must be a dataframe")
  }
  if (!is.character(student) || length(student) != 1) {
    stop("Student must be a single string")
  }

  # Convert search term to lowercase
  student <- tolower(student)

  # Find matching students with combined filter
  matches <- ap |>
    filter(
      str_detect(tolower(`Preferred Name`), student) |
        str_detect(tolower(`Family Name`), student) |
        str_detect(SID, student) |
        str_detect(tolower(Unikey), student),
      # Additional filters - only apply if not NULL
      if (!is.null(uos)) `UoS Code` == uos else TRUE,
      if (!is.null(year)) Year == year else TRUE
    )

  # Check if any matches found
  if (nrow(matches) == 0) {
    stop("No students found matching the search term")
  }

  # Add entry numbers
  matches |>
    group_by(SID, `UoS Code`, Year) |>
    mutate(Entry = row_number()) |>
    ungroup()
}

#' Extract and filter assessment information
#'
#' @description
#' Parses assessment information from academic plan data. Without arguments,
#' returns a list of all unique assessments. With a pattern argument, returns
#' matching assessment data.
#'
#' @param pattern Optional string to match against assessment names
#' @param data Optional data frame from ap_read(). If not provided, uses the most
#'   recent ap_read() result
#'
#' @return Without pattern: a character vector of unique assessment names
#'         With pattern: a filtered data frame of matching assessment entries
#'
#' @examples
#' # List all assessments
#' assessment()
#'
#' # Get data for a specific assessment
#' assessment("BIOL2022 Project 1 2024")
#'
#' @export
assessment <- function(data = NULL, pattern = NULL) {
  # Get assessment data
  if (is.null(data)) {
    stop("Please provide data from ap_read()")
  }

  # Create assessment strings
  assessments <- paste(
    data$`UoS Code`,
    data$Year,
    data$Assessment
  )

  # If no pattern, return unique assessment list
  if (is.null(pattern)) {
    return(unique(assessments))
  }

  # Find closest match using string distance
  matches <- agrep(
    pattern = pattern,
    x = assessments,
    max.distance = 0.2,
    value = TRUE
  )

  if (length(matches) == 0) {
    stop("No matching assessments found")
  }

  # Get best match
  best_match <- matches[1]

  # Filter data for matching assessment
  data |>
    filter(
      paste(`UoS Code`, Year, Assessment) == best_match
    )
}
