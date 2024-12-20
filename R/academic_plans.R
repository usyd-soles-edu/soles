ERRORS <- list(
  no_matches = "No students found matching the search term",
  invalid_entry = "Entry number out of range",
  no_adjustments = "No adjustments found for this plan",
  invalid_excel = "Invalid Excel file structure",
  invalid_plan = "Invalid academic plan data",
  missing_columns = "Required columns missing from data"
)

#' Read and Parse Academic Plans
#'
#' Reads an Excel file containing academic plans and parses it into a structured
#' dataframe. The file should have metadata in the first two rows followed by
#' the actual data.
#'
#' @importFrom readxl read_excel
#'
#' @param path Character string. Path to the Excel file containing academic plans.
#'
#' @return A dataframe containing:
#'   \item{Year}{Integer year of the plan}
#'   \item{UoS Code}{Unit of Study code}
#'   \item{Assessment}{Assessment name}
#'   \item{Preferred Name}{Student's preferred name}
#'   \item{Family Name}{Student's family name}
#'   \item{SID}{Student ID number}
#'   \item{Unikey}{Student's Unikey}
#'   Additional columns for specific adjustments may be present.
#'
#' @examples
#' # Read academic plans from Excel file
#' plans <- ap_read("academic_plans.xlsx")
#'
#' @seealso \code{\link{ap_match}} for finding students in the plans
#'
#' @export
ap_read <- function(path) {
  # Validate input
  if (!file.exists(path)) {
    stop("File not found:", path)
  }

  tryCatch(
    {
      # Read first two rows for column names
      raw_names <- read_excel(path, .name_repair = "unique_quiet", n_max = 2)

      # Extract and clean column names
      col_names <- clean_column_names(raw_names)

      # Read and process data
      read_excel(path, skip = 3, col_names = col_names) |>
        slice(-n()) |> # Remove last row
        mutate(Year = as.integer(Year)) |>
        select(-`Assessment ID (ignore)`)
    },
    error = function(e) {
      stop("Error reading Excel file: ", e$message)
    }
  )
}

#' Find Matching Students in Academic Plans
#'
#' Searches for and returns matching student entries from academic plans data.
#'
#' @import dplyr
#' @importFrom stringr str_detect
#'
#' @param ap Dataframe from ap_read().
#' @param student Character string. Search term to match with student identifiers.
#' @param uos Character string. Optional UoS code to filter by.
#' @param year Integer. Optional year to filter by.
#'
#' @return A dataframe of matching student entries, grouped by SID, UoS Code, and Year,
#'   with an added Entry number column.
#'
#' @export
ap_match <- function(ap, student, uos = NULL, year = NULL) {
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
      # Additional filters
      case_when(
        !is.null(uos) ~ `UoS Code` == uos,
        TRUE ~ TRUE
      ),
      case_when(
        !is.null(year) ~ Year == year,
        TRUE ~ TRUE
      )
    )

  # Check if any matches found
  if (nrow(matches) == 0) {
    stop(ERRORS$no_matches)
  }

  # Add entry numbers
  matches |>
    group_by(SID, `UoS Code`, Year) |>
    mutate(Entry = row_number()) |>
    ungroup()
}



#' Clean Excel Column Names
#'
#' Internal function to process and clean column names from the Excel file.
#'
#' @import dplyr
#' @importFrom stringr str_remove_all str_remove
#'
#' @param raw_names Dataframe. Raw names from Excel file.
#'
#' @return A vector of column names.
#'
clean_column_names <- function(raw_names) {
  # Input validation
  if (!is.data.frame(raw_names) || ncol(raw_names) < 15) {
    stop("Invalid Excel structure")
  }

  tryCatch(
    {
      # Extract category names
      category <- names(raw_names) |>
        str_remove_all("\\...\\d+$")

      # Extract metadata columns
      meta <- raw_names |>
        slice(2) |>
        select(1:15) |>
        unlist() |>
        unname()

      # Extract arrangement columns
      arrangements <- raw_names |>
        slice(1) |>
        select(-(1:15)) |>
        unlist() |>
        unname()

      # Combine and clean
      paste(category, c(meta, arrangements), sep = " - ") |>
        str_remove("^ - ") |>
        str_remove("^Category - ")
    },
    error = function(e) {
      stop("Error processing column names: ", e$message)
    }
  )
}
