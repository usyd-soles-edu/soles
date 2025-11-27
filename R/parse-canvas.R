#' Parse Canvas export file
#'
#' @description
#' Reads a Canvas export file and returns a list containg student data
#' and unit information. The unit information is useful for filtering
#' other files (e.g. academic plans) to match the Canvas unit.
#'
#' @param x Path to Canvas file
#' @return A list
#' @importFrom readr read_csv
#' @importFrom dplyr select rename all_of
#' @importFrom logger log_debug log_error
#' @export
parse_canvas <- function(x) {
  log_debug("Starting to parse Canvas file: {x}")
  col_names <- read_column_names(x)
  log_debug("Retrieved {length(col_names)} column names")

  canvas <- suppressWarnings(
    read_csv(x,
      skip = 3,
      col_names = col_names,
      na = "N/A",
      show_col_types = FALSE
    )
  ) |>
    select(all_of(c(
      "Student", "SIS User ID", "SIS Login ID",
      setdiff(col_names, c("Student", "SIS User ID", "SIS Login ID", "ID"))
    ))) |>
    rename(
      SID = `SIS User ID`,
      Unikey = `SIS Login ID`
    )
  log_debug("Successfully loaded and transformed Canvas data with {nrow(canvas)} rows")

  # Extract unit details from first section - now with error handling
  section <- canvas$Section[1]
  log_debug("Extracting unit details...")
  uos_code <- str_extract(section, "(?<=\\(activity\\)\\s).*?(?=/)")

  # Return NULL for uos_details if extraction fails
  uos_details <- tryCatch(
    {
      if (is.na(uos_code)) {
        log_debug("No UOS code found in section")
        return(list(canvas = canvas, uos_details = NULL))
      }
      parts <- strsplit(uos_code, "-")[[1]]
      if (length(parts) != 5) {
        log_debug("Invalid UOS code format: expected 5 parts, got {length(parts)}")
        return(list(canvas = canvas, uos_details = NULL))
      }
      details <- setNames(parts, c("year", "unit", "semester", "type", "delivery"))
      log_debug("Successfully extracted UOS details: {paste(names(details), details, sep='=', collapse=', ')}")
      details
    },
    error = function(e) {
      log_error("Error parsing UOS details: {conditionMessage(e)}")
      NULL
    }
  )

  out <-
    list(
      canvas = canvas,
      uos_details = uos_details
    )
  return(out)
}
