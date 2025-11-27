#' Parse SRES CSV files into a tibble
#'
#' @param file Path to the SRES CSV file
#' @return A tibble containing parsed SRES data
#' @export
parse_sres <- function(file) {
  # Required columns that identify an SRES file
  # required_cols <- c(
  #   "Preferred name", "Given names", "Surname", "SID",
  #   "Email", "Username"
  # )

  # Read the CSV file
  data <- readr::read_csv(file, show_col_types = FALSE)

  # Check if it's a valid SRES file by verifying required columns exist
  # if (!all(required_cols %in% colnames(data))) {
  #   stop("File does not appear to be a valid SRES export - missing required columns")
  # }

  # Convert to tibble if not already
  data <- tibble::as_tibble(data)

  # Basic data cleaning
  data <- data |>
    dplyr::mutate(
      # Ensure status is lowercase
      status = tolower(status),
      # Remove any trailing/leading whitespace
      across(where(is.character), trimws)
    )

  return(data)
}
