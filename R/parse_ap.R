#' Parse Disability Academic Plans file
#'
#' Convert a complex Excel-formatted Disability Academic Plans file into a clean
#' data frame. The input file contains two header rows that need special
#' processing to create proper column names. This function handles that
#' complexity and returns a structured tibble.
#'
#' @param path path to file
#'
#' @returns a tible
#' @export
parse_ap <- function(path) {
  # Validate input
  if (!file.exists(path)) {
    stop("File not found:", path)
  }
  # Read first two rows for column names
  raw_names <- read_excel(path, .name_repair = "unique_quiet", n_max = 2)

  # Extract and clean column names
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
  col_names <- paste(category, c(meta, arrangements), sep = " - ") |>
    str_remove("^ - ") |>
    str_remove("^Category - ")

  # Read and process data
  out <-
    read_excel(path, skip = 3, col_names = col_names) |>
    slice(-n()) |> # Remove last row
    mutate(Year = as.integer(Year)) |>
    select(-`Assessment ID (ignore)`)

  return(out)
}

#' Filter Academic Plan Data
#'
#' Filters the data frame produced by \code{\link{parse_ap}} based on specified
#' criteria such as year, session, and unit of study code.
#'
#' @param ap_data A data frame, typically the output of \code{\link{parse_ap}}.
#' @param year Optional numeric value. Filters for rows matching this 'Year'.
#' @param session Optional character string. Filters for rows matching this 'Session'.
#' @param uos_code Optional character string. Filters for rows matching this 'UoS Code'.
#'
#' @return A data frame filtered according to the provided criteria.
#'
#' @importFrom dplyr filter
#' @importFrom logger log_info log_debug
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'parsed_ap_data' is the output from parse_ap()
#' # Filter for year 2024
#' filtered_data_2024 & lt
#' -filter_ap(parsed_ap_data, year = 2024)
#'
#' # Filter for year 2024 and session "S1C"
#' filtered_data_2024_s1c & lt
#' -filter_ap(parsed_ap_data, year = 2024, session = "S1C")
#'
#' # Filter for UoS Code "ENVX2001"
#' filtered_data_envx & lt
#' -filter_ap(parsed_ap_data, uos_code = "ENVX2001")
#' }
filter_ap <- function(ap_data, year = NULL, session = NULL, uos_code = NULL) {
  logger::log_info("Entering filter_ap function.")
  logger::log_debug("Initial row count: {nrow(ap_data)}")

  filtered_data <- ap_data
  filters_applied <- character(0) # To store which filters were used

  # Apply year filter if provided
  if (!is.null(year)) {
    logger::log_debug("Applying filter: Year == {year}")
    filtered_data <- dplyr::filter(filtered_data, .data$Year == year)
    filters_applied <- c(filters_applied, paste("Year ==", year))
  }

  # Apply session filter if provided
  if (!is.null(session)) {
    logger::log_debug("Applying filter: Session == '{session}'")
    filtered_data <- dplyr::filter(filtered_data, .data$Session == session)
    filters_applied <- c(filters_applied, paste("Session ==", shQuote(session, type = "csh")))
  }

  # Apply UoS code filter if provided
  if (!is.null(uos_code)) {
    logger::log_debug("Applying filter: `UoS Code` == '{uos_code}'")
    filtered_data <- dplyr::filter(filtered_data, .data$`UoS Code` == uos_code)
    filters_applied <- c(filters_applied, paste("`UoS Code` ==", shQuote(uos_code, type = "csh")))
  }

  if (length(filters_applied) > 0) {
    logger::log_info("Filters applied: {paste(filters_applied, collapse = ' & ')}")
  } else {
    logger::log_info("No filters applied.")
  }

  logger::log_info("Exiting filter_ap function.")
  logger::log_debug("Final row count: {nrow(filtered_data)}")

  return(filtered_data)
}
