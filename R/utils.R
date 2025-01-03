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



#' Check if a URL exists
#'
#' Internal function.
#'
#' @param url string - URL to check, e.g. 'https://example.com'
#'
#' @returns TRUE if the URL exists, FALSE otherwise
#' @keywords internal
check_url <- function(url) {
  # Create request
  req <- request(url)

  # Perform the request with error handling
  result <- try({
    # Make the request
    resp <- req |>
      req_perform()

    # Get the status code and body
    status <- resp_status(resp)
    content <- resp_body_string(resp)

    # Check both status code and if the content contains error indicators
    is_valid <- status == 200 &&
      !grepl("Error \\(404\\)", content) &&
      !grepl("Page not found", content)

    return(is_valid)
  }, silent = TRUE)

  # If there was an error in the try block, return FALSE
  if (inherits(result, "try-error")) {
    return(FALSE)
  }

  return(result)
}
