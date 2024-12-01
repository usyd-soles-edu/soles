#' Read column names from a CSV file
#'
#' @param x Path to CSV file
#' @return Vector of column names
#' @importFrom readr read_csv
#' @keywords internal
read_column_names <- function(x) {
  readr::read_csv(x, n_max = 0, show_col_types = FALSE) |> names()
}

#' Read Canvas data from CSV file
#'
#' @param x Path to CSV file
#' @param col_names Vector of column names
#' @return Data frame of Canvas data
#' @importFrom readr read_csv
#' @keywords internal
read_canvas_data <- function(x, col_names) {
  suppressWarnings(
    readr::read_csv(x,
      skip = 3,
      col_names = col_names,
      na = c("N/A"),
      show_col_types = FALSE
    )
  )
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

#' Find all documents
#'
#' Searches the specified directory for required files related to exam
#' processing: Gradescope, Canvas, and Arrangements files.
#'
#' @param directory Character string specifying the path to search for files
#'
#' @return A list containing three elements: \item{gradescope}{Path to the
#'   Gradescope file} \item{canvas}{Path to the Canvas file}
#'   \item{arrangements}{Path to the Arrangements file}
#'
#' @export
find_docs <- function(directory) {
  gradescope <- find_gradescope_file(directory)
  canvas <- find_canvas_file(directory)
  arrangements <- find_spec_cons_file(directory)
  cat("Found required files:\n")
  cat("  - Gradescope:", gradescope, "\n")
  cat("  - Canvas:", canvas, "\n")
  cat("  - Special arrangements:", arrangements, "\n")
  return(invisible(
    list(
      gradescope = gradescope,
      canvas = canvas,
      arrangements = arrangements
    )
  ))
}
