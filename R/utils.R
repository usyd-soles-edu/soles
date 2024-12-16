#' Read column names from a CSV file
#'
#' @param x Path to CSV file
#' @return Vector of column names
#' @importFrom readr read_csv
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
  # Count files of each type before selecting most recent
  files <- list.files(path = directory, pattern = "*.csv")

  # Canvas files
  pattern <- "^\\d{4}-\\d{2}-\\d{2}T\\d{4}_Marks"
  canvas_count <- sum(grepl(pattern, files))

  # Gradescope files
  gradescope_count <- 0
  for (file in files) {
    cols <- suppressMessages(
      read_csv(
        file.path(directory, file),
        n_max = 0,
        show_col_types = FALSE
      ) |> names()
    )
    if (all(c("Submission ID", "Submission Time") %in% cols)) {
      gradescope_count <- gradescope_count + 1
    }
  }

  # Special considerations files
  spec_cons_count <- 0
  for (file in files) {
    cols <- suppressMessages(
      read_csv(
        file.path(directory, file),
        n_max = 0,
        show_col_types = FALSE
      ) |> names()
    )
    if (all(c("extension_in_calendar_days", "u_outcome_type") %in% cols)) {
      spec_cons_count <- spec_cons_count + 1
    }
  }

  # Get file paths
  gradescope <- find_gradescope_file(directory)
  canvas <- find_canvas_file(directory)
  arrangements <- find_spec_cons_file(directory)

  # Display summary
  cat("Gradescope:", basename(gradescope))
  if (gradescope_count > 1) cat(" (most recent from", gradescope_count, "files)")
  cat("\n")

  cat("Canvas:", basename(canvas))
  if (canvas_count > 1) cat(" (most recent from", canvas_count, "files)")
  cat("\n")

  cat("Special arrangements:", basename(arrangements))
  if (spec_cons_count > 1) cat(" (most recent from", spec_cons_count, "files)")
  cat("\n")

  return(invisible(
    list(
      gradescope = gradescope,
      canvas = canvas,
      arrangements = arrangements
    )
  ))
}
