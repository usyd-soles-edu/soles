find_gradescope_file <- function(dir = NULL) {
  # Use current directory if dir is NULL, otherwise use specified dir
  current_dir <- if (is.null(dir)) getwd() else dir

  # List all files in directory with .csv, .xlsx, or .xls extension
  files <- list.files(path = current_dir, pattern = ".*_scores\\.(csv|xlsx|xls)$")

  if (length(files) == 0) {
    stop("No Gradescope export files found in directory")
  } else if (length(files) > 1) {
    warning("Multiple Gradescope files found. Using most recent file.")
    # Sort by filename and take the last one
    files <- sort(files, decreasing = TRUE)[1]
  }

  # Read column names to verify it's a Gradescope file
  file_path <- file.path(current_dir, files[1])
  cols <- readr::read_csv(file_path, n_max = 0, show_col_types = FALSE) |>
    names()
  if (!all(c("View Count", "Submission Count") %in% cols)) {
    stop("File does not appear to be a Gradescope export (missing expected
  columns)")
  }

  return(file_path)
}


find_canvas_file <- function(dir = NULL) {
  # Use current directory if dir is NULL, otherwise use specified dir
  current_dir <- if (is.null(dir)) getwd() else dir

  # List all files in specified directory
  files <- list.files(path = current_dir, pattern = "*.csv")

  # Pattern for Canvas export files
  pattern <- "^\\d{4}-\\d{2}-\\d{2}T\\d{4}_Marks"

  # Find matching files
  canvas_files <- files[grepl(pattern, files)]

  if (length(canvas_files) == 0) {
    stop("No Canvas export files found in directory")
  } else if (length(canvas_files) > 1) {
    warning("Multiple Canvas files found. Using most recent file.")
    # Sort by filename (which includes date) and take the last one
    canvas_files <- sort(canvas_files, decreasing = TRUE)[1]
  }

  # Read column names to verify it's a Canvas file
  file_path <- file.path(current_dir, canvas_files[1])
  cols <- readr::read_csv(
    file_path,
    n_max = 0,
    show_col_types = FALSE
  ) |>
    names()
  required_cols <- c("SIS User ID", "SIS Login ID")
  if (!all(required_cols %in% cols)) {
    stop("File does not appear to be a Canvas export (missing expected
  columns)")
  }

  return(file_path)
}


find_arrangements_file <- function(dir = NULL) {
  # Use current directory if dir is NULL, otherwise use specified dir
  current_dir <- if (is.null(dir)) getwd() else dir

  # List all files in directory
  files <- list.files(path = current_dir, pattern = "*.csv")

  if (length(files) == 0) {
    stop("No CSV files found in directory")
  }

  # Read column names of each file to find special considerations file
  for (file in files) {
    cols <- suppressWarnings(suppressMessages(
      readr::read_csv(
        file.path(current_dir, file),
        n_max = 0,
        show_col_types = FALSE
      )
    )) |>
      names()
    required_cols <- c("extension_in_calendar_days", "u_outcome_type")
    if (all(required_cols %in% cols)) {
      return(file.path(current_dir, file))
    }
  }

  stop("No special considerations file found (missing expected columns)")
}


#' Find Required Files for Exam Processing
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
#'
#' @examples
#' files <- find_docs("path/to/directory")
find_docs <- function(directory) {
  gradescope <- find_gradescope_file(directory)
  canvas <- find_canvas_file(directory)
  arrangements <- find_arrangements_file(directory)

  cat("Found required files:\n")
  cat("  - Gradescope:", gradescope, "\n")
  cat("  - Canvas:", canvas, "\n")
  cat("  - Arrangements:", arrangements, "\n")

  return(list(
    gradescope = gradescope,
    canvas = canvas,
    arrangements = arrangements
  ))
}
