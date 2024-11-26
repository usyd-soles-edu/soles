#' Find Gradescope export File
#'
#' Searches for and validates a Gradescope export file in the specified
#' directory. If multiple files are found, it uses the most
#' recent one (based on filename).
#'
#' @param dir Character string specifying the directory to search. If NULL
#'   (default), the current working directory is used.
#'
#' @return Character string containing the full path to the validated Gradescope
#'   export file.
#'
#' @note If multiple matching files are found, a warning is issued and the most
#'   recent file (by filename) is used.
#'
#' @importFrom readr read_csv
#'
#' @export
find_gradescope_file <- function(dir = NULL) {
  current_dir <- if (is.null(dir)) getwd() else dir
  files <- list.files(
    path = current_dir,
    pattern = ".*_scores\\.(csv|xlsx|xls)$"
  )
  if (length(files) == 0) {
    stop("No Gradescope export files found in directory")
  } else if (length(files) > 1) {
    warning("Multiple Gradescope files found. Using most recent file.")
    files <- sort(files, decreasing = TRUE)[1]
  }
  file_path <- file.path(current_dir, files[1])
  cols <- readr::read_csv(
    file_path,
    n_max = 0,
    show_col_types = FALSE
  ) |>
    names()
  if (!all(c("View Count", "Submission Count") %in% cols)) {
    stop("File does not appear to be a Gradescope export")
  }
  return(file_path)
}

#' Find Canvas export file
#'
#' @description Searches a directory for Canvas grade export files (CSV) and
#' returns the path to the most recent file. The function validates that the
#' file contains expected Canvas columns.
#'
#' @param dir Character string specifying the directory to search. If NULL
#'   (default), the current working directory is used.
#'
#' @importFrom readr read_csv
#' @export
find_canvas_file <- function(dir = NULL) {
  current_dir <- if (is.null(dir)) getwd() else dir
  files <- list.files(path = current_dir, pattern = "*.csv")
  pattern <- "^\\d{4}-\\d{2}-\\d{2}T\\d{4}_Marks"
  canvas_files <- files[grepl(pattern, files)]
  if (length(canvas_files) == 0) {
    stop("No Canvas export files found in directory")
  } else if (length(canvas_files) > 1) {
    message("Multiple Canvas files found. Using most recent file.")
    canvas_files <- sort(canvas_files, decreasing = TRUE)[1]
  }
  file_path <- file.path(current_dir, canvas_files[1])
  cols <- readr::read_csv(
    file_path,
    n_max = 0,
    show_col_types = FALSE
  ) |>
    names()
  required_cols <- c("SIS User ID", "SIS Login ID")
  if (!all(required_cols %in% cols)) {
    stop("File does not appear to be a Canvas export (missing columns)")
  }
  return(file_path)
}


#' Find special considerations/arrangements file
#'
#' The special considerations/arrangements file is a CSV file that contains
#' information about special considerations/arrangements for students. At USYD
#' this can be downloaded from the UOS dashboard.
#'
#' @param dir Character string specifying the directory path to search. If NULL
#'   (default), uses current working directory.
#'
#' @return A character string containing the full file path to the most recent CSV
#'   file that contains all required columns.
#'
#' @details The function looks for CSV files containing the required columns:
#'   'extension_in_calendar_days' and 'u_outcome_type'. It will return the path
#'   to the most recent file that matches these criteria.
#'
#' @import logger
#' @importFrom readr read_csv
#' @importFrom lubridate dmy_hms
#' @importFrom dplyr pull
#'
#' @export
find_spec_cons_file <- function(dir = NULL) {
  if (is.null(dir)) {
    dir <- getwd()
  }
  # get names of all csv files in list
  logger::log_debug("Searching for CSV files in directory")
  files <- list.files(path = dir, pattern = "*.csv")
  logger::log_debug("Files found:")
  for (file in files) {
    logger::log_debug(paste("  ", file))
  }
  # if no csv files found, stop
  if (length(files) == 0) {
    stop("No CSV files found in directory")
  }
  # match files that have columns "extension_in_calendar_days" and "u_outcome_type"
  matching_files <- character(0)
  for (file in files) {
    cols <- suppressWarnings(suppressMessages(
      readr::read_csv(
        file.path(dir, file),
        n_max = 0,
        show_col_types = FALSE
      )
    )) |>
      names()
    required_cols <- c("extension_in_calendar_days", "u_outcome_type")
    if (all(required_cols %in% cols)) {
      matching_files <- c(matching_files, file)
      matching_files
    }
  }
  # print matching files
  logger::log_debug("Matching files:")
  for (file in matching_files) {
    logger::log_debug(paste("  ", file))
  }
  # pick most recent file if multiple files found, use date created
  if (length(matching_files) == 0) {
    stop("No special considerations file found (missing expected columns)")
  }
  if (length(matching_files) > 1) {
    message(
      "Multiple special considerations files found. Using most recent file."
    )
    file_dates <- sapply(matching_files, function(file) {
      readr::read_csv(file.path(dir, file), show_col_types = FALSE) |>
        dplyr::pull("sys_updated_on") |>
        lubridate::dmy_hms() |>
        max()
    })
    most_recent_file <- matching_files[which.max(file_dates)]
    return(file.path(dir, most_recent_file))
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
