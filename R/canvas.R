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
#'
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
  cols <- read_csv(
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

#' Parse Canvas export file
#'
#' @param x Path to Canvas export file
#' @param cols Optional vector of column indices to select
#' @return A data frame containing parsed Canvas data
#' @importFrom readr read_csv
#' @importFrom dplyr select rename all_of
#' @export
parse_canvas <- function(x, cols = NULL) {
  col_names <- read_column_names(x)
  canvas_df <- read_canvas_data(x, col_names)

  keep <- c("Student", "SIS User ID", "SIS Login ID")
  throw <- c(
    "ID", "Section", "Current Score", "Unposted Current Score",
    "Final Score", "Unposted Final Score"
  )

  # Automatically select remaining columns
  selected_cols <- col_names[!col_names %in% c(keep, throw)]

  raw_marks <- canvas_df |>
    dplyr::select(dplyr::all_of(c(keep, selected_cols))) |>
    dplyr::rename(
      SID = `SIS User ID`,
      Unikey = `SIS Login ID`
    )

  return(invisible(raw_marks))
}
