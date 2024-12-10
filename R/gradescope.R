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
#' @importFrom lubridate parse_date_time dmy_hms
#' @importFrom dplyr select mutate across pull
#' @importFrom tidyr drop_na
#'
#' @export
find_gradescope_file <- function(dir = NULL) {
  if (is.null(dir)) dir <- getwd()

  # If dir is a file path, validate and return it directly
  if (file.exists(dir) && !dir.exists(dir)) {
    file_path <- dir
    # Validate file has required columns
    required_cols <- c("Submission ID", "Submission Time")
    cols <- suppressMessages(
      read_csv(file_path, n_max = 0, show_col_types = FALSE) |> names()
    )
    if (!all(required_cols %in% cols)) {
      stop("File does not appear to be a Gradescope export (missing required columns)")
    }
    return(file_path)
  }

  # Otherwise treat as directory and find latest file
  files <- list.files(path = dir, pattern = "\\.csv$|\\.xlsx$|\\.xls$")
  if (length(files) == 0) {
    stop("No files found in directory")
  }

  # Check which files have required columns
  required_cols <- c("Submission ID", "Submission Time")
  matching_files <- character(0)
  for (file in files) {
    cols <- suppressMessages(
      read_csv(
        file.path(dir, file),
        n_max = 0,
        show_col_types = FALSE
      ) |> names()
    )
    if (all(required_cols %in% cols)) {
      matching_files <- c(matching_files, file)
    }
  }

  if (length(matching_files) == 0) {
    stop("No Gradescope file found (missing expected columns)")
  }

  # Get most recent file
  file_times <- sapply(matching_files, function(file) {
    suppressWarnings(
      read_csv(file.path(dir, file), show_col_types = FALSE) |>
        select("Submission Time") |>
        mutate(across(
          "Submission Time",
          ~ lubridate::parse_date_time(.,
            "YmdHMSz",
            tz = "Australia/Sydney",
            quiet = TRUE
          )
        )) |>
        drop_na() |>
        pull() |>
        max()
    )
  })

  return(file.path(dir, matching_files[which.max(file_times)]))
}

#' Parse Gradescope export file
#'
#' @param x Path to Gradescope export file
#' @return A data frame containing parsed Gradescope data
#' @importFrom readr read_csv
#' @importFrom dplyr select
#' @export
parse_gradescope <- function(x) {
  df <- suppressWarnings(readr::read_csv(x, show_col_types = FALSE))
  out <- df |>
    dplyr::select("First Name":"Status")
  return(invisible(out))
}
