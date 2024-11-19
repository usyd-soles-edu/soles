parse_canvas <- function(x, cols = NULL) {
  # Read column names
  col_names <- readr::read_csv(x, n_max = 0, show_col_types = FALSE) |>
    names()

  # Read canvas data
  canvas_df <- suppressWarnings(
    readr::read_csv(x,
      skip = 3,
      col_names = col_names,
      na = c("N/A"),
      show_col_types = FALSE
    )
  )

  # Define columns to keep and remove
  keep <- c("Student", "SIS User ID", "SIS Login ID")
  throw <- c(
    "ID", "Section", "Current Score", "Unposted Current Score",
    "Final Score", "Unposted Final Score"
  )

  # Get available columns for selection
  choose <- col_names[!col_names %in% c(keep, throw)]
  # If cols is provided, use those columns directly
  if (!is.null(cols)) {
    selected_cols <- choose[cols]
    raw_marks <- canvas_df |>
      dplyr::select(dplyr::all_of(c(keep, selected_cols)))
  } else {
    # Display instruction to user
    cat("A list of assessments will be provided. You will be asked to select the
  columns that correspond to the assessments you would like to process.
  Press Enter to continue...\n")

    readline()

    # Display available columns
    cat("Available columns:\n")
    for (i in seq_along(choose)) {
      cat(sprintf("%d: %s\n", i, choose[i]))
    }

    # Get user selection
    selected <- readline("Enter column numbers (separated by spaces): ")
    selected_indices <- as.numeric(strsplit(selected, " ")[[1]])
    selected_cols <- choose[selected_indices]
  }

  # Display selected columns
  cat("\nSelected columns:\n")
  for (i in seq_along(selected_cols)) {
    cat(sprintf("%d: %s\n", i, selected_cols[i]))
  }

  # Extract relevant columns
  raw_marks <- canvas_df |>
    dplyr::select(dplyr::all_of(c(keep, selected_cols))) |>
    # rename some columns for joining
    dplyr::rename(
      SID = `SIS User ID`,
      Unikey = `SIS Login ID`
    )

  return(invisible(raw_marks))
}


parse_gradescope <- function(x) {
  df <- suppressWarnings(readr::read_csv(x, show_col_types = FALSE))
  out <- df |>
    dplyr::select("First Name":"Status")
  return(invisible(out))
}


parse_arrangements <- function(x, uos = NULL, year = NULL) {
  if (is.null(uos)) {
    stop("UOS parameter must be provided")
  }
  if (is.null(year)) {
    stop("Year parameter must be provided")
  }

  df <- suppressWarnings(readr::read_csv(x, show_col_types = FALSE))

  # Create pattern to match UOS and year
  pattern <- paste0(uos, ".*", year)

  # Filter rows where Availability matches the pattern
  out <- df |>
    dplyr::filter(grepl(pattern, availability)) |>
    dplyr::filter(state == "Approved") |>
    dplyr::filter(u_outcome_type == "Replacement exam")

  cat(sprintf("\nNumber of students with approved replacement exams: %d\n", nrow(out)))

  return(invisible(out))
}
