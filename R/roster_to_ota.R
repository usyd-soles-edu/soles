#' Load and process a roster file for a specific unit
#'
#' This function loads a roster Excel file and processes it according to the unit specified.
#' Currently, only BIOL1007 is supported.
#'
#' @param path Path to the Excel roster file.
#' @param unit Character string specifying the unit (e.g., "BIOL1007").
#' @return A processed data frame with staff allocation counts per week and session.
#' @export
load_roster <- function(path, unit) {
  unit <- toupper(unit) # Ensure unit is in uppercase for consistency
  if (unit == "BIOL1007") {
    out <- roster_is_biol1007(path)
  } else {
    stop("Unit not supported")
  }
  attr(out, "source_file") <- path
  out
}

#' Process BIOL1007 roster Excel file into detailed staff allocation counts
#'
#' Reads and cleans the BIOL1007 roster, staff, and timetable sheets, merges them, and produces a summary
#' of staff allocations per week, session, and role.
#'
#' @param path Path to the Excel roster file.
#' @return A data frame summarizing staff allocations by week, role, and session.
#' @importFrom readxl read_excel
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate across filter left_join select group_by ungroup count recode
#' @importFrom tidyr fill pivot_longer extract drop_na pivot_wider
#' @importFrom stringr str_extract
#' @export
roster_is_biol1007 <- function(path) {
  # Read the roster file and clean it
  suppressMessages({
    roster <- read_excel(path, skip = 4) |>
      clean_names() |>
      mutate(across(-c(1:4), ~ replace(., . == ".", NA_character_))) |>
      fill(c(week, practical), .direction = "down") |>
      filter(!is.na(date)) |>
      mutate(week = sprintf("w%02d", as.numeric(week))) |>
      pivot_longer(
        cols = -c(1:4),
        names_to = "role_lab",
        values_to = "name"
      ) |>
      tidyr::extract(
        role_lab,
        into = c("role", "lab"),
        regex = "([a-z]+)_(\\d+)",
        remove = FALSE
      ) |>
      mutate(
        date = as.Date(date),
        day_of_week = str_extract(session, "^[A-Za-z]{3}"),
        start_hour = as.integer(str_extract(session, "(?<= )[0-9]{1,2}(?=-)")),
        start_time = sprintf(
          "%02d:00",
          ifelse(start_hour < 8, start_hour + 12, start_hour)
        )
      )
  })

  # Read the staff file and clean it
  staff <- read_excel(path, sheet = 2) |>
    clean_names()

  # Read the timetable file and clean it
  tt <- read_excel(path, sheet = "details", skip = 8) |>
    clean_names() |>
    mutate(
      lab = str_extract(part_location, "\\d{3}(?=\\D*$)")
    )

  # Join the roster with the timetable
  roster_detailed <- roster |>
    left_join(tt, by = c("day_of_week", "lab", "start_time")) |>
    left_join(staff, by = join_by(name == label)) |>
    mutate(
      fullname = paste0(given_name, " ", surname),
      role = recode(role, demo = "Demonstrator", sup = "Tutor")
    ) |>
    select(-c(practical, surname, given_name, start_hour, role_lab, lab, new))

  # Get all unique weeks
  all_weeks <- sort(unique(roster$week))

  counts <- roster_detailed |>
    group_by(fullname, role, day_of_week, start_time, subject_code, short_code, part_location, phd, week, .drop = FALSE) |>
    count(name = "n") |>
    ungroup() |>
    group_by(fullname, role, day_of_week, start_time, subject_code, short_code, part_location, phd) |>
    tidyr::complete(
      week = all_weeks,
      fill = list(n = 0)
    ) |>
    ungroup() |>
    pivot_wider(
      names_from = week,
      values_from = n,
      values_fill = 0
    )
  counts
}

#' Process staff allocation data to assign paycodes and calculate hours
#'
#' This function takes a data frame of staff allocations (as produced by roster_is_BIOL1007),
#' converts session counts to hours, assigns paycodes based on role and PhD status, and arranges the data.
#'
#' @param data A data frame of staff allocations, typically from roster_is_BIOL1007.
#' @return A data frame with paycodes, total hours, and session details for each staff member.
#' @importFrom dplyr mutate across group_by ungroup arrange select case_when row_number
#' @export
process_paycodes <- function(data) {
  # Define and sort week columns
  week_cols <- names(data)[grepl("^w\\d{2}$", names(data))]
  week_cols_sorted <- week_cols[order(as.numeric(sub("w", "", week_cols)))]

  # Define day order
  day_order <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

  # Reorder data frame to have sorted week columns
  non_week_cols <- setdiff(names(data), week_cols)
  data <- data[, c(non_week_cols, week_cols_sorted)]

  out <-
    data |>
    # Convert session counts to hours for each week
    mutate(across(all_of(week_cols_sorted), ~ . * 3)) |>
    # Calculate total_hours and set day order
    mutate(
      total_hours = rowSums(across(all_of(week_cols_sorted)), na.rm = TRUE),
      day_of_week = factor(day_of_week, levels = day_order)
    ) |>
    # Group by person to determine first session
    group_by(fullname) |>
    arrange(day_of_week, start_time) |>
    # Assign paycodes
    mutate(
      paycode = case_when(
        role == "Demonstrator" & phd == 1 ~ "DE1",
        role == "Demonstrator" & (is.na(phd) | phd == 0) ~ "DE2",
        role == "Tutor" & phd == 1 & row_number() == 1 ~ "TU1",
        role == "Tutor" & phd == 1 & row_number() > 1 ~ "TU3",
        role == "Tutor" & (is.na(phd) | phd == 0) & row_number() == 1 ~ "TU2",
        role == "Tutor" & (is.na(phd) | phd == 0) & row_number() > 1 ~ "TU4",
        TRUE ~ NA_character_
      )
    ) |>
    ungroup() |>
    mutate(activity = "Practical") |>
    select(subject_code, short_code, part_location, day_of_week, start_time, activity, role, total_hours, fullname, phd, paycode, everything()) |>
    arrange(subject_code, fullname, day_of_week, start_time) |>
    filter(fullname != "NA NA") # Remove rows with NA names

  attr(out, "source_file") <- attr(data, "source_file")
  out
}

#' Log OTA output to an Excel file
#'
#' Saves the processed OTA data frame to an Excel file, with options for custom
#' filenames and logging directories. The original source file path is retrieved
#' from the data's attributes.
#'
#' @param data A data frame, expected to have a "source_file" attribute.
#' @param log_dir The directory where the log file will be saved. Defaults to "logs".
#' @param filename The name of the output file. If NULL, a name is generated
#'   based on the timestamp and the original roster filename.
#' @return The input data frame, invisibly.
#' @export
#' @importFrom openxlsx2 write_xlsx
log_ota_output <- function(data, log_dir = "logs", filename = NULL) {
  roster_path <- attr(data, "source_file")
  if (is.null(roster_path)) {
    stop("The 'source_file' attribute is missing from the data.")
  }

  if (is.null(filename)) {
    timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
    basename <- tools::file_path_sans_ext(basename(roster_path))
    filename <- paste0(timestamp, "_", basename, "_ota_draft.xlsx")
  }

  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  output_path <- file.path(log_dir, filename)
  openxlsx2::write_xlsx(data, file = output_path)

  message("OTA draft saved to: ", output_path)
  invisible(data)
}
