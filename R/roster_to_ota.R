#' Load and process a roster file for a specific unit
#'
#' This function loads a roster Excel file and processes it according to the unit specified.
#' Currently, only BIOL1007 is supported.
#'
#' @param path Path to the Excel roster file.
#' @param unit Character string specifying the unit (e.g., "BIOL1007").
#' @param verbose If `TRUE`, print verbose output. If `NULL` (default), the global log level is unchanged.
#' @return A processed data frame with staff allocation counts per week and session.
#' @importFrom logger log_info
#' @importFrom glue glue
#' @export
load_roster <- function(path, unit, verbose = NULL) {
  if (!is.null(verbose)) {
    set_log_level(verbose)
  }
  log_info(glue::glue("Loading roster for {unit} from '{path}'..."))

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
  log_info("Processing BIOL1007 roster...")
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
  log_info(glue::glue("Read {nrow(roster)} rows from the roster sheet."))

  # Read the staff file and clean it
  staff <- read_excel(path, sheet = 2) |>
    clean_names()
  log_info(glue::glue("Read {nrow(staff)} rows from the staff sheet."))

  # Read the timetable file and clean it
  tt <- read_excel(path, sheet = "details", skip = 8) |>
    clean_names() |>
    mutate(
      lab = str_extract(part_location, "\\d{3}(?=\\D*$)")
    )
  log_info(glue::glue("Read {nrow(tt)} rows from the timetable sheet."))

  # Join the roster with the timetable
  roster_detailed <- roster |>
    left_join(tt, by = c("day_of_week", "lab", "start_time")) |>
    left_join(staff, by = join_by(name == label)) |>
    mutate(
      fullname = paste0(given_name, " ", surname),
      role = recode(role, demo = "Demonstrator", sup = "Tutor")
    ) |>
    select(-c(practical, surname, given_name, start_hour, role_lab, lab, new))
  log_info(glue::glue("Identified {length(unique(roster_detailed$fullname))} unique staff members."))

  # Get all unique weeks
  all_weeks <- sort(unique(roster$week))
  log_info(glue::glue("Detected {length(all_weeks)} unique weeks."))

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
  log_info(glue::glue("Final counts data frame has {nrow(counts)} rows and {ncol(counts)} columns."))
  counts
}

#' Process staff allocation data to assign paycodes and calculate hours
#'
#' This function takes a data frame of staff allocations (as produced by roster_is_BIOL1007),
#' converts session counts to hours, assigns paycodes based on role and PhD status, and arranges the data.
#'
#' @param data A data frame of staff allocations, typically from roster_is_BIOL1007.
#' @return A data frame with paycodes, total hours, and session details for each staff member.
#' @importFrom dplyr mutate across group_by ungroup arrange select case_when row_number filter
#' @export
process_paycodes <- function(data) {
  log_info("Processing paycodes...")
  # Define and sort week columns
  week_cols <- names(data)[grepl("^w\\d{2}$", names(data))]
  week_cols_sorted <- week_cols[order(as.numeric(sub("w", "", week_cols)))]
  log_info(glue::glue("Found {length(week_cols_sorted)} week columns."))

  # Define day order
  day_order <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

  # Reorder data frame to have sorted week columns
  non_week_cols <- setdiff(names(data), week_cols)
  data <- data[, c(non_week_cols, week_cols_sorted)]

  log_info(glue::glue("Total rows before processing: {nrow(data)}"))

  long_data <- data |>
    # Convert session counts to hours for each week
    mutate(across(all_of(week_cols_sorted), ~ . * 3)) |>
    # Reshape to long format
    tidyr::pivot_longer(
      cols = all_of(week_cols_sorted),
      names_to = "week",
      values_to = "hours"
    ) |>
    # Only consider sessions with assigned hours
    filter(hours > 0) |>
    mutate(day_of_week = factor(day_of_week, levels = day_order))

  # Process Demonstrator paycodes (logic remains unchanged)
  demo_paycodes <- long_data |>
    filter(role == "Demonstrator") |>
    mutate(
      paycode = case_when(
        phd == 1 ~ "DE1",
        is.na(phd) | phd == 0 ~ "DE2",
        TRUE ~ NA_character_
      )
    )

  # Process Tutor paycodes (new week-by-week logic)
  tutor_paycodes <- long_data |>
    filter(role == "Tutor") |>
    group_by(fullname, week) |>
    arrange(day_of_week, start_time) |>
    mutate(
      session_rank = row_number(),
      paycode = case_when(
        phd == 1 & session_rank == 1 ~ "TU1",
        phd == 1 & session_rank > 1 ~ "TU3",
        (is.na(phd) | phd == 0) & session_rank == 1 ~ "TU2",
        (is.na(phd) | phd == 0) & session_rank > 1 ~ "TU4",
        TRUE ~ NA_character_
      )
    ) |>
    ungroup() |>
    select(-session_rank)

  # Combine and filter
  combined_data <- dplyr::bind_rows(demo_paycodes, tutor_paycodes) |>
    mutate(activity = "Practical") |>
    filter(fullname != "NA NA")

  # Pivot to wide format
  out <- combined_data |>
    tidyr::pivot_wider(
      id_cols = c(subject_code, short_code, part_location, day_of_week, start_time, activity, role, fullname, phd, paycode),
      names_from = week,
      values_from = hours,
      values_fill = 0
    )

  # Ensure all weeks from w01 to w13 are present
  all_weeks <- sprintf("w%02d", 1:13)
  missing_weeks <- setdiff(all_weeks, names(out))
  if (length(missing_weeks) > 0) {
    for (wk in missing_weeks) {
      out[[wk]] <- 0
    }
  }

  # Dynamically find week columns and calculate total_hours
  week_cols <- names(out)[grepl("^w\\d{2}$", names(out))]
  week_cols_sorted <- week_cols[order(as.numeric(sub("w", "", week_cols)))]

  out <- out |>
    mutate(total_hours = rowSums(across(all_of(week_cols_sorted)), na.rm = TRUE)) |>
    # Arrange columns for final output
    select(
      subject_code, short_code, part_location, day_of_week, start_time,
      role, total_hours, fullname, phd, paycode, all_of(week_cols_sorted)
    ) |>
    arrange(fullname, subject_code, day_of_week, start_time)

  log_info(glue::glue("Total rows after processing: {nrow(out)}"))

  paycode_summary <- out |>
    filter(!is.na(paycode)) |>
    dplyr::count(paycode) |>
    dplyr::mutate(paycode = as.character(paycode))

  log_info("Paycode summary:")
  for (i in 1:nrow(paycode_summary)) {
    log_info(glue::glue("  {paycode_summary$paycode[i]}: {paycode_summary$n[i]}"))
  }

  attr(out, "source_file") <- attr(data, "source_file")
  out
}

#' Compare roster with a previous version and display differences in a gt table
#'
#' This function compares a given roster data frame with a previous version from a
#' CSV file. It identifies added, removed, and modified rows and displays them
#' in a formatted `gt` table.
#'
#' @param df A data frame, typically the output of [process_paycodes()]. It must
#'   have a `source_file` attribute if `csv` is not provided.
#' @param csv Optional. Path to the CSV file for comparison. If `NULL` (default),
#'   the function automatically finds the latest relevant roster file in the
#'   'logs' directory based on the `source_file` attribute of `df`.
#' @return Invisibly returns the input data frame `df` with a `has_changes`
#'   attribute set to `TRUE` if differences were found, and `FALSE` otherwise.
#'   This allows for chaining with [write_paycodes_to_csv()].
#' @importFrom dplyr mutate across anti_join inner_join semi_join select all_of arrange bind_rows everything distinct if_else rename
#' @importFrom readr read_csv
#' @importFrom tools file_path_sans_ext
#' @importFrom logger log_info
#' @importFrom glue glue
#' @importFrom tibble tibble
#' @importFrom gt gt tab_header tab_style cell_fill cells_body cell_text tab_options tab_source_note md
#' @importFrom stringr str_detect str_to_title
#' @importFrom tidyr pivot_longer
#' @export
compare_rosters <- function(df, csv = NULL) {
  # Automatically find the latest roster file if csv path is not provided
  if (is.null(csv)) {
    source_file <- attr(df, "source_file")
    if (is.null(source_file)) {
      stop("The 'source_file' attribute is missing from the data frame `df`.")
    }
    log_dir <- file.path(dirname(source_file), "logs")
    if (!dir.exists(log_dir)) {
      log_info("Log directory not found. No previous roster to compare against.")
      attr(df, "has_changes") <- TRUE # Assume changes if no log to compare
      return(invisible(df))
    }
    original_filename_base <- tools::file_path_sans_ext(basename(source_file))
    log_files <- list.files(log_dir, pattern = paste0(".*-", original_filename_base, "\\.csv$"), full.names = TRUE)

    if (length(log_files) == 0) {
      log_info("No previous roster files found for comparison.")
      attr(df, "has_changes") <- TRUE # Assume changes if no previous file
      return(invisible(df))
    }
    latest_file <- sort(log_files, decreasing = TRUE)[1]
    csv <- latest_file
    log_info(glue::glue("Comparing with latest roster: '{basename(csv)}'"))
  }

  if (!file.exists(csv)) {
    stop(glue::glue("Comparison file not found: '{csv}'"))
  }

  # Read the old roster
  old_roster <- readr::read_csv(csv, show_col_types = FALSE, col_types = readr::cols(.default = "c"))

  # Convert all columns to character for robust comparison
  df_char <- df %>% mutate(across(everything(), as.character))
  old_roster_char <- old_roster %>% mutate(across(everything(), as.character))

  # Align columns to ensure comparison is fair
  common_cols <- intersect(names(df_char), names(old_roster_char))
  df_char <- df_char[, common_cols]
  old_roster_char <- old_roster_char[, common_cols]

  # Define key columns to uniquely identify a row
  key_cols <- c("fullname", "subject_code", "part_location", "day_of_week", "start_time", "role", "paycode")
  if (!all(key_cols %in% common_cols)) {
    stop("One or more key columns are missing from the data frames, cannot compare.")
  }

  # 1. Find added rows
  added <- anti_join(df_char, old_roster_char, by = key_cols) %>%
    mutate(change_type = "Added")

  # 2. Find removed rows
  removed <- anti_join(old_roster_char, df_char, by = key_cols) %>%
    mutate(change_type = "Removed")

  # 3. Find modified rows
  common_keys <- inner_join(df_char, old_roster_char, by = key_cols, suffix = c("_new", "_old")) %>%
    select(all_of(key_cols)) %>%
    distinct()

  new_common <- semi_join(df_char, common_keys, by = key_cols)
  old_common <- semi_join(old_roster_char, common_keys, by = key_cols)

  modified_new <- anti_join(new_common, old_common, by = common_cols)

  modified_combined <- tibble()
  if (nrow(modified_new) > 0) {
    # Use a left_join to find corresponding old rows, preventing row-count mismatches
    # from duplicated keys in the old data.
    old_common_deduped <- old_common %>%
      distinct(across(all_of(key_cols)), .keep_all = TRUE)

    modified_old <- modified_new %>%
      select(all_of(key_cols)) %>%
      left_join(old_common_deduped, by = key_cols)

    # Ensure both dataframes are sorted by key columns to align them for comparison.
    modified_new <- modified_new %>% arrange(across(all_of(key_cols)))
    modified_old <- modified_old %>% arrange(across(all_of(key_cols)))

    modified_combined <- modified_new

    value_cols <- setdiff(common_cols, key_cols)
    for (col in value_cols) {
      # Compare values, handling NA correctly.
      is_diff <- (modified_new[[col]] != modified_old[[col]]) |
        (is.na(modified_new[[col]]) & !is.na(modified_old[[col]])) |
        (!is.na(modified_new[[col]]) & is.na(modified_old[[col]]))
      is_diff[is.na(is_diff)] <- FALSE # Treat NA vs NA as not a difference

      modified_combined[[col]] <- if_else(
        is_diff,
        paste(modified_old[[col]], "→", modified_new[[col]]),
        modified_new[[col]]
      )
    }
    modified_combined <- modified_combined %>% mutate(change_type = "Modified")
  }

  has_changes <- (nrow(added) > 0 || nrow(removed) > 0 || nrow(modified_combined) > 0)
  attr(df, "has_changes") <- has_changes

  log_info("Roster comparison summary:")
  if (!has_changes) {
    log_info("No changes detected.")
  } else {
    # --- Start of Detailed Week-by-Week Summary Notes Generation ---

    week_cols <- names(df_char)[grepl("^w\\d{2}$", names(df_char))]
    session_id_cols <- c("fullname", "subject_code", "part_location", "day_of_week", "start_time", "role")

    # Pivot to long format for week-by-week comparison
    df_long <- df_char %>%
      tidyr::pivot_longer(cols = all_of(week_cols), names_to = "week", values_to = "hours") %>%
      filter(as.numeric(hours) > 0)

    old_roster_long <- old_roster_char %>%
      tidyr::pivot_longer(cols = all_of(week_cols), names_to = "week", values_to = "hours") %>%
      filter(as.numeric(hours) > 0)

    # Identify added and removed sessions
    added_sessions <- anti_join(df_long, old_roster_long, by = c(session_id_cols, "week"))
    removed_sessions <- anti_join(old_roster_long, df_long, by = c(session_id_cols, "week"))

    # Identify replacements
    replacement_session_cols <- setdiff(session_id_cols, "fullname")
    replacements <- inner_join(
      added_sessions,
      removed_sessions,
      by = c(replacement_session_cols, "week"),
      suffix = c("_new", "_old")
    )

    notes <- c()
    if (nrow(replacements) > 0) {
      notes <- c(notes, glue::glue(
        "* In Week {as.numeric(sub('w', '', replacements$week))}, {replacements$fullname_new} replaces {replacements$fullname_old} on {replacements$day_of_week} {replacements$start_time} ({str_to_title(replacements$role)})"
      ))
    }

    # Identify pure additions
    added_only <- anti_join(added_sessions, replacements, by = c("fullname" = "fullname_new", replacement_session_cols, "week"))
    if (nrow(added_only) > 0) {
      notes <- c(notes, glue::glue(
        "* In Week {as.numeric(sub('w', '', added_only$week))}, {added_only$fullname} is added to {added_only$day_of_week} {added_only$start_time} ({str_to_title(added_only$role)})"
      ))
    }

    # Identify pure removals
    removed_only <- anti_join(removed_sessions, replacements, by = c("fullname" = "fullname_old", replacement_session_cols, "week"))
    if (nrow(removed_only) > 0) {
      notes <- c(notes, glue::glue(
        "* In Week {as.numeric(sub('w', '', removed_only$week))}, {removed_only$fullname} is removed from {removed_only$day_of_week} {removed_only$start_time} ({str_to_title(removed_only$role)}) with no replacement"
      ))
    }

    summary_notes <- if (length(notes) > 0) {
      paste("### Summary of Changes:\n\n", paste(sort(notes), collapse = "\n"))
    } else {
      NULL
    }

    # --- End of Summary Notes Generation ---

    all_changes <- bind_rows(added, removed, modified_combined)

    if (nrow(all_changes) > 0) {
      all_changes <- all_changes %>%
        mutate(change_type = factor(change_type, levels = c("Added", "Modified", "Removed"))) %>%
        arrange(change_type, across(all_of(key_cols)))

      # Create a named list for week column labels
      week_cols <- names(all_changes)[grepl("^w\\d{2}$", names(all_changes))]
      week_labels <- as.character(as.numeric(sub("w", "", week_cols)))
      names(week_labels) <- week_cols

      # Combine all labels
      all_labels <- c(
        list(
          subject_code = "Subject",
          short_code = "Act code",
          part_location = "Location",
          day_of_week = "Day",
          start_time = "Start time",
          role = "Role",
          total_hours = "Total hours",
          phd = "PhD"
        ),
        as.list(week_labels)
      )

      gt_table <- all_changes %>%
        gt::gt(groupname_col = "change_type") %>%
        gt::cols_label(.list = all_labels) %>%
        gt::tab_header(
          title = "Roster Changes",
          subtitle = glue::glue("Comparison between current roster and '{basename(csv)}'")
        ) %>%
        gt::tab_options(table.font.size = "small") %>%
        gt::tab_style(
          style = list(gt::cell_fill(color = "#d4edda")),
          locations = gt::cells_body(rows = change_type == "Added")
        ) %>%
        gt::tab_style(
          style = list(gt::cell_fill(color = "#f8d7da")),
          locations = gt::cells_body(rows = change_type == "Removed")
        )

      value_cols <- setdiff(common_cols, key_cols)
      for (col in value_cols) {
        gt_table <- gt_table %>%
          gt::tab_style(
            style = list(
              gt::cell_fill(color = "#fff3cd"),
              gt::cell_text(weight = "bold")
            ),
            locations = gt::cells_body(
              columns = all_of(col),
              rows = change_type == "Modified" & stringr::str_detect(.data[[col]], "→")
            )
          )
      }

      # Add summary notes to the table footer
      if (!is.null(summary_notes)) {
        gt_table <- gt_table %>%
          gt::tab_source_note(source_note = gt::md(summary_notes))
      }

      print(gt_table)
    }
  }

  invisible(df)
}


#' Write processed paycode data to a CSV file with a timestamped log
#'
#' This function takes the output from [process_paycodes()] and writes it to a
#' CSV file. The output file is automatically named with a timestamp and the
#' original roster filename, and saved in a 'logs' subdirectory relative to the
#' original roster file.
#'
#' @param data A data frame, typically the output of [process_paycodes()]. This
#'   data frame may have a `"has_changes"` attribute (logical) to indicate
#'   whether changes have been detected compared to a previous version. If this
#'   attribute is `FALSE`, the function will skip writing the CSV.
#' @return Invisibly returns the input data frame.
#' @importFrom readr write_csv
#' @importFrom logger log_info
#' @importFrom glue glue
#' @export
write_paycodes_to_csv <- function(data) {
  # Check for the 'has_changes' attribute
  has_changes <- attr(data, "has_changes")

  # If has_changes is FALSE, log and exit without writing
  if (!is.null(has_changes) && has_changes == FALSE) {
    log_info("Skipping CSV write.")
    return(invisible(data))
  }

  # Get the original source file path from the data attribute
  source_file <- attr(data, "source_file")
  if (is.null(source_file)) {
    stop("The 'source_file' attribute is missing from the data frame.")
  }

  # Create the logs directory if it doesn't exist
  log_dir <- file.path(dirname(source_file), "logs")
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
    log_info(glue::glue("Created log directory: '{log_dir}'"))
  }

  # Create the timestamped filename
  timestamp <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
  original_filename <- tools::file_path_sans_ext(basename(source_file))
  new_filename <- glue::glue("{timestamp}-{original_filename}.csv")
  output_path <- file.path(log_dir, new_filename)

  log_info(glue::glue("Writing OTA data to '{output_path}'..."))
  readr::write_csv(data, output_path)
  log_info("Done.")
  invisible(data)
}
