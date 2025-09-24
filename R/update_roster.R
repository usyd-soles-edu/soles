#' Compare Two Roster Data Frames for Changes
#'
#' This function compares two roster data frames (previous and current) to identify
#' additions, removals, replacements, and optional rate changes.
#' If only current_df is provided, it automatically finds and loads the latest previous
#' roster for the unit from the logs directory.
#'
#' @param current_df Data frame from the current roster.
#' @param previous_df Optional data frame from the previous roster. If NULL, automatically loads the latest.
#' @param verbose Logical, whether to print informational messages. Default TRUE.
#' @return A list containing data frames for additions, removals, replacements, and rate_changes.
#' @importFrom dplyr anti_join inner_join filter select distinct group_by n across all_of
#' @importFrom glue glue
#' @importFrom utils read.csv write.csv
#' @import lgr
#' @importFrom stringr str_extract
#' @import cli
#' @export
update_roster <- function(current_df, previous_df = NULL, verbose = TRUE) {
  # Validate inputs
  if (!is.data.frame(current_df)) {
    stop("current_df must be a data frame")
  }
  if (!is.null(previous_df) && !is.data.frame(previous_df)) {
    stop("previous_df must be a data frame or NULL")
  }
  
  # Define key columns that uniquely identify a roster slot
  key_cols <- c("week", "date", "lab", "day_of_week", "start_time", "role")
  required_cols <- c(key_cols, "name", "rate_code")
  
  # Check required columns exist
  missing_cols <- setdiff(required_cols, names(current_df))
  if (length(missing_cols) > 0) {
    stop("current_df is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  if (!is.null(previous_df)) {
    missing_cols_prev <- setdiff(required_cols, names(previous_df))
    if (length(missing_cols_prev) > 0) {
      stop("previous_df is missing required columns: ", paste(missing_cols_prev, collapse = ", "))
    }
  }
  
  # Helper function to get logs directory
  get_logs_dir <- function(df) {
    source_file <- attr(df, "source_file")
    if (!is.null(source_file)) {
      file.path(dirname(source_file), "logs")
    } else {
      "logs"
    }
  }
  
  # Helper function to save roster
  save_roster <- function(df, logs_dir, message) {
    if (!dir.exists(logs_dir)) dir.create(logs_dir, recursive = TRUE)
    unit <- attr(df, "unit")
    timestamp <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
    filename <- glue::glue("{unit}-{timestamp}.csv")
    filepath <- file.path(logs_dir, filename)
    write.csv(df, filepath, row.names = FALSE)
    if(verbose) lgr$info(glue::glue("{message}: {filename}"))
  }
  
  # If previous_df not provided, load the latest for the unit
  if (is.null(previous_df)) {
    unit <- attr(current_df, "unit")
    if (is.null(unit)) {
      stop("Unit attribute not found in current_df. Please provide previous_df or ensure parse_roster was called with unit.")
    }
    
    # Get logs directory relative to source file
    logs_dir <- get_logs_dir(current_df)
    
    # Find latest roster file for this unit in logs directory
    if (!dir.exists(logs_dir)) {
      if(verbose) lgr$info(glue::glue("No previous roster found for {unit} - saving current as baseline"))
      # Save current roster as baseline
      save_roster(current_df, logs_dir, "Baseline roster saved")
      return(invisible(NULL))
    }
    
    # List files matching unit-*.csv
    pattern <- paste0("^", unit, "-.*\\.csv$")
    roster_files <- list.files(logs_dir, pattern = pattern, full.names = TRUE)
    
    if (length(roster_files) == 0) {
      if(verbose) lgr$info(glue::glue("No previous roster found for {unit} - saving current as baseline"))
      # Save current roster as baseline
      save_roster(current_df, logs_dir, "Baseline roster saved")
      return(invisible(NULL))
    }
    
    # Find the latest by timestamp in filename
    timestamps <- sapply(roster_files, function(f) {
      basename_f <- basename(f)
      # Extract timestamp from filename like BIOL1007-2025-09-22-204839.csv
      ts_str <- sub(paste0("^", unit, "-(.*)\\.csv$"), "\\1", basename_f)
      as.POSIXct(ts_str, format = "%Y-%m-%d-%H%M%S")
    })
    latest_file <- roster_files[which.max(timestamps)]
    
    if(verbose) lgr$info(glue::glue("Comparing {unit} roster to previous version"))
    previous_df <- read.csv(latest_file, stringsAsFactors = FALSE, 
                           colClasses = c("character", "Date", "character", "character", 
                                        "character", "character", "character", "character", 
                                        "integer", "character", "character"))
    # Ensure date is Date
    previous_df$date <- as.Date(previous_df$date)
  } else {
    if(verbose) lgr$info("Comparing provided roster data frames")
  }
  
  # Remove duplicates efficiently
  prev_count <- nrow(previous_df)
  curr_count <- nrow(current_df)
  previous_df <- previous_df |> distinct(across(all_of(key_cols)), .keep_all = TRUE)
  current_df <- current_df |> distinct(across(all_of(key_cols)), .keep_all = TRUE)
  prev_removed <- prev_count - nrow(previous_df)
  curr_removed <- curr_count - nrow(current_df)
  
  # if (prev_removed > 0 || curr_removed > 0) {
  #   if(verbose) lgr$info(glue::glue("Data preprocessing: removed {prev_removed} duplicate entries from previous, {curr_removed} from current roster"))
  # }

  # Compute changes in one pass using full_join for efficiency
  full_join_df <- full_join(previous_df, current_df, by = key_cols, suffix = c("_prev", "_curr"))
  
  additions <- full_join_df |> 
    filter(is.na(name_prev)) |> 
    select(all_of(key_cols), name = name_curr, rate_code = rate_code_curr, subject_activitycode = subject_activitycode_curr)
  
  removals <- full_join_df |> 
    filter(is.na(name_curr)) |> 
    select(all_of(key_cols), name = name_prev, rate_code = rate_code_prev, subject_activitycode = subject_activitycode_prev)
  
  common <- full_join_df |> 
    filter(!is.na(name_prev) & !is.na(name_curr))
  
  replacements <- common |> 
    filter(name_prev != name_curr) |> 
    select(all_of(key_cols),
           previous_name = name_prev,
           current_name = name_curr,
           previous_rate = rate_code_prev,
           current_rate = rate_code_curr,
           subject_activitycode = subject_activitycode_curr)
  
  rate_changes <- common |> 
    filter(name_prev == name_curr, rate_code_prev != rate_code_curr) |> 
    select(all_of(key_cols),
           name = name_prev,
           previous_rate = rate_code_prev,
           current_rate = rate_code_curr,
           subject_activitycode = subject_activitycode_curr)

  # Check if there are changes
  total_changes <- nrow(additions) + nrow(removals) + nrow(replacements) + nrow(rate_changes)
  
  # If changes found, save the current roster
  if (total_changes > 0) {
    logs_dir <- get_logs_dir(current_df)
    save_roster(current_df, logs_dir, glue::glue("Roster changes detected ({total_changes} total) - saving updated roster"))
  } else {
    if(verbose) lgr$info(glue::glue("Roster unchanged - no updates needed"))
  }

  # Return results as a list
  result <- list(
    additions = additions,
    removals = removals,
    replacements = replacements,
    rate_changes = rate_changes
  )
  
  # Add class for summary method
  class(result) <- c("roster_changes", "list")
  
  if (total_changes == 0) {
    invisible(result)
  } else {
    result
  }
}
  
#' @export
#' @method summary roster_changes
summary.roster_changes <- function(object, ..., html = FALSE) {
  # Helper function to format role names
  format_role <- function(role) {
    ifelse(role == "sup", "Tutor", "Demonstrator")
  }
  
  # Helper function to get condensed details
  get_details <- function(row, type) {
    role <- format_role(row$role)
    
    switch(type,
           "Addition" = paste0(cli::style_bold(cli::col_blue(row$name)), " added (", role, ")"),
           "Removal" = paste0(cli::style_bold(cli::col_blue(row$name)), " removed (", role, ")"),
           "Replacement" = paste0(cli::style_strikethrough(cli::style_bold(cli::col_red(row$previous_name))), " → ", cli::style_bold(cli::col_blue(row$current_name)), " (", role, ")"),
           "Rate Change" = paste0(cli::style_bold(cli::col_blue(row$name)), " rate ", row$previous_rate, " → ", row$current_rate, " (", role, ")")
    )
  }
  
  # Helper function to get HTML formatted details
  get_details_html <- function(row, type) {
    role <- format_role(row$role)
    
    switch(type,
           "Addition" = paste0("<span style='color: blue; font-weight: bold;'>", row$name, "</span> added (", role, ")"),
           "Removal" = paste0("<span style='color: blue; font-weight: bold;'>", row$name, "</span> removed (", role, ")"),
           "Replacement" = paste0("<del style='color: red; font-weight: bold;'>", row$previous_name, "</del> &rarr; <span style='color: blue; font-weight: bold;'>", row$current_name, "</span> (", role, ")"),
           "Rate Change" = paste0("<span style='color: blue; font-weight: bold;'>", row$name, "</span> rate ", row$previous_rate, " &rarr; ", row$current_rate, " (", role, ")")
    )
  }
  
  # Helper function to build HTML table for a change type
  build_table_html <- function(changes_df, type_label) {
    if (nrow(changes_df) == 0) return("")
    
    # Add type column and arrange by date
    changes_with_type <- changes_df |> 
      mutate(type = type_label) |> 
      arrange(date)
    
    table_html <- ""
    
    # Group by week
    weeks <- unique(changes_with_type$week)
    for (wk in sort(weeks)) {
      wk_changes <- changes_with_type |> filter(week == wk)
      table_html <- paste0(table_html, "<h3>Wk ", sub('^w', '', wk), "</h3>")
      table_html <- paste0(table_html, "<table border='1' style='border-collapse: collapse; width: 100%;'>")
      table_html <- paste0(table_html, "<tr style='background-color: #f0f0f0;'><th style='padding: 8px;'>Date</th><th style='padding: 8px;'>Type</th><th style='padding: 8px;'>Details</th><th style='padding: 8px;'>Activity</th></tr>")
      
      for (i in seq_len(nrow(wk_changes))) {
        row <- wk_changes[i, ]
        details <- get_details_html(row, row$type)
        
        subject <- toupper(str_extract(row$subject_activitycode, "^[^-]+"))
        activity_code <- str_extract(row$subject_activitycode, "(?<=-).*")
        lab_num <- sub("Lab ", "", row$lab)
        start_hour <- as.integer(substr(row$start_time, 1, 2))
        end_hour <- start_hour + 3
        start_12 <- ifelse(start_hour > 12, start_hour - 12, start_hour)
        end_12 <- ifelse(end_hour > 12, end_hour - 12, end_hour)
        time_str <- paste0(start_12, "-", end_12, "pm")
        activity <- paste0(subject, " <strong>", sprintf("%02d", as.integer(activity_code)), "</strong>: ", lab_num, " ", row$day_of_week, " ", time_str)
        
        table_html <- paste0(table_html, "<tr><td style='padding: 8px;'>", format(row$date, "%Y-%m-%d"), "</td><td style='padding: 8px;'>", row$type, "</td><td style='padding: 8px;'>", details, "</td><td style='padding: 8px;'>", activity, "</td></tr>")
      }
      table_html <- paste0(table_html, "</table><br>")
    }
    return(table_html)
  }
  
  # Calculate totals
  total_additions <- nrow(object$additions)
  total_removals <- nrow(object$removals)
  total_replacements <- nrow(object$replacements)
  total_rate_changes <- nrow(object$rate_changes)
  total_changes <- total_additions + total_removals + total_replacements + total_rate_changes
  
  # Print summary header
  cat(cli::style_bold("\nRoster Changes Summary\n"))
  cat(cli::style_bold("======================\n"))
  cat("Total changes: ", total_changes, " |")
  cat(" Additions: ", total_additions, " |")
  cat(" Removals: ", total_removals, " |")
  cat(" Replacements: ", total_replacements, " |")
  cat(" Rate changes: ", total_rate_changes, " ")
  cat("\n\n")
  
  # Generate HTML if requested
  if (html) {
    # Get logs directory from source file
    source_file <- attr(object$additions, "source_file")
    if (!is.null(source_file)) {
      logs_dir <- file.path(dirname(source_file), "logs")
      if (!dir.exists(logs_dir)) {
        dir.create(logs_dir, recursive = TRUE)
      }
      timestamp <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
      filename <- glue::glue("roster_changes_summary-{timestamp}.html")
      html_path <- file.path(logs_dir, filename)
    } else {
      # Fallback to current directory
      timestamp <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
      filename <- glue::glue("roster_changes_summary-{timestamp}.html")
      html_path <- filename
    }
    
    html_content <- paste0(
      "<!DOCTYPE html><html><head><title>Roster Changes Summary</title></head><body>",
      "<h1>Roster Changes Summary</h1>",
      "<p><strong>Total changes:</strong> ", total_changes, " | <strong>Additions:</strong> ", total_additions, " | <strong>Removals:</strong> ", total_removals, " | <strong>Replacements:</strong> ", total_replacements, " | <strong>Rate changes:</strong> ", total_rate_changes, "</p>",
      if (total_additions > 0) paste0("<h2>Additions</h2>", build_table_html(object$additions, "Addition")) else "",
      if (total_removals > 0) paste0("<h2>Removals</h2>", build_table_html(object$removals, "Removal")) else "",
      if (total_replacements > 0) paste0("<h2>Replacements</h2>", build_table_html(object$replacements, "Replacement")) else "",
      if (total_rate_changes > 0) paste0("<h2>Rate changes</h2>", build_table_html(object$rate_changes, "Rate Change")) else "",
      if (total_changes == 0) "<p>No changes detected.</p>" else "",
      "</body></html>"
    )
    writeLines(html_content, html_path)
    message(glue::glue("HTML summary saved to '{html_path}'"))
  }
  
  # Helper function to print tables for a specific change type
  print_change_section <- function(changes_df, section_title, type_label) {
    if (nrow(changes_df) == 0) return()
    
    cat(cli::style_bold(section_title), "\n", sep = "")
    cat(cli::style_bold(strrep("=", nchar(section_title))), "\n", sep = "")
    
    # Add type column and arrange by date
    changes_with_type <- changes_df |> 
      mutate(type = type_label) |> 
      arrange(date)
    
    # Group by week and print tables
    weeks <- unique(changes_with_type$week)
    for (wk in sort(weeks)) {
      wk_changes <- changes_with_type |> filter(week == wk)
      cat("\n")
      cat(cli::style_bold(glue::glue("Wk {sub('^w', '', wk)}")))
      cat("\n")
      
      # Create table data frame
      table_df <- data.frame(Date = character(), Type = character(), Details = character(), Activity = character())
      for (i in seq_len(nrow(wk_changes))) {
        row <- wk_changes[i, ]
        details <- get_details(row, row$type)
        
        subject <- toupper(str_extract(row$subject_activitycode, "^[^-]+"))
        activity_code <- str_extract(row$subject_activitycode, "(?<=-).*")
        lab_num <- sub("Lab ", "", row$lab)
        start_hour <- as.integer(substr(row$start_time, 1, 2))
        end_hour <- start_hour + 3
        start_12 <- ifelse(start_hour > 12, start_hour - 12, start_hour)
        end_12 <- ifelse(end_hour > 12, end_hour - 12, end_hour)
        time_str <- paste0(start_12, "-", end_12, "pm")
        activity <- paste0(subject, " ", cli::style_bold(sprintf("%02d", as.integer(activity_code))), ": ", lab_num, " ", row$day_of_week, " ", time_str)
        
        table_df <- rbind(table_df, data.frame(
          Date = format(row$date, "%Y-%m-%d"),
          Type = row$type,
          Details = details,
          Activity = activity
        ))
      }
      
      # Print table with proper alignment using base R formatting
      # Calculate column widths based on visible content (strip ANSI codes)
      col_widths <- c(
        Date = max(nchar(table_df$Date), na.rm = TRUE),
        Type = max(nchar(table_df$Type), na.rm = TRUE),
        Details = max(nchar(cli::ansi_strip(table_df$Details)), na.rm = TRUE),
        Activity = max(nchar(cli::ansi_strip(table_df$Activity)), na.rm = TRUE)
      )

      # Ensure minimum widths
      col_widths <- pmax(col_widths, c(Date = 10, Type = 12, Details = 20, Activity = 15))

      # Function to pad strings properly accounting for ANSI codes
      pad_string <- function(text, width) {
        visible_text <- cli::ansi_strip(text)
        padding_needed <- width - nchar(visible_text)
        if (padding_needed > 0) {
          return(paste0(text, strrep(" ", padding_needed)))
        } else {
          return(text)
        }
      }

      # Print header
      cat(pad_string("Date", col_widths["Date"]), " | ",
          pad_string("Type", col_widths["Type"]), " | ",
          pad_string("Details", col_widths["Details"]), " | ",
          pad_string("Activity", col_widths["Activity"]), "\n", sep = "")

      # Print separator
      cat(strrep("-", col_widths["Date"]), "-|-",
          strrep("-", col_widths["Type"]), "-|-",
          strrep("-", col_widths["Details"]), "-|-",
          strrep("-", col_widths["Activity"]), "\n", sep = "")

      # Print rows
      for (i in seq_len(nrow(table_df))) {
        cat(pad_string(table_df$Date[i], col_widths["Date"]), " | ",
            pad_string(table_df$Type[i], col_widths["Type"]), " | ",
            pad_string(table_df$Details[i], col_widths["Details"]), " | ",
            pad_string(table_df$Activity[i], col_widths["Activity"]), "\n", sep = "")
      }
      cat("\n")
    }
  }
  
  # Print sections for each change type
  print_change_section(object$additions, "Additions", "Addition")
  print_change_section(object$removals, "Removals", "Removal") 
  print_change_section(object$replacements, "Replacements", "Replacement")
  print_change_section(object$rate_changes, "Rate changes", "Rate Change")
  
  if (total_changes == 0) {
    cat("No changes detected.\n")
  }
  
  invisible(object)
}