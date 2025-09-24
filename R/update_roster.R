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
  
  if (prev_removed > 0 || curr_removed > 0) {
    if(verbose) lgr$info(glue::glue("Data preprocessing: removed {prev_removed} duplicate entries from previous, {curr_removed} from current roster"))
  }

  # Compute changes in one pass using full_join for efficiency
  full_join_df <- full_join(previous_df, current_df, by = key_cols, suffix = c("_prev", "_curr"))
  
  additions <- full_join_df |> 
    filter(is.na(name_prev)) |> 
    select(all_of(key_cols), name = name_curr, rate_code = rate_code_curr)
  
  removals <- full_join_df |> 
    filter(is.na(name_curr)) |> 
    select(all_of(key_cols), name = name_prev, rate_code = rate_code_prev)
  
  common <- full_join_df |> 
    filter(!is.na(name_prev) & !is.na(name_curr))
  
  replacements <- common |> 
    filter(name_prev != name_curr) |> 
    select(all_of(key_cols),
           previous_name = name_prev,
           current_name = name_curr,
           previous_rate = rate_code_prev,
           current_rate = rate_code_curr)
  
  rate_changes <- common |> 
    filter(name_prev == name_curr, rate_code_prev != rate_code_curr) |> 
    select(all_of(key_cols),
           name = name_prev,
           previous_rate = rate_code_prev,
           current_rate = rate_code_curr)

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
summary.roster_changes <- function(object, ...) {
  # Helper function to format role names
  format_role <- function(role) {
    ifelse(role == "sup", "Supervisor", role)
  }
  
  # Helper function to format a single change
  format_change <- function(change_type, row) {
    week <- row$week
    day <- row$day_of_week
    time <- row$start_time
    role <- format_role(row$role)
    
    switch(change_type,
           "addition" = glue::glue("In {week}, {row$name} is added as {role} on {day} {time}"),
           "removal" = glue::glue("In {week}, {row$name} is removed from {day} {time} ({role}) with no replacement"),
           "replacement" = glue::glue("In {week}, {row$current_name} replaces {row$previous_name} on {day} {time} ({role})"),
           "rate_change" = glue::glue("In {week}, {row$name}'s rate changed from {row$previous_rate} to {row$current_rate} on {day} {time} ({role})")
    )
  }
  
  # Calculate totals
  total_additions <- nrow(object$additions)
  total_removals <- nrow(object$removals)
  total_replacements <- nrow(object$replacements)
  total_rate_changes <- nrow(object$rate_changes)
  total_changes <- total_additions + total_removals + total_replacements + total_rate_changes
  
  # Print summary header
  cat("\nRoster Changes Summary\n")
  cat("======================\n")
  cat(glue::glue("Total changes: {total_changes} |"))
  cat(glue::glue(" Additions: {total_additions} |"))
  cat(glue::glue(" Removals: {total_removals} |"))
  cat(glue::glue(" Replacements: {total_replacements} |"))
  cat(glue::glue(" Rate changes: {total_rate_changes} "))
  cat("\n\n")
  
  # Print details for each type
  if (total_additions > 0) {
    cat("Additions:\n")
    for (i in seq_len(nrow(object$additions))) {
      row <- object$additions[i, ]
      cat(format_change("addition", row), "\n")
    }
    cat("\n")
  }
  
  if (total_removals > 0) {
    cat("Removals:\n")
    for (i in seq_len(nrow(object$removals))) {
      row <- object$removals[i, ]
      cat(format_change("removal", row), "\n")
    }
    cat("\n")
  }
  
  if (total_replacements > 0) {
    cat("Replacements:\n")
    for (i in seq_len(nrow(object$replacements))) {
      row <- object$replacements[i, ]
      cat(format_change("replacement", row), "\n")
    }
    cat("\n")
  }
  
  if (total_rate_changes > 0) {
    cat("Rate changes:\n")
    for (i in seq_len(nrow(object$rate_changes))) {
      row <- object$rate_changes[i, ]
      cat(format_change("rate_change", row), "\n")
    }
    cat("\n")
  }
  
  if (total_changes == 0) {
    cat("No changes detected.\n")
  }
  
  invisible(object)
}