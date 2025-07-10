#' Assign pay rates based on role and PhD status
#'
#' @param data A data frame containing staff roles and information.
#' @return A data frame with a new 'rate' column.
assign_rates <- function(data) {
  data %>%
    dplyr::mutate(
      rate = dplyr::case_when(
        role == "Tutor" & phd == TRUE ~ "TU1",
        role == "Tutor" & (is.na(phd) | phd == FALSE) ~ "TU2",
        role == "Repeat Tutor" & phd == TRUE ~ "TU3",
        role == "Repeat Tutor" & (is.na(phd) | phd == FALSE) ~ "TU4",
        role == "Demonstrator" & phd == TRUE ~ "DE1",
        role == "Demonstrator" & (is.na(phd) | phd == FALSE) ~ "DE2",
        TRUE ~ NA_character_
      )
    )
}

#' Parse a template from an Excel file
#'
#' This function reads roster and staff data from an Excel file, processes it,
#' and returns a cleaned and summarised data frame.
#'
#' @param path Path to the Excel file.
#' @param ota_template_sheet Name or index of the sheet containing the template data.
#' @param staff_sheet Name or index of the sheet containing the staff data.
#' @param ota_template_skip Number of rows to skip when reading the template sheet.
#' @param id_cols A character vector of column names to fill downwards.
#' @param role_prefix_map A named character vector mapping column prefixes to roles.
#' @return A processed data frame.
#' @export
parse_roster <- function(path,
                         ota_template_sheet = 1,
                         staff_sheet = "staff",
                         ota_template_skip = 4,
                         id_cols = c("week", "practical"),
                         role_prefix_map = c("^sup" = "Tutor", "^demo" = "Demonstrator")) {
  ota_template_raw <- readxl::read_excel(path, sheet = ota_template_sheet, skip = ota_template_skip) %>%
    janitor::clean_names() %>%
    # convert data that is "." to NA
    dplyr::mutate(
      dplyr::across(-c(1:4), ~ ifelse(. == ".", NA_character_, .))
    )
  staff <- readxl::read_excel(path, sheet = staff_sheet) %>%
    janitor::clean_names() %>%
    dplyr::rename(phd = ph_d)

  message("Successfully imported template and staff data.")

  if (!all(id_cols %in% names(ota_template_raw))) {
    stop("One or more ID columns are missing from the imported template.")
  }

  template <- ota_template_raw %>%
    tidyr::fill(dplyr::all_of(id_cols), .direction = "down")

  role_pattern <- paste(names(role_prefix_map), collapse = "|")

  counts_wide <- template %>%
    tidyr::pivot_longer(
      cols = dplyr::matches(role_pattern),
      names_to = "role_col",
      values_to = "name"
    ) %>%
    dplyr::mutate(
      role = sapply(role_col, function(x) {
        for (prefix in names(role_prefix_map)) {
          if (grepl(prefix, x)) {
            return(role_prefix_map[[prefix]])
          }
        }
        return(NA_character_)
      })
    ) %>%
    dplyr::filter(!is.na(name) & name != "." & !grepl("exam|semester|stuvac", name, ignore.case = TRUE)) %>%
    dplyr::arrange(name, week, practical, date, session) %>%
    dplyr::group_by(name, week) %>%
    dplyr::mutate(
      role = dplyr::if_else(
        role == "Tutor" & cumsum(role == "Tutor") > 1,
        "Repeat Tutor",
        role
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(staff %>% dplyr::select(staff_label, phd), by = c("name" = "staff_label")) %>%
    assign_rates() %>%
    dplyr::select(week, practical, date, session, role, name, rate) %>%
    dplyr::group_by(name, role, week, rate, session) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = week,
      values_from = n,
      values_fill = 0
    )

  week_cols <- setdiff(names(counts_wide), c("name", "role", "rate", "session"))
  week_nums <- suppressWarnings(as.integer(week_cols))
  week_map <- tibble::tibble(orig = week_cols, num = week_nums) %>%
    dplyr::filter(!is.na(num)) %>%
    dplyr::arrange(num) %>%
    dplyr::mutate(new = paste0("w", num))

  wk_names_sorted <- week_map$new
  week_cols_numeric <- week_map$orig
  names(counts_wide)[match(week_cols_numeric, names(counts_wide))] <- wk_names_sorted
  non_week_cols <- setdiff(names(counts_wide), wk_names_sorted)
  counts_wide <- counts_wide[, c(non_week_cols, wk_names_sorted)]

  counts_wide <- counts_wide %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(wk_names_sorted), ~ .x * 3))

  ota_draft <- counts_wide %>%
    dplyr::left_join(staff, by = c("name" = "staff_label")) %>%
    dplyr::relocate(name, full_name, phd, role, rate, session)

  ota_draft <- ota_draft %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(., 0)))

  compare_ota_templates(ota_draft, source_file_path = path, log_dir = file.path(dirname(path), "log"))

  return(ota_draft)
}


#' Prepare a template dataframe for comparison
#'
#' @param df A data frame.
#' @return A data frame prepared for comparison.
prepare_for_comparison <- function(df) {
  df <- dplyr::mutate_all(df, as.character)
  df <- df[, stringr::str_sort(names(df), numeric = TRUE), drop = FALSE]
  df <- dplyr::arrange(df, dplyr::across(dplyr::everything()))
  return(df)
}

#' Check if two templates are identical
#'
#' @param template A template data frame.
#' @param template2 Another template data frame.
#' @return A logical value indicating if the templates are identical.
#' @export
check_equal_templates <- function(template, template2) {
  df1 <- prepare_for_comparison(template)
  df2 <- prepare_for_comparison(template2)
  identical(as.data.frame(df1), as.data.frame(df2))
}

#' Plot differences between two templates
#'
#' @param before The old template data frame.
#' @param after The new template data frame.
#' @param key_cols A character vector of key column names for joining.
#' @return A gt table object (invisibly).
#' @export
plot_differences <- function(before, after) {
  if (!requireNamespace("dplyr", quietly = TRUE) ||
    !requireNamespace("tidyr", quietly = TRUE) ||
    !requireNamespace("gt", quietly = TRUE) ||
    !requireNamespace("stringr", quietly = TRUE)) {
    stop("Please install dplyr, tidyr, gt, and stringr packages.")
  }

  if (check_equal_templates(before, after)) {
    cat("The templates are identical.\n")
    return(invisible(NULL))
  }

  before <- dplyr::mutate(before, dplyr::across(dplyr::everything(), as.character))
  after <- dplyr::mutate(after, dplyr::across(dplyr::everything(), as.character))

  all_names <- union(names(before), names(after))
  week_cols <- stringr::str_sort(stringr::str_subset(all_names, "^w\\d+"), numeric = TRUE)
  key_cols <- setdiff(intersect(names(before), names(after)), week_cols)

  diff_data <- dplyr::full_join(before, after, by = key_cols, suffix = c("_old", "_new"))

  week_cols_old <- paste0(week_cols, "_old")
  week_cols_new <- paste0(week_cols, "_new")

  if (length(week_cols) == 0) {
    stop("No week columns (e.g., w1, w2) found to compare.")
  }

  diff_data <- diff_data %>%
    dplyr::mutate(
      status = dplyr::case_when(
        is.na(.data[[paste0(week_cols[1], "_old")]]) ~ "Added",
        is.na(.data[[paste0(week_cols[1], "_new")]]) ~ "Deleted",
        TRUE ~ "Potentially Modified"
      )
    )

  potentially_modified_idx <- which(diff_data$status == "Potentially Modified")
  if (length(potentially_modified_idx) > 0) {
    is_modified <- rep(FALSE, length(potentially_modified_idx))
    for (i in seq_along(potentially_modified_idx)) {
      row_idx <- potentially_modified_idx[i]
      for (wk in week_cols) {
        old_val <- diff_data[[paste0(wk, "_old")]][row_idx]
        new_val <- diff_data[[paste0(wk, "_new")]][row_idx]
        if (!identical(old_val, new_val)) {
          is_modified[i] <- TRUE
          break
        }
      }
    }
    modified_indices_in_diff_data <- potentially_modified_idx[is_modified]
    unmodified_indices_in_diff_data <- potentially_modified_idx[!is_modified]
    diff_data$status[modified_indices_in_diff_data] <- "Modified"
    diff_data$status[unmodified_indices_in_diff_data] <- "Unchanged"
  }

  diff_data <- dplyr::filter(diff_data, status != "Unchanged")

  if (nrow(diff_data) == 0) {
    cat("No differences found.\n")
    return(invisible(NULL))
  }

  display_df <- diff_data %>% dplyr::select(status, dplyr::all_of(key_cols))
  for (wk in week_cols) {
    display_df[[wk]] <- ""
  }

  for (i in 1:nrow(diff_data)) {
    row_status <- diff_data$status[i]
    for (wk in week_cols) {
      old_val <- diff_data[[paste0(wk, "_old")]][i]
      new_val <- diff_data[[paste0(wk, "_new")]][i]
      old_val_disp <- ifelse(is.na(old_val), " ", old_val)
      new_val_disp <- ifelse(is.na(new_val), " ", new_val)
      display_df[[wk]][i] <- switch(row_status,
        "Added" = new_val_disp,
        "Deleted" = old_val_disp,
        "Modified" = if (!identical(old_val, new_val)) paste0(old_val_disp, " → ", new_val_disp) else new_val_disp
      )
    }
  }

  change_summary <- diff_data %>%
    dplyr::count(status) %>%
    tidyr::pivot_wider(names_from = status, values_from = n, values_fill = list(n = 0))

  n_added <- if ("Added" %in% names(change_summary)) change_summary$Added else 0
  n_deleted <- if ("Deleted" %in% names(change_summary)) change_summary$Deleted else 0
  n_modified <- if ("Modified" %in% names(change_summary)) change_summary$Modified else 0

  cat("OTA template comparison found:", n_modified, "modified,", n_added, "added, and", n_deleted, "deleted rows.\n\n")

  display_df <- display_df %>%
    dplyr::arrange(factor(status, levels = c("Added", "Modified", "Deleted"))) %>%
    dplyr::group_by(status)

  gt_tbl <- gt::gt(display_df) %>%
    gt::tab_header(title = "OTA Template Changes") %>%
    gt::tab_options(
      table.width = "90%",
      row_group.as_column = TRUE,
      data_row.padding = gt::px(3),
      table.font.size = "small"
    ) %>%
    gt::sub_missing(missing_text = "")

  gt_tbl <- gt_tbl %>%
    gt::tab_style(
      style = gt::cell_fill(color = "#d4edda"), # light green
      locations = gt::cells_body(rows = status == "Added")
    ) %>%
    gt::tab_style(
      style = gt::cell_fill(color = "#f8d7da"), # light red
      locations = gt::cells_body(rows = status == "Deleted")
    ) %>%
    gt::tab_style(
      style = gt::cell_fill(color = "#fff3cd"), # light yellow
      locations = gt::cells_body(rows = status == "Modified")
    )

  gt_tbl <- gt_tbl %>%
    gt::tab_style(
      style = "white-space: nowrap;",
      locations = gt::cells_body()
    )

  print(gt_tbl)
  return(invisible(gt_tbl))
}

#' Compare a new template with the latest logged template
#'
#' @param new_template The new template data frame.
#' @param key_cols A character vector of key column names for joining.
#' @param log_dir The directory where template logs are stored.
#' @param source_file_path The path to the source Excel file, used for log file naming.
#' @export
compare_ota_templates <- function(new_template, source_file_path, log_dir = "log/") {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    stop("Please install the 'openxlsx2' package to write Excel files.")
  }
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  log_file_basename <- tools::file_path_sans_ext(basename(source_file_path))
  log_file_pattern <- paste0("^", log_file_basename, "_ota_template_\\d{8}_\\d{6}\\.xlsx$")
  log_files <- list.files(log_dir, pattern = log_file_pattern, full.names = TRUE)

  if (length(log_files) == 0) {
    message("This is the first run for this source file. Saving new template.")
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    output_path <- file.path(log_dir, paste0(log_file_basename, "_ota_template_", ts, ".xlsx"))
    openxlsx2::write_xlsx(new_template, output_path)
    message("New template saved to ", output_path)
  } else {
    last_file <- log_files[which.max(file.info(log_files)$mtime)]
    previous_template <- readxl::read_excel(last_file)

    if (check_equal_templates(new_template, previous_template)) {
      message("No changes found in the template.")
    } else {
      message("Roster has changed. Displaying differences and saving the updated template.")
      plot_differences(before = previous_template, after = new_template)

      # Determine rows to highlight
      before <- dplyr::mutate(previous_template, dplyr::across(dplyr::everything(), as.character))
      after <- dplyr::mutate(new_template, dplyr::across(dplyr::everything(), as.character))

      all_names <- union(names(before), names(after))
      week_cols <- stringr::str_sort(stringr::str_subset(all_names, "^w\\d+"), numeric = TRUE)
      key_cols <- setdiff(intersect(names(before), names(after)), week_cols)

      diff_data <- dplyr::full_join(before, after, by = key_cols, suffix = c("_old", "_new")) %>%
        dplyr::mutate(
          status = dplyr::case_when(
            is.na(.data[[paste0(week_cols[1], "_old")]]) ~ "Added",
            is.na(.data[[paste0(week_cols[1], "_new")]]) ~ "Deleted",
            TRUE ~ "Potentially Modified"
          )
        )

      potentially_modified_idx <- which(diff_data$status == "Potentially Modified")
      if (length(potentially_modified_idx) > 0) {
        is_modified <- sapply(potentially_modified_idx, function(row_idx) {
          any(sapply(week_cols, function(wk) {
            !identical(diff_data[[paste0(wk, "_old")]][row_idx], diff_data[[paste0(wk, "_new")]][row_idx])
          }))
        })
        diff_data$status[potentially_modified_idx[is_modified]] <- "Modified"
      }

      changed_rows_for_highlight <- diff_data %>%
        dplyr::filter(status %in% c("Added", "Modified"))

      highlight_indices <- c()
      if (nrow(changed_rows_for_highlight) > 0) {
        new_template_indexed <- new_template %>%
          dplyr::mutate(..row_index = dplyr::row_number()) %>%
          dplyr::mutate(dplyr::across(dplyr::all_of(key_cols), as.character))

        changed_keys <- changed_rows_for_highlight %>% dplyr::select(dplyr::all_of(key_cols))
        highlight_data <- dplyr::inner_join(new_template_indexed, changed_keys, by = key_cols)
        highlight_indices <- highlight_data$..row_index
      }

      # Create and save styled Excel file
      ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
      output_path <- file.path(log_dir, paste0(log_file_basename, "_ota_template_", ts, ".xlsx"))

      wb <- openxlsx2::wb_workbook()
      wb <- openxlsx2::wb_add_worksheet(wb, "Roster")
      wb <- openxlsx2::wb_add_data(wb, "Roster", new_template)

      if (length(highlight_indices) > 0) {
        dims_to_highlight <- openxlsx2::wb_dims(rows = highlight_indices + 1, cols = 1:ncol(new_template))
        wb <- openxlsx2::wb_add_fill(
          wb,
          sheet = "Roster",
          dims = dims_to_highlight,
          color = openxlsx2::wb_color(hex = "#FFFF99")
        )
      }

      openxlsx2::wb_save(wb, output_path, overwrite = TRUE)
      message("New template saved to ", output_path)
    }
  }
}
