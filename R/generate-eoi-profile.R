#' Create EOI Applicant Profile String
#'
#' Generates a formatted string summarizing an applicant's Expression of
#' Interest (EOI) details. This is typically used for display or textual reports.
#'
#' @param applicant_data A list or a single-row data frame/tibble containing
#'   the EOI data for one applicant. Expected to have specific named elements
#'   corresponding to EOI form fields (e.g., `given_name`, `surname`,
#'   `preferred_units`, etc.).
#' @return A character string representing the formatted applicant profile in Markdown.
#'   Empty or "N/A" fields are generally omitted.
#' @export
#' @examples
#' \dontrun{
#' mock_applicant <- list(
#'   given_name = "Jane", surname = "Doe", title = "Dr",
#'   worked_at_usyd = "Yes", staff_id = "9876543",
#'   preferred_email = "jane.doe@example.com", preferred_contact = "0412345678",
#'   phd_conferred = "Yes",
#'   previous_demonstrator = "No", previous_units = NA, # or ""
#'   preferred_units = "DATA4001, COMP5002",
#'   desired_hours_per_week = "Up to 20",
#'   availability_monday = "Not available", availability_tuesday = "Full Day",
#'   availability_wednesday = "AM only", availability_thursday = "PM only",
#'   availability_friday = "Full Day",
#'   blockout_dates = "None",
#'   completed_training = "Yes, in 2022",
#'   lead_demonstrator_interest = "Maybe",
#'   lead_demonstrator_other = "Interested in curriculum development support",
#'   expertise_area = "Statistical Modeling, Bioinformatics",
#'   higher_education_degrees = "PhD (Statistics), BSc (Biology)",
#'   teaching_philosophy = "Student-centered and practical.",
#'   experience_benefit = "Industry experience in data analysis."
#' )
#' profile_string <- create_eoi_profile(mock_applicant)
#' # cat(profile_string) # To see the output as it would be printed
#'
#' # Example with a data frame row
#' mock_applicant_df <- as.data.frame(mock_applicant)
#' profile_string_df <- create_eoi_profile(mock_applicant_df)
#' # cat(profile_string_df)
#' }
create_eoi_profile <- function(applicant_data) {
  # Ensure applicant_data is a list-like structure
  if (is.data.frame(applicant_data)) {
    if (nrow(applicant_data) != 1) {
      stop("If applicant_data is a data frame, it must contain only one row.")
    }
    ad <- as.list(applicant_data)
  } else if (is.list(applicant_data)) {
    ad <- applicant_data
  } else {
    stop("applicant_data must be a list or a single-row data frame.")
  }

  # Helper to safely get values, using .get_val_or_default
  get_val <- function(field_name, default_val = "N/A") {
    val <- ad[[field_name]]
    .get_val_or_default(val, default_val)
  }

  # Helper function to map availability text to AM/PM symbols or text
  map_availability_to_symbols <- function(avail_text) {
    norm_text <- tolower(trimws(avail_text))

    # Handle clear "unavailable" cases first
    if (norm_text %in% c("", "n/a", "not available", "unavailable") ||
      grepl("unavailable", norm_text, fixed = TRUE)) {
      return(list(am = "x", pm = "x"))
    }

    is_full_day <- norm_text == "full day"
    has_am_mention <- grepl("am|morning", norm_text)
    has_pm_mention <- grepl("pm|afternoon", norm_text)

    # Initialize symbols assuming not available, then prove availability
    am_symbol <- "x"
    pm_symbol <- "x"

    if (is_full_day || (has_am_mention && has_pm_mention)) {
      # Covers "full day" or cases like "AM and PM", "Morning, Afternoon"
      am_symbol <- "\u2713"
      pm_symbol <- "\u2713"
    } else if (has_am_mention) {
      # AM mentioned, and not PM (because previous condition was false)
      am_symbol <- "\u2713"
      # pm_symbol remains "x"
    } else if (has_pm_mention) {
      # PM mentioned, and not AM (because previous conditions were false)
      # am_symbol remains "x"
      pm_symbol <- "\u2713"
    } else {
      # Not explicitly unavailable, not full day, no clear AM/PM only.
      # This is for other non-empty strings like "flexible", "by appointment".
      return(list(am = "?", pm = "?"))
    }

    return(list(am = am_symbol, pm = pm_symbol))
  }

  # Helper to format multi-line text for table cells (escape pipes, then replace newline with <br>)
  format_for_table_cell <- function(text) {
    if (text == "N/A") {
      return("")
    }
    if (!is.character(text)) text <- as.character(text)
    # Order of escaping matters:
    # 1. Escape backslashes first to prevent them from interfering with subsequent escapes
    text <- gsub("\\\\", "\\\\\\\\", text) # Replace \ with \\
    # 2. Escape pipe characters
    text <- gsub("\\|", "\\\\|", text) # Replace | with \|
    # 3. Replace newlines with <br> for HTML display in Markdown tables
    text <- gsub("\n", "<br>", text)
    return(text)
  }

  profile_parts <- c()

  # 1. Applicant Name (H1)
  title_val <- get_val("title")
  given_name_val <- get_val("given_name")
  surname_val <- get_val("surname")

  name_parts_vec <- c()
  if (title_val != "N/A") name_parts_vec <- c(name_parts_vec, title_val)
  if (given_name_val != "N/A") name_parts_vec <- c(name_parts_vec, given_name_val)
  if (surname_val != "N/A") name_parts_vec <- c(name_parts_vec, surname_val)

  if (length(name_parts_vec) > 0) {
    profile_parts <- c(profile_parts, paste0("# ", paste(name_parts_vec, collapse = " "), "\n\n"))
  } else {
    profile_parts <- c(profile_parts, "# Applicant Profile\n\n") # Fallback
  }

  prev_demo_val <- get_val("previous_demonstrator", "") # Moved from Teaching Experience section

  # 2. Key Information Table
  key_info_rows <- c()

  email_val <- get_val("preferred_email")
  if (email_val != "N/A") {
    key_info_rows <- c(key_info_rows, sprintf("|**Email**|%s|", email_val))
  }

  contact_val <- get_val("preferred_contact")
  if (contact_val != "N/A") {
    key_info_rows <- c(key_info_rows, sprintf("|**Phone**|%s|", contact_val))
  }

  worked_usyd_val <- get_val("worked_at_usyd")
  staff_id_val <- get_val("staff_id")
  usyd_staff_info <- worked_usyd_val
  if (tolower(worked_usyd_val) == "yes" && staff_id_val != "N/A") {
    usyd_staff_info <- sprintf("Yes (ID: %s)", staff_id_val)
  }
  if (worked_usyd_val != "N/A") {
    key_info_rows <- c(key_info_rows, sprintf("|**USYD Staff**|%s|", usyd_staff_info))
  }

  phd_val <- get_val("phd_conferred")
  if (phd_val != "N/A") {
    key_info_rows <- c(key_info_rows, sprintf("|**PhD Conferred**|%s|", phd_val))
  }

  training_val <- get_val("completed_training")
  if (training_val != "N/A") {
    key_info_rows <- c(key_info_rows, sprintf("|**Completed Faculty Training**|%s|", training_val))
  }

  # Add Previous SOLES Demonstrator to Key Information
  # prev_demo_val is now fetched before this section
  if (prev_demo_val != "") {
    key_info_rows <- c(key_info_rows, sprintf("|**Previous SOLES Demonstrator**|%s|", prev_demo_val))
  }

  lead_interest_val <- get_val("lead_demonstrator_interest")
  lead_other_val <- get_val("lead_demonstrator_other")
  lead_info <- lead_interest_val
  if (lead_interest_val != "N/A" && tolower(lead_interest_val) != "no" && lead_other_val != "N/A") {
    lead_info <- sprintf("%s (Details: %s)", lead_interest_val, lead_other_val)
  }
  if (lead_interest_val != "N/A") {
    key_info_rows <- c(key_info_rows, sprintf("|**Interest in Lead Demonstrator Role**|%s|", lead_info))
  }

  desired_hours_val <- get_val("desired_hours_per_week")
  if (desired_hours_val != "N/A") {
    key_info_rows <- c(key_info_rows, sprintf("|**Desired Hours per Week:**|%s|", desired_hours_val))
  }

  blockout_val <- get_val("blockout_dates")
  if (blockout_val != "N/A") {
    key_info_rows <- c(key_info_rows, sprintf("|**Blockout Dates:**|%s|", blockout_val))
  }

  if (length(key_info_rows) > 0) {
    key_info_header <- c(
      "|Key Information|Details|",
      "|---|---|"
    )
    profile_parts <- c(profile_parts, paste(c(key_info_header, key_info_rows), collapse = "\n"), "\n\n")
  }

  # 3. Availability Table
  availability_rows <- c()
  days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  day_fields <- paste0("availability_", tolower(days))

  has_availability_info <- FALSE
  for (i in seq_along(days)) {
    avail_val_text <- get_val(day_fields[i])
    # Check if any availability info exists, even if it's "N/A" which will be mapped to 'x'
    # The goal is to show the table if any day has a non-default (empty) value from source.
    # get_val returns "N/A" if field is missing or empty.
    # So, if avail_val_text is ever different from the default "N/A" for a missing field,
    # or if it's explicitly set, we consider it info.
    # A simpler check: if any day's value is not the default "N/A" from get_val for a truly absent field.
    # However, the original data might contain "N/A" as a string.
    # Let's assume if get_val returns something other than its own default_val for a missing field,
    # or if the field exists, it's info.
    # The current get_val returns "N/A" for empty/missing.
    # So, if any avail_val_text is *ever* populated (even with "N/A" from data), we show table.
    # This means we always show the table if the fields are expected.
    # Let's refine: only show table if at least one day has an actual stated availability
    # (i.e., not just "N/A" from get_val because the field was missing).
    # The current logic of map_availability_to_symbols will produce 'x' for "N/A".
    # We need to know if the "N/A" was from actual data or from get_val default.
    # For simplicity, if any of the availability fields exist in `ad`, we show the table.
    # A pragmatic approach: if any `ad[[day_field]]` is not NULL.
    raw_avail_values <- sapply(day_fields, function(field) !is.null(ad[[field]]))
    if (any(raw_avail_values)) {
      has_availability_info <- TRUE
    }

    symbols <- map_availability_to_symbols(avail_val_text) # avail_val_text can be "N/A"
    availability_rows <- c(availability_rows, sprintf("|%s|%s|%s|", days[i], symbols$am, symbols$pm))
  }

  if (has_availability_info) {
    availability_header <- c(
      "## Availability\n",
      "|Day|AM (8am - 1pm)|PM (1pm - 6pm)|",
      "|---|:---:|:---:|"
    )
    profile_parts <- c(profile_parts, paste(c(availability_header, availability_rows), collapse = "\n"), "\n\n")
  }

  # 4. Teaching Experience
  # prev_demo_val is now fetched earlier, before Key Information section

  # Process Previous Units Taught
  prev_units_val_raw <- get_val("previous_units", "")
  prev_units_display_val <- "" # Initialize as empty
  if (prev_units_val_raw != "" && prev_units_val_raw != "N/A") {
    # Only process if it's not empty and not "N/A"
    processed_val <- gsub("\n", " ", prev_units_val_raw) # Replace newlines with spaces
    prev_units_display_val <- trimws(gsub("\\s+", " ", processed_val)) # Replace multiple spaces with single and trim
  }
  # prev_units_display_val will be an empty string if raw was "" or "N/A"

  # Process Preferred Units for Consideration
  pref_units_val_original <- get_val("preferred_units", "")
  pref_units_md_list <- "" # Will hold the formatted bullet list string e.g., "- Unit A\n- Unit B"

  if (pref_units_val_original != "" && pref_units_val_original != "N/A") {
    units_list <- strsplit(pref_units_val_original, ",")[[1]]
    # Create bullet points directly
    formatted_bullet_points <- sapply(units_list, function(unit) {
      trimmed_unit <- trimws(unit)
      if (nzchar(trimmed_unit)) { # Ensure unit is not empty after trimming
        paste0("- ", trimmed_unit) # Format as a bullet point
      } else {
        NULL # Return NULL for empty units to filter them out later
      }
    })
    # Filter out any NULLs that resulted from empty strings after split and trim
    formatted_bullet_points <- Filter(Negate(is.null), formatted_bullet_points)
    if (length(formatted_bullet_points) > 0) {
      pref_units_md_list <- paste(formatted_bullet_points, collapse = "\n")
    }
  }

  # Build the teaching experience section content parts
  teaching_experience_md_parts <- c() # Store individual Markdown strings for each sub-section

  # Add "Previous Units Taught" if content exists
  if (nzchar(prev_units_display_val)) {
    teaching_experience_md_parts <- c(
      teaching_experience_md_parts,
      paste0("**Previous Units Taught**: ", prev_units_display_val)
    )
  }

  # Add "Preferred Units for Consideration" if content exists
  if (nzchar(pref_units_md_list)) {
    # pref_units_md_list is already like "- Unit A\n- Unit B"
    # The request is: **Header**:\n- Unit A\n- Unit B\n (extra \n after list)
    preferred_units_block_md <- paste0(
      "**Preferred Units for Consideration**:\n\n",
      pref_units_md_list,
      "\n"
    ) # The extra newline after the list itself
    teaching_experience_md_parts <- c(teaching_experience_md_parts, preferred_units_block_md)
  }

  # If there's any content for the "Teaching Experience" section
  if (length(teaching_experience_md_parts) > 0) {
    # Join the parts (e.g., "Previous Units..." and "Preferred Units...") with "\n\n"
    # This creates the body of the "Teaching Experience" section
    teaching_experience_body_md <- paste(teaching_experience_md_parts, collapse = "\n\n")

    # Prepend the main section header "## Teaching Experience\n\n"
    full_section_md <- paste0("## Teaching Experience\n\n", teaching_experience_body_md)

    # Add the complete section to profile_parts, followed by the standard two newlines
    # to separate it from the next section.
    profile_parts <- c(profile_parts, full_section_md, "\n\n")
  }
  # The comment "# prev_demo_val is no longer used..." is removed as part of the block replacement.

  # 5. Background Information
  # This section now uses paragraphs with in-text headers instead of a table.

  # Retrieve all background values first
  expertise_val <- get_val("expertise_area")
  degrees_val <- get_val("higher_education_degrees")
  philosophy_val <- get_val("teaching_philosophy")
  experience_val <- get_val("experience_benefit")

  # Check if any background information is available to include the "## Background" header
  has_any_background_info <- expertise_val != "N/A" ||
    degrees_val != "N/A" ||
    philosophy_val != "N/A" ||
    experience_val != "N/A"

  if (has_any_background_info) {
    profile_parts <- c(profile_parts, "## Background\n\n")

    if (expertise_val != "N/A") {
      profile_parts <- c(profile_parts, paste0("**Area(s) of Expertise**: ", expertise_val, "\n\n"))
    }

    if (degrees_val != "N/A") {
      profile_parts <- c(profile_parts, paste0("**Higher education degrees and majors**: ", degrees_val, "\n\n"))
    }

    if (philosophy_val != "N/A") {
      profile_parts <- c(profile_parts, paste0("**Teaching Philosophy**: ", philosophy_val, "\n\n"))
    }

    if (experience_val != "N/A") {
      profile_parts <- c(profile_parts, paste0("**How teaching benefits school**: ", experience_val, "\n\n"))
    }
  }

  # Combine all parts
  final_profile_string <- paste(profile_parts, collapse = "")

  # Cleanup multiple newlines:
  # 1. Reduce sequences of 3 or more newlines to 2 newlines.
  final_profile_string <- gsub("\n{3,}", "\n\n", final_profile_string)

  # 2. Trim whitespace from the end of the string.
  final_profile_string <- trimws(final_profile_string, which = "right")

  # 3. Ensure a single trailing newline if the string is not empty.
  if (nzchar(final_profile_string)) {
    final_profile_string <- paste0(final_profile_string, "\n")
  }

  return(final_profile_string)
}

#' Render EOI Applicant Profiles to a Single PDF with TOC
#'
#' This function takes a data frame of applicant data, generates a Markdown profile
#' for each applicant using \code{\link{create_eoi_profile}}, concatenates these
#' profiles with page breaks, and then renders the combined content to a single PDF
#' document using Quarto with the Typst format. The resulting PDF includes a
#' table of contents. It checks for Quarto's availability and handles
#' temporary file creation and cleanup.
#'
#' @param all_applicants_data A data frame where each row represents an applicant.
#'   Each row should contain the necessary EOI data fields compatible with
#'   what \code{\link{create_eoi_profile}} expects when processing a single applicant (as a list).
#' @param output_pdf_path Character string; the desired file path for the
#'   output PDF document.
#'
#' @return Invisibly returns the \code{output_pdf_path} on successful PDF generation.
#'   Stops with an error if Quarto CLI is not found or if rendering fails.
#' @export
#' @importFrom quarto quarto_render quarto_path
#'
#' @examples
#' \dontrun{
#' if (interactive() && !is.null(quarto::quarto_path())) {
#'   mock_applicants_df <- data.frame(
#'     title = c("Mr", "Dr"),
#'     given_name = c("John", "Jane"),
#'     surname = c("Smith", "Doe"),
#'     preferred_email = c("john.smith@example.com", "jane.doe@example.com"),
#'     preferred_units = c("INFO1000, DATA1001", "COMP2000, COMP2001"),
#'     availability_monday = c("Full Day", "Not available"),
#'     availability_tuesday = c("AM only", "Full Day"),
#'     availability_wednesday = c("Not available", "PM only"),
#'     availability_thursday = c("PM only", "Not available"),
#'     availability_friday = c("Full Day", "Full Day"),
#'     expertise_area = c("Data Analysis", "Machine Learning"),
#'     higher_education_degrees = c("MSc Data Science", "PhD Computer Science"),
#'     stringsAsFactors = FALSE
#'   )
#'   temp_pdf_file <- tempfile(fileext = ".pdf")
#'   render_eoi_profile_to_pdf(mock_applicants_df, temp_pdf_file)
#'   if (file.exists(temp_pdf_file)) {
#'     message("PDF generated: ", temp_pdf_file)
#'     # To open the PDF (platform dependent):
#'     # if (Sys.info()["sysname"] == "Darwin") system2("open", temp_pdf_file)
#'     # if (Sys.info()["sysname"] == "Windows") shell.exec(temp_pdf_file)
#'     # if (Sys.info()["sysname"] == "Linux") system2("xdg-open", temp_pdf_file)
#'   }
#'   unlink(temp_pdf_file) # Clean up
#' } else {
#'   message("Quarto not found or not in interactive session. Skipping PDF example.")
#' }
#' }
render_eoi_profile_to_pdf <- function(all_applicants_data, output_pdf_path) {
  # Check for Quarto CLI
  quarto_bin <- tryCatch(
    {
      quarto::quarto_path()
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(quarto_bin)) {
    stop(
      "Quarto CLI not found. Please install Quarto (see https://quarto.org/docs/get-started/) ",
      "and ensure it's in your system's PATH, or that the QUARTO_PATH environment variable is set."
    )
  }

  # Determine target directory and filename
  target_dir <- dirname(output_pdf_path)
  target_filename <- basename(output_pdf_path)

  # Ensure target directory exists
  if (target_dir != "." && !dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(target_dir)) { # Verify creation
      stop(paste("Failed to create output directory:", target_dir))
    }
  }
  # Normalize target_dir to be absolute for tempfile's tmpdir argument
  abs_target_dir <- normalizePath(target_dir, mustWork = TRUE) # mustWork = TRUE now as dir should exist

  # --- NEW LOGIC FOR MULTIPLE APPLICANTS ---
  if (!is.data.frame(all_applicants_data)) {
    stop("'all_applicants_data' must be a data frame.")
  }

  final_markdown_body <- "" # Initialize

  if (nrow(all_applicants_data) == 0) {
    warning("No applicant data provided. A PDF with a message 'No applicant data to display.' will be generated.")
    final_markdown_body <- "No applicant data to display.\n" # Add newline for consistency
  } else {
    num_applicants <- nrow(all_applicants_data)
    markdown_profiles_list <- vector("list", num_applicants) # Pre-allocate list

    for (i in 1:num_applicants) {
      current_applicant_row_as_list <- as.list(all_applicants_data[i, , drop = FALSE])
      # create_eoi_profile already adds a trailing \n
      markdown_profiles_list[[i]] <- create_eoi_profile(current_applicant_row_as_list)
    }
    # Join with Typst page breaks.
    final_markdown_body <- paste(markdown_profiles_list, collapse = "\n{{< pagebreak >}}\n\n")
  }
  # --- END OF NEW LOGIC FOR MULTIPLE APPLICANTS ---

  # Create a temporary .qmd file *inside the target directory*
  temp_qmd_path <- tempfile(pattern = "eoi_profile_doc_", tmpdir = abs_target_dir, fileext = ".qmd")
  on.exit(unlink(temp_qmd_path, force = TRUE), add = TRUE)

  # Prepare QMD content with Typst format and TOC
  qmd_full_content <- paste0(
    "---\n",
    "format: typst\n",
    "toc: true\n",
    "toc-depth: 1\n",
    "---\n\n",
    "{{< pagebreak >}}\n\n",
    final_markdown_body
  )

  # Write to the temporary .qmd file
  tryCatch(
    {
      writeLines(qmd_full_content, temp_qmd_path, useBytes = TRUE)
    },
    error = function(e) {
      stop(paste("Failed to write to temporary QMD file:", temp_qmd_path, "\nError:", e$message))
    }
  )

  # Temporarily change working directory to ensure Quarto outputs correctly.
  old_wd <- getwd()
  # This on.exit for setwd(old_wd) is added after the on.exit for unlink().
  # Due to LIFO, setwd(old_wd) will run BEFORE unlink().
  on.exit(setwd(old_wd), add = TRUE)

  setwd(abs_target_dir) # Change to target directory for Quarto execution

  # Render the QMD to PDF.
  render_result <- tryCatch(
    {
      quarto::quarto_render(
        input = basename(temp_qmd_path), # Use basename as we are in temp_qmd_path's directory
        output_file = target_filename, # Output is just the filename
        as_job = FALSE,
        quiet = FALSE # Keep verbose to see Quarto CLI output
      )
      # After quarto_render, we are still in abs_target_dir.
      # Check if the file was actually created in the current (target) directory.
      if (!file.exists(target_filename)) {
        expected_file_location <- file.path(abs_target_dir, target_filename) # For error message clarity
        stop(paste0(
          "Quarto rendering reported success but output PDF '", target_filename,
          "' not found in the target directory: ", abs_target_dir,
          ". Full expected path: ", expected_file_location
        ))
      }
      TRUE # Indicate success
    },
    error = function(e) {
      # WD is abs_target_dir here. on.exit will restore it to old_wd.
      err_msg <- paste0(
        "Failed to render EOI profile to PDF.\n",
        "Attempted to render in directory: ", abs_target_dir, "\n",
        "Input QMD (basename used): ", basename(temp_qmd_path), " (full path: ", temp_qmd_path, ")\n",
        "Target output filename for Quarto: ", target_filename, "\n",
        "Original full output PDF path: ", output_pdf_path, "\n",
        "Working directory at time of Quarto call: ", abs_target_dir, "\n",
        "Quarto CLI Error Details: ", e$message
      )
      stop(err_msg, call. = FALSE)
      FALSE # Indicate failure
    }
  )
  # old_wd is restored by on.exit.

  if (!render_result) {
    # If rendering failed, on.exit for setwd(old_wd) would have already run or will run.
    stop("PDF rendering failed. See previous messages for details.")
  }

  # Invisibly return the original output path on success
  invisible(output_pdf_path)
}
