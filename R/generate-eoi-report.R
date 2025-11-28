#' Generate Modern EOI Applicant Report (Markdown for PDF)
#'
#' Creates a professional, modern Markdown profile for an EOI applicant,
#' optimised for academic hiring decisions. Focuses on key qualifications,
#' teaching experience, and availability in a clean, scannable format.
#'
#' @param applicant_data A list or single-row data frame containing EOI data
#' @return Character string with Markdown-formatted profile
#' @export
create_eoi_profile_modern <- function(applicant_data) {
  # Convert to list if needed
  if (is.data.frame(applicant_data)) {
    if (nrow(applicant_data) != 1) {
      stop("applicant_data must contain only one row if it's a data frame")
    }
    ad <- as.list(applicant_data)
  } else if (is.list(applicant_data)) {
    ad <- applicant_data
  } else {
    stop("applicant_data must be a list or single-row data frame")
  }

  # Helper to safely get values
  get_val <- function(field_name, default_val = NULL) {
    val <- ad[[field_name]]
    .get_val_or_default(val, default_val)
  }

  # Build profile sections
  profile <- character()

  # 1. HEADER: Name and Key Badges
  title_val <- get_val("title")
  given_name <- get_val("given_name")
  surname <- get_val("surname")

  name_parts <- c(title_val, given_name, surname)
  name_parts <- name_parts[!sapply(name_parts, is.null)]
  full_name <- if (length(name_parts) > 0) {
    paste(name_parts, collapse = " ")
  } else {
    "Applicant"
  }

  profile <- c(profile, paste0("# ", full_name, "\n"))

  # Qualification badges
  badges <- character()
  if (!is.null(get_val("phd_conferred")) &&
      tolower(get_val("phd_conferred")) == "yes") {
    badges <- c(badges, "**PhD Conferred**")
  }
  if (!is.null(get_val("previous_demonstrator")) &&
      tolower(get_val("previous_demonstrator")) == "yes") {
    badges <- c(badges, "**Returning Staff**")
  }
  if (!is.null(get_val("hdr_student")) &&
      tolower(get_val("hdr_student")) == "yes") {
    badges <- c(badges, "**HDR Student**")
  }
  if (!is.null(get_val("completed_training")) &&
      tolower(get_val("completed_training")) == "yes") {
    badges <- c(badges, "**Trained**")
  }
  if (!is.null(get_val("worked_at_usyd")) &&
      tolower(get_val("worked_at_usyd")) == "yes") {
    badges <- c(badges, "**USYD Staff**")
  }

  if (length(badges) > 0) {
    profile <- c(profile, paste(badges, collapse = " | "), "\n\n")
  }

  # 2. CONTACT INFORMATION
  email <- get_val("preferred_email")
  phone <- get_val("preferred_contact")

  if (!is.null(email) || !is.null(phone)) {
    profile <- c(profile, "## Contact Information\n\n")
    if (!is.null(email)) {
      profile <- c(profile, paste0("**Email:** ", email, "\n\n"))
    }
    if (!is.null(phone)) {
      profile <- c(profile, paste0("**Phone:** ", phone, "\n\n"))
    }
  }

  # 3. PREFERRED UNITS (Critical for hiring)
  pref_units <- get_val("preferred_units")
  if (!is.null(pref_units)) {
    units_list <- strsplit(pref_units, ",")[[1]]
    units_list <- trimws(units_list)
    units_list <- units_list[nzchar(units_list)]

    if (length(units_list) > 0) {
      profile <- c(profile, "## Units of Interest\n\n")
      for (unit in units_list) {
        profile <- c(profile, paste0("- ", unit, "\n"))
      }
      profile <- c(profile, "\n")
    }
  }

  # 4. AVAILABILITY SUMMARY (Critical for scheduling)
  days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  day_fields <- paste0("availability_", tolower(days))

  avail_data <- sapply(day_fields, function(field) get_val(field))
  if (any(!sapply(avail_data, is.null))) {
    profile <- c(profile, "## Availability\n\n")
    profile <- c(profile, "| Day | Morning | Afternoon |\n")
    profile <- c(profile, "|-----|---------|----------|\n")

    for (i in seq_along(days)) {
      avail_text <- avail_data[[i]]
      if (is.null(avail_text)) avail_text <- "Not specified"

      norm_text <- tolower(trimws(avail_text))

      # Determine availability
      if (norm_text %in% c("", "n/a", "not available", "unavailable")) {
        am_sym <- "✗"
        pm_sym <- "✗"
      } else if (norm_text == "full day" ||
                 (grepl("am|morning", norm_text) &&
                  grepl("pm|afternoon", norm_text))) {
        am_sym <- "✓"
        pm_sym <- "✓"
      } else if (grepl("am|morning", norm_text)) {
        am_sym <- "✓"
        pm_sym <- "✗"
      } else if (grepl("pm|afternoon", norm_text)) {
        am_sym <- "✗"
        pm_sym <- "✓"
      } else {
        am_sym <- "?"
        pm_sym <- "?"
      }

      profile <- c(profile, paste0("| ", days[i], " | ", am_sym, " | ",
                                   pm_sym, " |\n"))
    }
    profile <- c(profile, "\n")
  }

  # 5. TEACHING EXPERIENCE & QUALIFICATIONS
  prev_units <- get_val("previous_units")
  degrees <- get_val("higher_education_degrees")
  expertise <- get_val("expertise_area")
  philosophy <- get_val("teaching_philosophy")

  if (!is.null(prev_units) || !is.null(degrees) ||
      !is.null(expertise) || !is.null(philosophy)) {
    profile <- c(profile, "## Qualifications & Experience\n\n")

    if (!is.null(degrees)) {
      profile <- c(profile, paste0("**Education:** ", degrees, "\n\n"))
    }

    if (!is.null(expertise)) {
      profile <- c(profile, paste0("**Areas of Expertise:** ",
                                   expertise, "\n\n"))
    }

    if (!is.null(prev_units)) {
      # Clean up previous units text
      prev_units_clean <- gsub("\n", " ", prev_units)
      prev_units_clean <- trimws(gsub("\\s+", " ", prev_units_clean))
      profile <- c(profile, paste0("**Previous Teaching:** ",
                                   prev_units_clean, "\n\n"))
    }

    if (!is.null(philosophy)) {
      profile <- c(profile, paste0("**Teaching Philosophy:** _",
                                   philosophy, "_\n\n"))
    }
  }

  # 6. ADDITIONAL INFORMATION
  lead_interest <- get_val("lead_demonstrator_interest")
  blockout <- get_val("blockout_dates")
  experience_benefit <- get_val("experience_benefit")

  additional_info <- character()

  if (!is.null(lead_interest) && tolower(lead_interest) != "no") {
    lead_other <- get_val("lead_demonstrator_other")
    lead_text <- if (!is.null(lead_other)) {
      paste0(lead_interest, " (", lead_other, ")")
    } else {
      lead_interest
    }
    additional_info <- c(additional_info,
                        paste0("**Interest in Lead Role:** ",
                               lead_text, "\n\n"))
  }

  if (!is.null(blockout)) {
    additional_info <- c(additional_info,
                        paste0("**Blockout Dates:** ", blockout, "\n\n"))
  }

  if (!is.null(experience_benefit)) {
    additional_info <- c(additional_info,
                        paste0("**Value to SOLES:** _", experience_benefit,
                               "_\n\n"))
  }

  if (length(additional_info) > 0) {
    profile <- c(profile, "## Additional Information\n\n")
    profile <- c(profile, additional_info)
  }

  # Combine and return
  final_profile <- paste(profile, collapse = "")

  # Clean up multiple newlines
  final_profile <- gsub("\n{3,}", "\n\n", final_profile)
  final_profile <- trimws(final_profile, which = "right")

  if (nzchar(final_profile)) {
    final_profile <- paste0(final_profile, "\n")
  }

  return(final_profile)
}


#' Generate Interactive HTML Report for EOI Applicants
#'
#' Creates a modern, self-contained HTML report with filtering capabilities
#' for reviewing EOI applicants. Perfect for sharing with hiring academics.
#'
#' @param all_applicants_data Data frame containing all applicant records
#' @param output_html_path Path where HTML file should be saved
#' @param title Report title (default: "EOI Applicant Report")
#' @return Invisibly returns the output path on success
#' @export
generate_eoi_html_report <- function(all_applicants_data,
                                     output_html_path,
                                     title = "EOI Applicant Report") {
  if (!is.data.frame(all_applicants_data)) {
    stop("all_applicants_data must be a data frame")
  }

  if (nrow(all_applicants_data) == 0) {
    stop("all_applicants_data is empty")
  }

  # Helper function to safely get value
  get_val <- function(row_list, field_name, default = "") {
    val <- row_list[[field_name]]
    if (is.null(val) || is.na(val) || val == "") {
      return(default)
    }
    return(as.character(val))
  }

  # Generate HTML for each applicant card
  generate_card_html <- function(row_data) {
    rd <- as.list(row_data)

    # Build name
    title_val <- get_val(rd, "title")
    given_name <- get_val(rd, "given_name")
    surname <- get_val(rd, "surname")
    name_parts <- c(title_val, given_name, surname)
    name_parts <- name_parts[nzchar(name_parts)]
    full_name <- paste(name_parts, collapse = " ")

    # Build badges and data attributes for filtering
    badges_html <- ""
    has_phd <- tolower(get_val(rd, "phd_conferred")) == "yes"
    is_returning <- tolower(get_val(rd, "previous_demonstrator")) == "yes"
    is_hdr <- tolower(get_val(rd, "hdr_student")) == "yes"
    is_trained <- tolower(get_val(rd, "completed_training")) == "yes"

    if (has_phd) {
      badges_html <- paste0(badges_html,
        '<span class="badge badge-primary">PhD</span> ')
    }
    if (is_returning) {
      badges_html <- paste0(badges_html,
        '<span class="badge badge-success">Returning</span> ')
    }
    if (is_hdr) {
      badges_html <- paste0(badges_html,
        '<span class="badge badge-warning">HDR Student</span> ')
    }
    if (is_trained) {
      badges_html <- paste0(badges_html,
        '<span class="badge badge-info">Trained</span> ')
    }

    # Get units for filtering
    pref_units <- get_val(rd, "preferred_units")
    units_list <- if (nzchar(pref_units)) {
      trimws(strsplit(pref_units, ",")[[1]])
    } else {
      character(0)
    }
    units_data_attr <- paste(units_list, collapse = ",")
    units_html <- if (length(units_list) > 0) {
      paste(sapply(units_list, function(u) {
        sprintf('<span class="unit-badge">%s</span>', htmltools::htmlEscape(u))
      }), collapse = " ")
    } else {
      "<em>No units specified</em>"
    }

    # Availability summary
    days <- c("monday", "tuesday", "wednesday", "thursday", "friday")
    day_names <- c("Mon", "Tue", "Wed", "Thu", "Fri")
    avail_count <- 0  # Count how many days they're available
    avail_icons <- sapply(seq_along(days), function(i) {
      avail_text <- tolower(trimws(get_val(
        rd, paste0("availability_", days[i]))))

      if (avail_text %in% c("", "n/a", "not available", "unavailable")) {
        return('<span class="avail-no">✗</span>')
      } else if (avail_text == "full day" ||
                 (grepl("am|morning", avail_text) &&
                  grepl("pm|afternoon", avail_text))) {
        avail_count <<- avail_count + 1
        return('<span class="avail-yes">✓</span>')
      } else if (grepl("am|morning", avail_text)) {
        avail_count <<- avail_count + 0.5
        return('<span class="avail-partial">AM</span>')
      } else if (grepl("pm|afternoon", avail_text)) {
        avail_count <<- avail_count + 0.5
        return('<span class="avail-partial">PM</span>')
      } else {
        return('<span class="avail-unknown">?</span>')
      }
    })
    avail_html <- paste(mapply(function(day, icon) {
      sprintf('<div class="avail-day"><small>%s:</small> %s</div>',
              day, icon)
    }, day_names, avail_icons), collapse = "")

    # Contact info
    email <- get_val(rd, "preferred_email")
    phone <- get_val(rd, "preferred_contact")
    contact_html <- ""
    if (nzchar(email)) {
      contact_html <- paste0(contact_html,
        sprintf('<div><strong>Email:</strong> <a href="mailto:%s">%s</a></div>',
                htmltools::htmlEscape(email), htmltools::htmlEscape(email)))
    }
    if (nzchar(phone)) {
      contact_html <- paste0(contact_html,
        sprintf('<div><strong>Phone:</strong> %s</div>',
                htmltools::htmlEscape(phone)))
    }

    # Additional info
    degrees <- get_val(rd, "higher_education_degrees")
    expertise <- get_val(rd, "expertise_area")
    philosophy <- get_val(rd, "teaching_philosophy")

    details_html <- ""
    if (nzchar(degrees)) {
      details_html <- paste0(details_html,
        sprintf('<div><strong>Education:</strong> %s</div>',
                htmltools::htmlEscape(degrees)))
    }
    if (nzchar(expertise)) {
      details_html <- paste0(details_html,
        sprintf('<div><strong>Expertise:</strong> %s</div>',
                htmltools::htmlEscape(expertise)))
    }
    if (nzchar(philosophy)) {
      details_html <- paste0(details_html,
        sprintf('<div class="philosophy"><strong>Teaching Philosophy:</strong> <em>%s</em></div>',
                htmltools::htmlEscape(philosophy)))
    }

    # Build complete card with data attributes for filtering
    sprintf('
<div class="applicant-card"
     data-phd="%s"
     data-returning="%s"
     data-trained="%s"
     data-availability="%.1f">
  <div class="card-header">
    <h3>%s</h3>
    <div class="badges">%s</div>
  </div>
  <div class="card-body">
    <div class="section">
      <h4>Units of Interest</h4>
      <div class="units-list">%s</div>
    </div>
    <div class="section">
      <h4>Availability</h4>
      <div class="availability-grid">%s</div>
    </div>
    <div class="section">
      <h4>Contact</h4>
      %s
    </div>
    <div class="section">
      <h4>Qualifications</h4>
      %s
    </div>
  </div>
</div>
    ',
    tolower(as.character(has_phd)),
    tolower(as.character(is_returning)),
    tolower(as.character(is_trained)),
    avail_count,
    htmltools::htmlEscape(full_name), badges_html,
    units_html, avail_html, contact_html, details_html)
  }

  # Generate all cards
  cards_html <- paste(apply(all_applicants_data, 1, generate_card_html),
                      collapse = "\n")

  # Create complete HTML document
  html_content <- sprintf('
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>%s</title>
  <style>
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body {
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto,
        "Helvetica Neue", Arial, sans-serif;
      background: #f5f5f5;
      padding: 20px;
      line-height: 1.6;
    }
    .container { max-width: 1400px; margin: 0 auto; }
    header {
      background: white;
      padding: 30px;
      border-radius: 8px;
      margin-bottom: 30px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    header h1 {
      color: #333;
      margin-bottom: 20px;
      font-size: 2em;
    }
    .filters {
      display: flex;
      gap: 15px;
      align-items: center;
      flex-wrap: wrap;
    }
    .filter-group { display: flex; flex-direction: column; gap: 5px; }
    .filter-group label {
      font-weight: 600;
      font-size: 0.9em;
      color: #555;
    }
    select, input {
      padding: 10px 15px;
      border: 1px solid #ddd;
      border-radius: 4px;
      font-size: 1em;
      background: white;
    }
    select { min-width: 200px; }
    input[type="search"] { min-width: 300px; }
    .stats {
      margin-top: 15px;
      padding-top: 15px;
      border-top: 1px solid #eee;
      color: #666;
      font-size: 0.9em;
    }
    .applicant-grid {
      display: grid;
      grid-template-columns: repeat(auto-fill, minmax(400px, 1fr));
      gap: 20px;
    }
    .applicant-card {
      background: white;
      border-radius: 8px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      overflow: hidden;
      transition: transform 0.2s, box-shadow 0.2s;
    }
    .applicant-card:hover {
      transform: translateY(-2px);
      box-shadow: 0 4px 8px rgba(0,0,0,0.15);
    }
    .card-header {
      padding: 20px;
      background: linear-gradient(135deg, #667eea 0%%, #764ba2 100%%);
      color: white;
    }
    .card-header h3 {
      margin-bottom: 10px;
      font-size: 1.3em;
    }
    .badges { display: flex; gap: 8px; flex-wrap: wrap; }
    .badge {
      display: inline-block;
      padding: 4px 10px;
      border-radius: 12px;
      font-size: 0.75em;
      font-weight: 600;
      text-transform: uppercase;
      background: rgba(255,255,255,0.2);
    }
    .badge-primary { background: rgba(100,150,255,0.3); }
    .badge-success { background: rgba(100,200,100,0.3); }
    .badge-warning { background: rgba(255,200,100,0.3); }
    .badge-info { background: rgba(100,200,255,0.3); }
    .card-body { padding: 20px; }
    .section { margin-bottom: 20px; }
    .section:last-child { margin-bottom: 0; }
    .section h4 {
      font-size: 0.9em;
      color: #888;
      text-transform: uppercase;
      letter-spacing: 0.5px;
      margin-bottom: 10px;
      font-weight: 600;
    }
    .unit-badge {
      display: inline-block;
      padding: 4px 10px;
      margin: 2px;
      background: #f0f0f0;
      border-radius: 4px;
      font-size: 0.85em;
      font-weight: 500;
    }
    .availability-grid {
      display: flex;
      gap: 10px;
      flex-wrap: wrap;
    }
    .avail-day {
      display: flex;
      align-items: center;
      gap: 5px;
    }
    .avail-yes { color: #28a745; font-weight: bold; }
    .avail-no { color: #dc3545; }
    .avail-partial { color: #ffc107; font-weight: bold; font-size: 0.8em; }
    .avail-unknown { color: #6c757d; }
    .philosophy { margin-top: 10px; font-size: 0.95em; }
    .hidden { display: none !important; }
    a { color: #667eea; text-decoration: none; }
    a:hover { text-decoration: underline; }
  </style>
</head>
<body>
  <div class="container">
    <header>
      <h1>%s</h1>
      <div class="filters">
        <div class="filter-group">
          <label for="search-filter">Search:</label>
          <input type="search" id="search-filter"
                 placeholder="Search names, expertise...">
        </div>
        <div class="filter-group">
          <label for="phd-filter">PhD Status:</label>
          <select id="phd-filter">
            <option value="">All</option>
            <option value="true">PhD Holders Only</option>
            <option value="false">No PhD</option>
          </select>
        </div>
        <div class="filter-group">
          <label for="returning-filter">Experience:</label>
          <select id="returning-filter">
            <option value="">All</option>
            <option value="true">Returning Staff</option>
            <option value="false">New Applicants</option>
          </select>
        </div>
        <div class="filter-group">
          <label for="trained-filter">Training:</label>
          <select id="trained-filter">
            <option value="">All</option>
            <option value="true">Trained</option>
            <option value="false">Not Trained</option>
          </select>
        </div>
        <div class="filter-group">
          <label for="availability-filter">Availability:</label>
          <select id="availability-filter">
            <option value="0">All</option>
            <option value="3">3+ days</option>
            <option value="2">2+ days</option>
            <option value="1">1+ day</option>
          </select>
        </div>
      </div>
      <div class="stats">
        Showing <strong id="visible-count">0</strong> of
        <strong id="total-count">0</strong> applicants
      </div>
    </header>

    <div class="applicant-grid" id="applicant-grid">
      %s
    </div>
  </div>

  <script>
    const cards = document.querySelectorAll(".applicant-card");
    const searchFilter = document.getElementById("search-filter");
    const phdFilter = document.getElementById("phd-filter");
    const returningFilter = document.getElementById("returning-filter");
    const trainedFilter = document.getElementById("trained-filter");
    const availabilityFilter = document.getElementById("availability-filter");
    const visibleCount = document.getElementById("visible-count");
    const totalCount = document.getElementById("total-count");

    totalCount.textContent = cards.length;

    function updateDisplay() {
      const searchText = searchFilter.value.toLowerCase();
      const phdValue = phdFilter.value;
      const returningValue = returningFilter.value;
      const trainedValue = trainedFilter.value;
      const minAvailability = parseFloat(availabilityFilter.value);
      let visible = 0;

      cards.forEach(card => {
        const cardText = card.textContent.toLowerCase();
        const cardPhd = card.dataset.phd;
        const cardReturning = card.dataset.returning;
        const cardTrained = card.dataset.trained;
        const cardAvailability = parseFloat(card.dataset.availability);

        const searchMatch = !searchText || cardText.includes(searchText);
        const phdMatch = !phdValue || cardPhd === phdValue;
        const returningMatch = !returningValue || cardReturning === returningValue;
        const trainedMatch = !trainedValue || cardTrained === trainedValue;
        const availabilityMatch = cardAvailability >= minAvailability;

        if (searchMatch && phdMatch && returningMatch && trainedMatch && availabilityMatch) {
          card.classList.remove("hidden");
          visible++;
        } else {
          card.classList.add("hidden");
        }
      });

      visibleCount.textContent = visible;
    }

    searchFilter.addEventListener("input", updateDisplay);
    phdFilter.addEventListener("change", updateDisplay);
    returningFilter.addEventListener("change", updateDisplay);
    trainedFilter.addEventListener("change", updateDisplay);
    availabilityFilter.addEventListener("change", updateDisplay);

    // Initial display
    updateDisplay();
  </script>
</body>
</html>
  ', htmltools::htmlEscape(title), htmltools::htmlEscape(title),
  cards_html)

  # Write to file
  tryCatch({
    writeLines(html_content, output_html_path, useBytes = TRUE)
    message("HTML report generated: ", output_html_path)
  }, error = function(e) {
    stop(paste("Failed to write HTML report:", e$message))
  })

  invisible(output_html_path)
}

#' Render Modern EOI Profiles to PDF
#'
#' Creates a professional PDF report of EOI applicants using the modern
#' profile format. Requires Quarto CLI to be installed.
#'
#' @param all_applicants_data Data frame where each row is an applicant
#' @param output_pdf_path Path where PDF should be saved
#' @param title Document title (default: "EOI Applicant Profiles")
#' @return Invisibly returns the output path on success
#' @export
#' @importFrom quarto quarto_render quarto_path
render_eoi_profiles_to_pdf <- function(all_applicants_data,
                                       output_pdf_path,
                                       title = "EOI Applicant Profiles") {
  # Check for Quarto CLI
  quarto_bin <- tryCatch({
    quarto::quarto_path()
  }, error = function(e) NULL)

  if (is.null(quarto_bin)) {
    stop(
      "Quarto CLI not found. Install from https://quarto.org/docs/get-started/\n",
      "Ensure it's in PATH or set QUARTO_PATH environment variable."
    )
  }

  if (!is.data.frame(all_applicants_data)) {
    stop("all_applicants_data must be a data frame")
  }

  # Prepare output directory
  target_dir <- dirname(output_pdf_path)
  target_filename <- basename(output_pdf_path)

  if (target_dir != "." && !dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(target_dir)) {
      stop("Failed to create output directory: ", target_dir)
    }
  }

  abs_target_dir <- normalizePath(target_dir, mustWork = TRUE)

  # Generate markdown content
  if (nrow(all_applicants_data) == 0) {
    warning("No applicant data. Generating empty PDF.")
    markdown_body <- "No applicant data to display.\n"
  } else {
    # Generate profile for each applicant
    profiles <- lapply(1:nrow(all_applicants_data), function(i) {
      create_eoi_profile_modern(all_applicants_data[i, , drop = FALSE])
    })

    # Join with page breaks
    markdown_body <- paste(profiles, collapse = "\n{{< pagebreak >}}\n\n")
  }

  # Create temporary .qmd file
  temp_qmd <- tempfile(
    pattern = "eoi_modern_",
    tmpdir = abs_target_dir,
    fileext = ".qmd"
  )
  on.exit(unlink(temp_qmd, force = TRUE), add = TRUE)

  # Prepare Quarto document with modern styling
  qmd_content <- paste0(
    "---\n",
    "title: \"", title, "\"\n",
    "format:\n",
    "  typst:\n",
    "    toc: true\n",
    "    toc-depth: 1\n",
    "    number-sections: false\n",
    "    mainfont: \"Helvetica Neue\"\n",
    "    fontsize: 9pt\n",
    "    margin:\n",
    "      x: 1.5cm\n",
    "      y: 1.5cm\n",
    "---\n\n",
    "{{< pagebreak >}}\n\n",
    markdown_body
  )

  # Write QMD file
  tryCatch({
    writeLines(qmd_content, temp_qmd, useBytes = TRUE)
  }, error = function(e) {
    stop("Failed to write temp QMD file: ", e$message)
  })

  # Change to target directory for rendering
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(abs_target_dir)

  # Render to PDF
  render_result <- tryCatch({
    quarto::quarto_render(
      input = basename(temp_qmd),
      output_file = target_filename,
      as_job = FALSE,
      quiet = FALSE
    )

    if (!file.exists(target_filename)) {
      stop("Quarto succeeded but PDF not found at: ", target_filename)
    }

    TRUE
  }, error = function(e) {
    stop(
      "Failed to render PDF.\n",
      "Directory: ", abs_target_dir, "\n",
      "Input: ", basename(temp_qmd), "\n",
      "Output: ", target_filename, "\n",
      "Error: ", e$message
    )
  })

  if (!render_result) {
    stop("PDF rendering failed")
  }

  message("PDF generated: ", output_pdf_path)
  invisible(output_pdf_path)
}
