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

    # Get qualification data
    has_phd <- tolower(get_val(rd, "phd_conferred")) == "yes"
    is_returning <- tolower(get_val(rd, "previous_demonstrator")) == "yes"
    is_hdr <- tolower(get_val(rd, "hdr_student")) == "yes"
    is_trained <- tolower(get_val(rd, "completed_training")) == "yes"

    # Get units
    pref_units <- get_val(rd, "preferred_units")
    units_str <- if (nzchar(pref_units)) {
      paste(trimws(strsplit(pref_units, ",")[[1]]), collapse = ", ")
    } else {
      "None specified"
    }

    # Availability summary
    days <- c("monday", "tuesday", "wednesday", "thursday", "friday")
    day_names <- c("Mon", "Tue", "Wed", "Thu", "Fri")
    avail_count <- 0
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
      sprintf('<div class="avail-day"><span>%s</span>%s</div>', day, icon)
    }, day_names, avail_icons), collapse = "")

    # Contact and additional info
    email <- get_val(rd, "preferred_email")
    phone <- get_val(rd, "preferred_contact")
    degrees <- get_val(rd, "higher_education_degrees")
    expertise <- get_val(rd, "expertise_area")
    philosophy <- get_val(rd, "teaching_philosophy")

    # Build complete card with structured layout
    sprintf('
<div class="applicant-card"
     data-phd="%s"
     data-returning="%s"
     data-trained="%s"
     data-hdr="%s"
     data-availability="%.1f"
     data-surname="%s"
     data-given-name="%s">
  <h3>%s</h3>
  <div class="card-grid">
    <div class="field">
      <label>PhD:</label>
      <span class="value">%s</span>
    </div>
    <div class="field">
      <label>Returning:</label>
      <span class="value">%s</span>
    </div>
    <div class="field">
      <label>HDR Student:</label>
      <span class="value">%s</span>
    </div>
    <div class="field">
      <label>Trained:</label>
      <span class="value">%s</span>
    </div>
    <div class="field full-width">
      <label>Units of Interest:</label>
      <span class="value">%s</span>
    </div>
    <div class="field full-width">
      <label>Availability:</label>
      <div class="availability-grid">%s</div>
    </div>
    <div class="field full-width">
      <label>Email:</label>
      <span class="value">%s</span>
    </div>
    <div class="field full-width">
      <label>Phone:</label>
      <span class="value">%s</span>
    </div>
    <div class="field full-width">
      <label>Education:</label>
      <span class="value">%s</span>
    </div>
    <div class="field full-width">
      <label>Expertise:</label>
      <span class="value">%s</span>
    </div>
    <div class="field full-width">
      <label>Teaching Philosophy:</label>
      <span class="value philosophy">%s</span>
    </div>
  </div>
</div>
    ',
    tolower(as.character(has_phd)),
    tolower(as.character(is_returning)),
    tolower(as.character(is_trained)),
    tolower(as.character(is_hdr)),
    avail_count,
    htmltools::htmlEscape(surname),
    htmltools::htmlEscape(given_name),
    htmltools::htmlEscape(full_name),
    if (has_phd) "Yes" else "No",
    if (is_returning) "Yes" else "No",
    if (is_hdr) "Yes" else "No",
    if (is_trained) "Yes" else "No",
    htmltools::htmlEscape(units_str),
    avail_html,
    if (nzchar(email))
      sprintf('<a href="mailto:%s">%s</a>',
        htmltools::htmlEscape(email), htmltools::htmlEscape(email))
    else "Not provided",
    if (nzchar(phone)) htmltools::htmlEscape(phone) else "Not provided",
    if (nzchar(degrees)) htmltools::htmlEscape(degrees) else "Not provided",
    if (nzchar(expertise)) htmltools::htmlEscape(expertise) else "Not provided",
    if (nzchar(philosophy)) htmltools::htmlEscape(philosophy) else "Not provided")
  }

  # Generate all cards - vectorized approach for better performance
  n_rows <- nrow(all_applicants_data)
  cards_list <- vector("list", n_rows)
  for (i in seq_len(n_rows)) {
    cards_list[[i]] <- generate_card_html(all_applicants_data[i, ])
  }
  cards_html <- paste(cards_list, collapse = "\n")

  # Create complete HTML document - build in parts to avoid sprintf length limit
html_head <- sprintf('
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>%s</title>
  <style>
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body {
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
      background: #f5f5f5;
      line-height: 1.5;
      display: flex;
      height: 100vh;
      overflow: hidden;
    }
    .sidebar {
      width: 280px;
      background: white;
      border-right: 1px solid #ddd;
      display: flex;
      flex-direction: column;
      overflow: hidden;
    }
    .sidebar-header {
      padding: 20px;
      border-bottom: 1px solid #e0e0e0;
      background: #f8f9fa;
    }
    .sidebar-header h2 {
      font-size: 1.1em;
      color: #333;
      margin-bottom: 10px;
    }
    .select-all-container {
      display: flex;
      align-items: center;
      gap: 8px;
      padding: 10px 0;
      font-size: 0.9em;
    }
    .select-all-container input[type="checkbox"] {
      width: 16px;
      height: 16px;
      cursor: pointer;
    }
    .applicant-list {
      flex: 1;
      overflow-y: auto;
      padding: 10px;
    }
    .applicant-item {
      padding: 10px 15px;
      border-bottom: 1px solid #f0f0f0;
      cursor: pointer;
      display: flex;
      align-items: center;
      gap: 10px;
      transition: background 0.2s;
    }
    .applicant-item:hover {
      background: #f8f9fa;
    }
    .applicant-item input[type="checkbox"] {
      width: 16px;
      height: 16px;
      cursor: pointer;
    }
    .applicant-name {
      flex: 1;
      font-size: 0.9em;
      color: #333;
    }
    .main-content {
      flex: 1;
      display: flex;
      flex-direction: column;
      overflow: hidden;
    }
    .top-bar {
      background: white;
      border-bottom: 1px solid #ddd;
      padding: 20px;
    }
    .top-bar h1 {
      font-size: 1.5em;
      color: #333;
      margin-bottom: 15px;
    }
    .filters {
      display: flex;
      gap: 15px;
      align-items: center;
      flex-wrap: wrap;
      margin-bottom: 10px;
    }
    .filter-group {
      display: flex;
      align-items: center;
      gap: 8px;
    }
    .filter-group label {
      font-size: 0.9em;
      color: #555;
      font-weight: 500;
    }
    select {
      padding: 6px 10px;
      border: 1px solid #ddd;
      border-radius: 4px;
      font-size: 0.9em;
      background: white;
      cursor: pointer;
    }
    select:focus {
      outline: none;
      border-color: #007bff;
    }
    .stats {
      font-size: 0.85em;
      color: #666;
      margin-top: 10px;
    }
    .cards-container {
      flex: 1;
      overflow-y: auto;
      padding: 20px;
      display: flex;
      flex-direction: column;
      gap: 15px;
    }
    .applicant-card {
      background: white;
      border: 1px solid #e0e0e0;
      border-radius: 6px;
      padding: 20px;
      transition: box-shadow 0.2s;
    }
    .applicant-card:hover {
      box-shadow: 0 2px 8px rgba(0,0,0,0.1);
    }
    .applicant-card.hidden {
      display: none;
    }
    .applicant-card h3 {
      color: #333;
      font-size: 1.3em;
      margin-bottom: 15px;
      padding-bottom: 10px;
      border-bottom: 2px solid #f0f0f0;
    }
    .card-grid {
      display: grid;
      grid-template-columns: repeat(2, 1fr);
      gap: 12px;
    }
    .field {
      display: flex;
      flex-direction: column;
      gap: 4px;
    }
    .field.full-width {
      grid-column: 1 / -1;
    }
    .field label {
      font-size: 0.85em;
      font-weight: 600;
      color: #666;
      text-transform: uppercase;
      letter-spacing: 0.5px;
    }
    .field .value {
      font-size: 0.95em;
      color: #333;
    }
    .field .value.philosophy {
      font-style: italic;
      color: #555;
      line-height: 1.6;
    }
    .availability-grid {
      display: flex;
      gap: 8px;
      margin-top: 5px;
    }
    .avail-day {
      display: flex;
      flex-direction: column;
      align-items: center;
      gap: 2px;
      padding: 6px;
      background: #f8f9fa;
      border-radius: 4px;
      font-size: 0.85em;
    }
    .avail-day span {
      font-weight: 600;
      color: #666;
    }
    .avail-yes { color: #28a745; font-size: 1.1em; font-weight: bold; }
    .avail-no { color: #dc3545; font-size: 1.1em; font-weight: bold; }
    .avail-partial { color: #ffc107; font-size: 0.75em; font-weight: bold; }
    .avail-unknown { color: #6c757d; font-size: 0.9em; }
  </style>
</head>
<body>
  <div class="sidebar">
    <div class="sidebar-header">
      <h2>Applicants</h2>
      <div class="select-all-container">
        <input type="checkbox" id="select-all" checked>
        <label for="select-all">Select All</label>
      </div>
    </div>
    <div class="applicant-list" id="sidebar-list">
    </div>
  </div>

  <div class="main-content">
    <div class="top-bar">
      <h1>%s</h1>
      <div class="filters">
        <div class="filter-group">
          <label for="sort-order">Sort:</label>
          <select id="sort-order">
            <option value="surname-asc">Surname (A-Z)</option>
            <option value="surname-desc">Surname (Z-A)</option>
            <option value="given-asc">First Name (A-Z)</option>
            <option value="given-desc">First Name (Z-A)</option>
          </select>
        </div>
        <div class="filter-group">
          <label for="phd-filter">PhD:</label>
          <select id="phd-filter">
            <option value="">All</option>
            <option value="true">Yes</option>
            <option value="false">No</option>
          </select>
        </div>
        <div class="filter-group">
          <label for="returning-filter">Returning:</label>
          <select id="returning-filter">
            <option value="">All</option>
            <option value="true">Yes</option>
            <option value="false">No</option>
          </select>
        </div>
        <div class="filter-group">
          <label for="trained-filter">Trained:</label>
          <select id="trained-filter">
            <option value="">All</option>
            <option value="true">Yes</option>
            <option value="false">No</option>
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
    </div>

    <div class="cards-container" id="cards-container">
', htmltools::htmlEscape(title), htmltools::htmlEscape(title))


html_footer <- '
    </div>
  </div>

  <script>
    const cardsContainer = document.getElementById("cards-container");
    const sidebarList = document.getElementById("sidebar-list");
    const selectAllCheckbox = document.getElementById("select-all");
    const sortOrder = document.getElementById("sort-order");
    const phdFilter = document.getElementById("phd-filter");
    const returningFilter = document.getElementById("returning-filter");
    const trainedFilter = document.getElementById("trained-filter");
    const availabilityFilter = document.getElementById("availability-filter");
    const visibleCount = document.getElementById("visible-count");
    const totalCount = document.getElementById("total-count");

    const cards = Array.from(document.querySelectorAll(".applicant-card"));
    const checkboxes = [];

    totalCount.textContent = cards.length;

    // Populate sidebar with applicant checkboxes
    cards.forEach((card, idx) => {
      const name = card.querySelector("h3").textContent;
      const surname = card.dataset.surname;
      const givenName = card.dataset.givenName;

      const item = document.createElement("div");
      item.className = "applicant-item";

      const checkbox = document.createElement("input");
      checkbox.type = "checkbox";
      checkbox.id = `applicant-${idx}`;
      checkbox.checked = true;
      checkbox.dataset.index = idx;

      const label = document.createElement("label");
      label.htmlFor = `applicant-${idx}`;
      label.className = "applicant-name";
      label.textContent = name;
      label.style.cursor = "pointer";

      item.appendChild(checkbox);
      item.appendChild(label);
      sidebarList.appendChild(item);

      checkboxes.push(checkbox);

      // Click on item also toggles checkbox
      item.addEventListener("click", (e) => {
        if (e.target !== checkbox) {
          checkbox.checked = !checkbox.checked;
          updateDisplay();
        }
      });

      checkbox.addEventListener("change", updateDisplay);
    });

    // Select all functionality
    selectAllCheckbox.addEventListener("change", () => {
      const isChecked = selectAllCheckbox.checked;
      checkboxes.forEach(cb => {
        cb.checked = isChecked;
      });
      updateDisplay();
    });

    // Update select-all state when individual checkboxes change
    function updateSelectAllState() {
      const allChecked = checkboxes.every(cb => cb.checked);
      const noneChecked = checkboxes.every(cb => !cb.checked);
      selectAllCheckbox.checked = allChecked;
      selectAllCheckbox.indeterminate = !allChecked && !noneChecked;
    }

    function sortCards() {
      const sortValue = sortOrder.value;
      const sortedCards = [...cards].sort((a, b) => {
        const surnameA = a.dataset.surname.toLowerCase();
        const surnameB = b.dataset.surname.toLowerCase();
        const givenA = a.dataset.givenName.toLowerCase();
        const givenB = b.dataset.givenName.toLowerCase();

        switch(sortValue) {
          case "surname-asc":
            return surnameA.localeCompare(surnameB) ||
              givenA.localeCompare(givenB);
          case "surname-desc":
            return surnameB.localeCompare(surnameA) ||
              givenB.localeCompare(givenA);
          case "given-asc":
            return givenA.localeCompare(givenB) ||
              surnameA.localeCompare(surnameB);
          case "given-desc":
            return givenB.localeCompare(givenA) ||
              surnameB.localeCompare(surnameA);
          default:
            return 0;
        }
      });

      // Re-append cards in sorted order
      sortedCards.forEach(card => cardsContainer.appendChild(card));
    }

    function updateDisplay() {
      updateSelectAllState();

      const phdValue = phdFilter.value;
      const returningValue = returningFilter.value;
      const trainedValue = trainedFilter.value;
      const minAvailability = parseFloat(availabilityFilter.value);
      let visible = 0;

      cards.forEach((card, idx) => {
        const checkbox = checkboxes[idx];
        const selectedByUser = checkbox.checked;

        const cardPhd = card.dataset.phd;
        const cardReturning = card.dataset.returning;
        const cardTrained = card.dataset.trained;
        const cardAvailability = parseFloat(card.dataset.availability);

        const phdMatch = !phdValue || cardPhd === phdValue;
        const returningMatch = !returningValue || cardReturning === returningValue;
        const trainedMatch = !trainedValue || cardTrained === trainedValue;
        const availabilityMatch = cardAvailability >= minAvailability;

        if (selectedByUser && phdMatch && returningMatch &&
            trainedMatch && availabilityMatch) {
          card.classList.remove("hidden");
          visible++;
        } else {
          card.classList.add("hidden");
        }
      });

      visibleCount.textContent = visible;
    }

    sortOrder.addEventListener("change", () => {
      sortCards();
      updateDisplay();
    });
    phdFilter.addEventListener("change", updateDisplay);
    returningFilter.addEventListener("change", updateDisplay);
    trainedFilter.addEventListener("change", updateDisplay);
    availabilityFilter.addEventListener("change", updateDisplay);

    // Initial display
    sortCards();
    updateDisplay();
  </script>
</body>
</html>'


  # Combine HTML parts
  html_content <- paste0(
    html_head,
    cards_html,
    html_footer
  )

  # Write to file - use writeBin for better performance with large files
  tryCatch({
    # Convert to raw bytes for faster writing
    con <- file(output_html_path, "wb")
    on.exit(close(con), add = TRUE)
    writeBin(charToRaw(html_content), con)
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
    # Generate profile for each applicant - preallocate for performance
    n_applicants <- nrow(all_applicants_data)
    profiles <- vector("list", n_applicants)
    for (i in seq_len(n_applicants)) {
      profiles[[i]] <- create_eoi_profile_modern(
        all_applicants_data[i, , drop = FALSE])
    }

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

  # Render to PDF - use quiet mode for better performance
  render_result <- tryCatch({
    quarto::quarto_render(
      input = basename(temp_qmd),
      output_file = target_filename,
      as_job = FALSE,
      quiet = TRUE  # Suppress output for faster rendering
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
