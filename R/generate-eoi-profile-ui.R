#' Create Concise HTML Profile Card for EOI Applicant
#'
#' Generates a modern, compact HTML card view for displaying applicant profiles
#' in the Shiny app. Uses bslib card components for a clean, professional look.
#'
#' @param applicant_data A list or single-row data frame containing EOI data
#' @return HTML string for rendering in Shiny UI
#' @importFrom htmltools tags div span h3 h4 HTML
#' @export
create_eoi_profile_ui <- function(applicant_data) {
  # Ensure applicant_data is a list
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

  # Helper to safely get values
  get_val <- function(field_name, default_val = NULL) {
    val <- ad[[field_name]]
    .get_val_or_default(val, default_val)
  }

  # Helper for availability icon
  avail_icon <- function(avail_text) {
    norm_text <- tolower(trimws(avail_text %||% ""))

    if (norm_text %in% c("", "n/a", "not available", "unavailable")) {
      return(list(am = "×", pm = "×", class = "text-danger"))
    }

    is_full <- norm_text == "full day"
    has_am <- grepl("am|morning", norm_text)
    has_pm <- grepl("pm|afternoon", norm_text)

    if (is_full || (has_am && has_pm)) {
      return(list(am = "✓", pm = "✓", class = "text-success"))
    } else if (has_am) {
      return(list(am = "✓", pm = "×", class = "text-warning"))
    } else if (has_pm) {
      return(list(am = "×", pm = "✓", class = "text-warning"))
    } else {
      return(list(am = "?", pm = "?", class = "text-muted"))
    }
  }

  # Build name
  title_val <- get_val("title")
  given_name_val <- get_val("given_name")
  surname_val <- get_val("surname")
  name_parts <- c(title_val, given_name_val, surname_val)
  name_parts <- name_parts[!sapply(name_parts, is.null)]
  full_name <- if (length(name_parts) > 0) {
    paste(name_parts, collapse = " ")
  } else {
    "Applicant"
  }

  # Badges for key qualifications
  badges <- character()

  if (!is.null(get_val("phd_conferred")) &&
    tolower(get_val("phd_conferred")) == "yes") {
    badges <- c(badges, '<span class="badge bg-primary me-1">PhD</span>')
  }

  if (!is.null(get_val("previous_demonstrator")) &&
    tolower(get_val("previous_demonstrator")) == "yes") {
    badges <- c(
      badges,
      '<span class="badge bg-success me-1">Returning Staff</span>'
    )
  }

  if (!is.null(get_val("completed_training")) &&
    tolower(get_val("completed_training")) == "yes") {
    badges <- c(badges, '<span class="badge bg-info me-1">Trained</span>')
  }

  if (!is.null(get_val("worked_at_usyd")) &&
    tolower(get_val("worked_at_usyd")) == "yes") {
    badges <- c(badges, '<span class="badge bg-secondary me-1">USYD Staff</span>')
  }

  badges_html <- if (length(badges) > 0) {
    paste0('<div class="mb-3">', paste(badges, collapse = ""), '</div>')
  } else {
    ""
  }

  # Contact info (compact)
  contact_parts <- character()
  email_val <- get_val("preferred_email")
  phone_val <- get_val("preferred_contact")

  if (!is.null(email_val)) {
    contact_parts <- c(
      contact_parts,
      sprintf(
        '<div><small class="text-muted">✉</small> <a href="mailto:%s">%s</a></div>',
        email_val, email_val
      )
    )
  }

  if (!is.null(phone_val)) {
    contact_parts <- c(
      contact_parts,
      sprintf(
        '<div><small class="text-muted">☎</small> %s</div>',
        phone_val
      )
    )
  }

  contact_html <- if (length(contact_parts) > 0) {
    paste0(
      '<div class="mb-3">',
      paste(contact_parts, collapse = "\n"),
      '</div>'
    )
  } else {
    ""
  }

  # Preferred units (compact list)
  pref_units_val <- get_val("preferred_units")
  units_html <- if (!is.null(pref_units_val)) {
    units_list <- strsplit(pref_units_val, ",")[[1]]
    units_list <- trimws(units_list)
    units_list <- units_list[nzchar(units_list)]

    if (length(units_list) > 0) {
      units_badges <- sapply(units_list, function(u) {
        sprintf('<span class="badge bg-light text-dark me-1 mb-1">%s</span>', u)
      })
      paste0(
        '<div class="mb-3">',
        '<h6 class="text-muted mb-2">Preferred Units</h6>',
        paste(units_badges, collapse = ""),
        '</div>'
      )
    } else {
      ""
    }
  } else {
    ""
  }

  # Compact availability table
  days <- c("Mon", "Tue", "Wed", "Thu", "Fri")
  day_fields <- paste0(
    "availability_",
    c("monday", "tuesday", "wednesday", "thursday", "friday")
  )

  avail_rows <- sapply(seq_along(days), function(i) {
    avail_val <- get_val(day_fields[i])
    icons <- avail_icon(avail_val)
    sprintf(
      '<tr><td class="text-muted small">%s</td><td class="text-center %s">%s</td><td class="text-center %s">%s</td></tr>',
      days[i],
      icons$class, icons$am,
      icons$class, icons$pm
    )
  })

  avail_html <- paste0(
    '<div class="mb-3">',
    '<h6 class="text-muted mb-2">Availability</h6>',
    '<table class="table table-sm table-borderless mb-0">',
    '<thead><tr><th></th><th class="text-center small">AM</th><th class="text-center small">PM</th></tr></thead>',
    '<tbody>',
    paste(avail_rows, collapse = "\n"),
    '</tbody></table>',
    '</div>'
  )

  # Background (collapsible section)
  background_parts <- character()

  expertise_val <- get_val("expertise_area")
  if (!is.null(expertise_val)) {
    background_parts <- c(
      background_parts,
      sprintf('<p><strong>Expertise:</strong> %s</p>', expertise_val)
    )
  }

  degrees_val <- get_val("higher_education_degrees")
  if (!is.null(degrees_val)) {
    background_parts <- c(
      background_parts,
      sprintf('<p><strong>Education:</strong> %s</p>', degrees_val)
    )
  }

  prev_units_val <- get_val("previous_units")
  if (!is.null(prev_units_val)) {
    prev_units_clean <- gsub("\n", " ", prev_units_val)
    prev_units_clean <- trimws(gsub("\\s+", " ", prev_units_clean))
    background_parts <- c(
      background_parts,
      sprintf('<p><strong>Previous Units:</strong> %s</p>', prev_units_clean)
    )
  }

  philosophy_val <- get_val("teaching_philosophy")
  if (!is.null(philosophy_val)) {
    background_parts <- c(
      background_parts,
      sprintf(
        '<p><strong>Teaching Philosophy:</strong> <em>%s</em></p>',
        philosophy_val
      )
    )
  }

  experience_val <- get_val("experience_benefit")
  if (!is.null(experience_val)) {
    background_parts <- c(
      background_parts,
      sprintf(
        '<p><strong>How they can benefit SOLES:</strong> <em>%s</em></p>',
        experience_val
      )
    )
  }

  background_html <- if (length(background_parts) > 0) {
    paste0(
      '<div class="accordion mb-3" id="backgroundAccordion">',
      '<div class="accordion-item">',
      '<h2 class="accordion-header">',
      '<button class="accordion-button collapsed" type="button" ',
      'data-bs-toggle="collapse" data-bs-target="#collapseBackground">',
      'Additional Background',
      '</button></h2>',
      '<div id="collapseBackground" class="accordion-collapse collapse">',
      '<div class="accordion-body small">',
      paste(background_parts, collapse = "\n"),
      '</div></div></div></div>'
    )
  } else {
    ""
  }

  # Additional details (compact)
  details_parts <- character()

  lead_interest_val <- get_val("lead_demonstrator_interest")
  if (!is.null(lead_interest_val) && tolower(lead_interest_val) != "no") {
    lead_other_val <- get_val("lead_demonstrator_other")
    lead_text <- if (!is.null(lead_other_val)) {
      sprintf("%s (%s)", lead_interest_val, lead_other_val)
    } else {
      lead_interest_val
    }
    details_parts <- c(
      details_parts,
      sprintf(
        '<div class="text-muted small"><strong>Lead Interest:</strong> %s</div>',
        lead_text
      )
    )
  }

  blockout_val <- get_val("blockout_dates")
  if (!is.null(blockout_val)) {
    details_parts <- c(
      details_parts,
      sprintf(
        '<div class="text-muted small"><strong>Blockout Dates:</strong> %s</div>',
        blockout_val
      )
    )
  }

  details_html <- if (length(details_parts) > 0) {
    paste0(
      '<div class="mt-3 pt-3 border-top">',
      paste(details_parts, collapse = "\n"),
      '</div>'
    )
  } else {
    ""
  }

  # Combine everything into a card
  html_output <- paste0(
    '<div class="card shadow-sm">',
    '<div class="card-body">',
    '<h4 class="card-title mb-3">', full_name, '</h4>',
    badges_html,
    contact_html,
    units_html,
    avail_html,
    background_html,
    details_html,
    '</div>',
    '</div>'
  )

  return(htmltools::HTML(html_output))
}
