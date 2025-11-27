#' Generate Markdown Summary for a Unit of Study
#'
#' This function takes the processed EOI data and a specific unit name
#' to generate a markdown-formatted summary for that unit. The summary
#' includes the total number of applicants, the number of applicants
#' with a PhD conferred, and the number of returning educators who have
#' taught the unit before.
#'
#' @param elist A list object containing processed EOI data, typically the
#'   output of \code{\link{process_eoi_data}}. Each element of the list should
#'   be a data frame corresponding to a unit of study, with applicants as rows.
#' @param unit_name A character string representing the full name of the unit
#'   of study for which to generate the summary (e.g.,
#'   "BIOL1009 From Molecules to Cells and Organisms").
#'
#' @return A character string containing the markdown-formatted summary.
#'   Returns an error message if the unit_name is not found in elist.
#' @export
#' @importFrom stringr str_extract str_detect regex
#'
#' @examples
#' \dontrun{
#' # Assuming 'eoi_data' is loaded and 'processed_elist' is generated:
#' # eoi_raw <- parse_eoi("path/to/your/EOI_data.csv")
#' # preferred_units_extracted <- eoi_extract(eoi_raw)
#' # processed_elist <- process_eoi_data(eoi_raw, preferred_units_extracted)
#' # summary_md <- generate_unit_summary(processed_elist,
#' #  "BIOL1009 From Molecules to Cells and Organisms")
#' # cat(summary_md)
#' }
generate_unit_summary <- function(elist, unit_name) {
  if (!unit_name %in% names(elist)) {
    return(sprintf("Error: Unit '%s' not found in the provided EOI data.", unit_name))
  }

  selected_unit <- elist[[unit_name]]

  # Calculate number of applicants
  applicants <- nrow(selected_unit)

  # Calculate applicants with PhD conferred
  # Default value is 0. If 'phd_conferred' column exists, sum "Yes" values.
  # Otherwise, a warning is issued.
  total_phd_conferred <- 0
  if ("phd_conferred" %in% names(selected_unit)) {
    total_phd_conferred <- sum(selected_unit[["phd_conferred"]] == "Yes", na.rm = TRUE)
  } else {
    warning(sprintf("Column 'phd_conferred' not found for unit '%s'. Assuming 0 PhDs conferred.", unit_name))
  }

  # Extract unit code from unit name
  # This is used for calculating returning educators.
  uos_code <- stringr::str_extract(unit_name, "[A-Z]{4}\\d{4}")

  # Calculate applicants that have taught the unit before
  # Default value is 0.
  # If UoS code cannot be extracted, a warning is issued and returning_educators remains 0.
  # Else (UoS code extracted):
  #   If 'previous_units' column exists, calculate matches.
  #   Else (if 'previous_units' column is missing), a warning is issued and returning_educators remains 0.
  returning_educators <- 0
  if (is.null(uos_code)) {
    warning(sprintf("Could not extract UoS code from unit name: '%s'. Cannot accurately calculate returning educators.", unit_name))
  } else { # UoS code is NOT NULL
    if ("previous_units" %in% names(selected_unit)) {
      matches <- stringr::str_detect(selected_unit[["previous_units"]], stringr::regex(uos_code, ignore_case = TRUE))
      returning_educators <- sum(matches, na.rm = TRUE)
    } else { # 'previous_units' column is MISSING
      warning(sprintf("Column 'previous_units' not found for unit '%s'. Assuming 0 returning educators.", unit_name))
    }
  }

  # Calculate applicants who have previously demonstrated at SOLES
  soles_returning_demonstrators_count <- 0
  if ("previous_demonstrator" %in% names(selected_unit)) {
    soles_returning_demonstrators_count <- sum(selected_unit[["previous_demonstrator"]] == "Yes", na.rm = TRUE)
  } else {
    warning(sprintf("Column 'previous_demonstrator' not found for unit '%s'. Assuming 0 previous SOLES demonstrators.", unit_name))
  }

  # Construct the concise summary string
  heading_line <- sprintf("### %s", unit_name)

  # Sentence 1: Applicants
  applicants_noun <- if (applicants == 1) "application" else "applications"
  applicants_sentence <- sprintf("For this unit, there are %d %s.", applicants, applicants_noun)

  # Sentence 2: PhD Holders
  if (total_phd_conferred == 0) {
    phd_sentence <- "Among the applicants, none hold a PhD."
  } else {
    phd_verb <- if (total_phd_conferred == 1) "holds" else "hold"
    phd_percentage_string <- ""
    if (applicants > 0) {
      phd_percentage <- (total_phd_conferred / applicants * 100)
      phd_percentage_string <- sprintf("(around %.0f%%) ", phd_percentage)
    } else { # total_phd_conferred > 0 but applicants == 0
      phd_percentage_string <- "(PhD % N/A) "
    }
    phd_sentence <- sprintf(
      "Among the applicants, %d %s%s a PhD.",
      total_phd_conferred,
      phd_percentage_string,
      phd_verb
    )
  }

  # Sentence 3: Experience
  # SOLES Experience Part
  soles_experience_phrase <- ""
  if (soles_returning_demonstrators_count == 0) {
    soles_experience_phrase <- "none have prior experience demonstrating at SOLES"
  } else {
    soles_verb <- if (soles_returning_demonstrators_count == 1) "has" else "have"
    soles_percentage_string <- ""
    if (applicants > 0) {
      soles_percentage <- (soles_returning_demonstrators_count / applicants * 100)
      soles_percentage_string <- sprintf("(%.0f%%) ", soles_percentage)
    } else { # soles_returning_demonstrators_count > 0 but applicants == 0
      soles_percentage_string <- "(% N/A) "
    }
    soles_experience_phrase <- sprintf(
      "%d %s%s prior experience demonstrating at SOLES",
      soles_returning_demonstrators_count,
      soles_percentage_string,
      soles_verb
    )
  }

  # Unit Taught Part
  unit_taught_phrase <- ""
  if (returning_educators == 0) {
    unit_taught_phrase <- "none have taught this particular unit before"
  } else {
    unit_verb <- if (returning_educators == 1) "has" else "have"
    unit_taught_phrase <- sprintf(
      "%d %s taught this particular unit before",
      returning_educators,
      unit_verb
    )
  }

  experience_sentence <- sprintf(
    "In terms of specific experience, %s and %s.",
    soles_experience_phrase,
    unit_taught_phrase
  )


  # Combine all parts into a paragraph
  summary_paragraph <- paste(applicants_sentence, phd_sentence, experience_sentence, sep = " ")

  full_summary <- paste(heading_line, summary_paragraph, sep = "\n")

  return(full_summary)
}
