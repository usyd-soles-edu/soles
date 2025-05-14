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

  # Helper function for formatting counts with correct grammar
  # This function is defined within generate_unit_summary for local use.
  format_count_phrase <- function(count, singular_noun, plural_noun, zero_prefix = "no") {
    if (count == 0) {
      return(paste(zero_prefix, plural_noun))
    } else if (count == 1) {
      return(paste("1", singular_noun))
    } else {
      return(paste(count, plural_noun))
    }
  }

  # Construct the concise summary string
  heading_line <- sprintf("### %s", unit_name)

  # 1. Applicants phrase
  applicants_phrase <- format_count_phrase(applicants, "applicant", "applicants")
  applicants_sentence <- paste0(toupper(substr(applicants_phrase, 1, 1)), substr(applicants_phrase, 2, nchar(applicants_phrase)), ".")

  # 2. PhD holders phrase
  phd_holders_phrase <- format_count_phrase(total_phd_conferred, "PhD holder", "PhD holders")

  phd_percentage_string <- ""
  if (applicants > 0) {
    phd_percentage_string <- sprintf(" (~%.0f%% of applicants)", (total_phd_conferred / applicants * 100))
  } else {
    phd_percentage_string <- " (PhD % N/A)"
  }
  phd_sentence <- paste0(toupper(substr(phd_holders_phrase, 1, 1)), substr(phd_holders_phrase, 2, nchar(phd_holders_phrase)), phd_percentage_string, ".")

  # 3. Returning educators phrase
  returning_educators_sentence <- ""
  if (returning_educators == 0) {
    no_returning_educators_phrase <- format_count_phrase(0, "returning educator", "returning educators")
    returning_educators_sentence <- paste0(toupper(substr(no_returning_educators_phrase, 1, 1)), substr(no_returning_educators_phrase, 2, nchar(no_returning_educators_phrase)), ".")
  } else {
    returning_educators_base_phrase <- format_count_phrase(returning_educators, "returning educator", "returning educators")
    returning_educators_sentence <- paste0(toupper(substr(returning_educators_base_phrase, 1, 1)), substr(returning_educators_base_phrase, 2, nchar(returning_educators_base_phrase)), " had taught this unit previously.")
  }

  # Combine all parts into a paragraph
  summary_paragraph <- paste(applicants_sentence, phd_sentence, returning_educators_sentence, sep = " ")

  full_summary <- paste(heading_line, summary_paragraph, sep = "\n")

  return(full_summary)
}
