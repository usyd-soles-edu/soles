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
  summary_lines <- list()

  # Heading: unit name
  summary_lines <- c(summary_lines, sprintf("## %s", unit_name))

  # Report on number of applicants
  applicants <- nrow(selected_unit)
  summary_lines <- c(summary_lines, sprintf("- Total applied: %d", applicants))

  # Report on applicants with PhD conferred
  # Ensure 'phd_conferred' column exists, otherwise default to 0
  if ("phd_conferred" %in% names(selected_unit)) {
    total_phd_conferred <- sum(selected_unit$phd_conferred == "Yes", na.rm = TRUE)
  } else {
    total_phd_conferred <- 0
    warning(sprintf("Column 'phd_conferred' not found for unit %s. Assuming 0 PhDs conferred.", unit_name))
  }

  if (applicants > 0) {
    summary_lines <- c(
      summary_lines,
      sprintf(
        "- Total with PhD conferred: %d of %d (~%.0f%%)",
        total_phd_conferred,
        applicants,
        (total_phd_conferred / applicants * 100)
      )
    )
  } else {
    summary_lines <- c(
      summary_lines,
      sprintf("- PhD holders: %d of %d (N/A%%)", total_phd_conferred, applicants)
    )
  }


  # Extract unit code from unit name
  uos_code <- stringr::str_extract(unit_name, "[A-Z]{4}\\d{4}")

  # Report on applicants that have taught the unit before
  # Ensure 'previous_units' column exists
  if (!is.null(uos_code) && "previous_units" %in% names(selected_unit)) {
    matches <- stringr::str_detect(selected_unit$previous_units, stringr::regex(uos_code, ignore_case = TRUE))
    returning_educators <- sum(matches, na.rm = TRUE)
  } else {
    returning_educators <- 0
    if (is.null(uos_code)) {
      warning(sprintf("Could not extract UoS code from unit name: %s", unit_name))
    }
    if (!"previous_units" %in% names(selected_unit)) {
      warning(sprintf("Column 'previous_units' not found for unit %s. Assuming 0 returning educators.", unit_name))
    }
  }

  summary_lines <- c(
    summary_lines,
    sprintf(
      "- Returning educators: %d\n",
      returning_educators
    )
  )

  return(paste(summary_lines, collapse = "\n"))
}
