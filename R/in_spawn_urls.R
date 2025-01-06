#' Generate a list of URLs for a unit
#'
#' Perhaps there is a better way but for now, the aim is to generate a list of
#' URLs for a given unit and year to check if the unit outline is available in
#' a later step.
#'
#' @param unit string - Unit of study code, e.g. 'BIOL2022'
#' @param year numeric - Academic year, e.g. 2024
#'
#' @returns A character vector of URLs
#'
#' @keywords internal
spawn_urls <- function(unit, year = as.character(format(Sys.Date(), "%Y"))) {
  # possible values (for now)
  semesters <- c("S1C", "S2C")
  periods <- c("ND")
  types <- c("CC", "RE")

  urls <- expand.grid(
    year = year,
    semester = semesters,
    period = periods,
    type = types,
    stringsAsFactors = FALSE
  ) |>
    with(paste0("https://www.sydney.edu.au/units/", unit, "/", year, "-", semester, "-", period, "-", type))

  return(urls)
}

