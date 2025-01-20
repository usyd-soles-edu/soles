#' Build student database
#'
#' Build a student database from Canvas, Gradescope, and other files. This
#' function is limited to one file of each type. Gradescope file is optional.
#'
#' @param df A data frame containing file paths and metadata
#' @param uos Optional. Unit of study URL or code (e.g., "BIOL1009-2024-S1C-ND-CC")
#'
#' @returns A list of parsed data frames (as tibbles)
#' @export
#' @importFrom logger log_info log_debug log_error
#' @importFrom purrr walk
#' @importFrom dplyr filter group_by slice_max ungroup mutate pull
build_database <- function(df, uos = NULL) {
  # Initialize logger for this function
  logger::log_threshold(logger::DEBUG)
  log_debug("Starting build_database function")

  # Get latest file of each type
  log_info("Selecting most recent files of each type...")
  picked <- df |>
    filter(!type == "unknown") |>
    group_by(type) |>
    slice_max(order_by = modified, n = 1) |>
    ungroup()

  log_debug("Selected files:")
  picked |>
    mutate(display = paste("-", str_to_title(type), ":", basename(path))) |>
    pull(display) |>
    walk(~ log_debug(.x))

  # read canvas
  log_info("Parsing Canvas file...")
  canvas <- picked |>
    filter(type == "canvas") |>
    pull(path) |>
    parse_canvas()
  log_debug("Canvas parsing complete")

  # Handle unit details
  log_info("Processing unit details...")
  uos_data <- if (!is.null(uos)) {
    log_info("Using provided unit details...")
    uos(uos)
  } else if (!is.null(canvas$uos_details)) {
    log_info("Using Canvas unit details...")
    canvas$uos_details
  } else {
    log_error("No unit details available")
    stop(
      "Could not determine unit details. Please provide the unit ",
      "details using the 'uos' parameter (URL or unit code)."
    )
  }

  unit <- uos_data$unit
  semester <- uos_data$semester
  year <- uos_data$year

  log_info(sprintf("Processing data for %s-%s-%s", unit, semester, year))

  # read gradescope
  log_info("Parsing Gradescope file...")
  gradescope <- picked |>
    filter(type == "gradescope") |>
    pull(path) |>
    parse_gradescope()

  log_debug("Gradescope parsing complete")

  # read ap
  log_info("Parsing academic plans...")
  ap <- picked |>
    filter(type == "academic_plans") |>
    pull(path) |>
    parse_ap() |>
    filter(Year == year) |>
    filter(`UoS Code` == unit) |>
    filter(Session == semester)

  log_debug("Academic plans parsing complete")

  # read spec_cons
  log_info("Parsing special considerations...")
  spec_cons_path <- picked |>
    filter(type == "spec_cons") |>
    pull(path)

  spec_cons <- parse_sc(
    spec_cons_path,
    uos = unit,
    year = year
  )
  log_debug("Special considerations parsing complete")

  out <- list(
    unit_details = uos_data,
    canvas = canvas$canvas,
    gradescope = gradescope,
    ap = ap,
    spec_cons = spec_cons
  )
  log_info(sprintf("Build complete for %s-%s-%s", unit, semester, year))

  return(invisible(out))
}
