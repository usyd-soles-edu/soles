#' Build student database
#'
#' Build a student database from Canvas, Gradescope, and other files. This
#' function is limited to one file of each type. Gradescope file is optional.
#'
#' @param df A data frame containing file paths and metadata
#'
#' @returns A list of parsed data frames (as tibbles)
#' @export
build_database <- function(df) {
  # Get latest file of each type
  message("Picking files...")
  picked <- df |>
    filter(!type == "unknown") |>
    group_by(type) |>
    slice_max(order_by = modified, n = 1) |>
    ungroup()

  cat("\nFiles used:\n")
  picked |>
    mutate(display = paste("-", str_to_title(type), ":", basename(path))) |>
    pull(display) |>
    walk(~ cat(.x, "\n"))

  # read canvas
  message("Parsing Canvas...")
  canvas <-
    picked |>
    filter(type == "canvas") |>
    pull(path) |>
    parse_canvas()
  unit <- canvas$uos_details[["unit"]]
  semester <- canvas$uos_details[["semester"]]
  year <- canvas$uos_details[["year"]]
  message(
    "Unit of Study: ",
    unit, "-",
    semester, "-",
    year
  )
  message("Filtering data based on unit, semester, and year...")
  Sys.sleep(1)

  # read gradescope
  message("Parsing Gradescope...")
  gradescope <-
    picked |>
    filter(type == "gradescope") |>
    pull(path) |>
    parse_gradescope()

  # read ap
  message("Parsing academic plans...")
  ap <-
    picked |>
    filter(type == "academic_plans") |>
    pull(path) |>
    parse_ap() |>
    filter(Year %in% canvas$uos_details[["year"]]) |>
    filter(`UoS Code` %in% canvas$uos_details[["unit"]]) |>
    filter(Session %in% canvas$uos_details[["semester"]])

  # read spec_cons
  message("Parsing special considerations...")
  spec_cons <-
    picked |>
    filter(type == "spec_cons") |>
    pull(path) |>
    parse_sc() |>
    filter(
      str_detect(
        availability,
        paste0(
          unit, "-",
          semester, "-",
          year
        )
      )
    )
  out <-
    list(
      canvas = canvas$canvas,
      gradescope = gradescope,
      ap = ap,
      spec_cons = spec_cons
    )
  message(
    "Done! All files are parsed for ",
    unit, "-",
    semester, "-",
    year
  )

  return(invisible(out))
}
