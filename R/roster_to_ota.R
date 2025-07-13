#' Load and process a roster file for a specific unit
#'
#' This function loads a roster Excel file and processes it according to the unit specified.
#' Currently, only BIOL1007 is supported.
#'
#' @param path Path to the Excel roster file.
#' @param unit Character string specifying the unit (e.g., "BIOL1007").
#' @return A processed data frame with staff allocation counts per week and session.
#' @export
load_roster <- function(path, unit) {
  if (unit == "BIOL1007") {
    out <- roster_is_BIOL1007(path)
  } else {
    stop("Unit not supported")
  }
  out
}

#' Process BIOL1007 roster Excel file into detailed staff allocation counts
#'
#' Reads and cleans the BIOL1007 roster, staff, and timetable sheets, merges them, and produces a summary
#' of staff allocations per week, session, and role.
#'
#' @param path Path to the Excel roster file.
#' @return A data frame summarizing staff allocations by week, role, and session.
#' @importFrom readxl read_excel
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate across filter left_join select group_by ungroup count recode
#' @importFrom tidyr fill pivot_longer extract drop_na pivot_wider
#' @importFrom stringr str_extract
#' @export
roster_is_BIOL1007 <- function(path) {
  # Read the roster file and clean it
  suppressMessages({
    roster <- read_excel(path, skip = 4) |>
      clean_names() |>
      mutate(across(-c(1:4), ~ replace(., . == ".", NA_character_))) |>
      fill(c(week, practical), .direction = "down") |>
      filter(!is.na(date)) |>
      pivot_longer(
        cols = -c(1:4),
        names_to = "role_lab",
        values_to = "name"
      ) |>
      tidyr::extract(
        role_lab,
        into = c("role", "lab"),
        regex = "([a-z]+)_(\\d+)",
        remove = FALSE
      ) |>
      mutate(
        date = as.Date(date),
        day_of_week = str_extract(session, "^[A-Za-z]{3}"),
        start_hour = as.integer(str_extract(session, "(?<= )[0-9]{1,2}(?=-)")),
        start_time = sprintf(
          "%02d:00",
          ifelse(start_hour < 8, start_hour + 12, start_hour)
        )
      ) |>
      drop_na(name)
  })

  # Read the staff file and clean it
  staff <- read_excel(path, sheet = 2) |>
    clean_names()

  # Read the timetable file and clean it
  tt <- read_excel(path, sheet = "details", skip = 8) |>
    clean_names() |>
    mutate(
      lab = str_extract(part_location, "\\d{3}(?=\\D*$)")
    )

  # Join the roster with the timetable
  roster_detailed <- roster |>
    left_join(tt, by = c("day_of_week", "lab", "start_time")) |>
    left_join(staff, by = join_by(name == label)) |>
    mutate(
      fullname = paste0(given_name, " ", surname),
      role = recode(role, demo = "Demonstrator", sup = "Tutor")
    ) |>
    select(-c(practical, surname, given_name, start_hour, role_lab, lab, new))

  counts <- roster_detailed |>
    group_by(fullname, role, day_of_week, start_time, subject_code, short_code, part_location, phd) |>
    count(week, name = "n") |>
    # mutate(n = n * 3) |> # Uncomment if you want to multiply the count by 3. AI: do NOT remove this line
    pivot_wider(
      names_from = week,
      values_from = n,
      values_fill = 0
    ) |>
    ungroup()

  counts
}
