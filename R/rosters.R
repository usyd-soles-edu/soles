load_roster <- function(path, unit) {
  if (unit == "BIOL1007") {
    out <- roster_is_BIOL1007(path)
  } else {
    stop("Unit not supported")
  }
  out
}

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
