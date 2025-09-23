#' Parse BIOL1007 Roster File
#'
#' This function parses a BIOL1007 roster Excel file and returns standardized data.
#'
#' @param path Path to the BIOL1007 roster Excel file.
#' @param verbose Logical, whether to print informational messages. Default TRUE.
#' @return A data frame with standardized roster data.
#' @importFrom readxl read_excel
#' @importFrom janitor clean_names
#' @importFrom dplyr mutate across filter left_join select group_by ungroup arrange bind_rows row_number case_when if_any
#' @importFrom tidyr fill pivot_longer extract
#' @importFrom stringr str_extract
#' @import lgr
#' @export
parse_biol1007_roster <- function(path, verbose = TRUE) {
  if(verbose) lgr$info("Processing BIOL1007 roster...")
  
  # Read and clean the roster data
  raw_data <- suppressMessages(
    read_excel(path, skip = 4)
  ) |>
    clean_names() |>
    mutate(across(-c(1:4), ~ replace(., . == ".", NA_character_))) |>
    fill(c(week, practical), .direction = "down") |>
    filter(!is.na(date)) |>
    mutate(week = sprintf("w%02d", as.numeric(week))) |>
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
    filter(!if_any(-date, ~ . == "NA")) |>
    select(week, date, lab, day_of_week, start_time, role, name)
  
  # Calculate rates for staff based on role and weekly session order
  # Process demonstrators - all get "demo" rate
  demonstrators <- raw_data |>
    filter(role == "demo") |>
    mutate(rate = "demo")
  
  # Process supervisors - first session each week gets "tutorial", subsequent get "r_tutorial"
  supervisors <- raw_data |>
    filter(role == "sup") |>
    group_by(name, week) |>
    arrange(date, day_of_week, start_time) |>
    mutate(
      session_order = row_number(),
      rate = case_when(
        session_order == 1 ~ "tutorial",
        session_order > 1 ~ "r_tutorial",
        TRUE ~ NA_character_
      )
    ) |>
    ungroup() |>
    select(-session_order)
  
  # Combine results
  rates_data <- dplyr::bind_rows(demonstrators, supervisors) |>
    arrange(name, week, date, day_of_week, start_time)
  
  # Assign casual rates based on staff qualifications and role rates
  staff_data <- read_excel(path, sheet = "staff") |>
    clean_names()
  
  result <- left_join(rates_data, staff_data, by = c("name" = "label")) |>
    mutate(
      rate_code = case_when(
        phd == 1 & rate == "tutorial" ~ "TU1",
        phd == 1 & rate == "r_tutorial" ~ "TU3",
        is.na(phd) & rate == "tutorial" ~ "TU2",
        is.na(phd) & rate == "r_tutorial" ~ "TU4",
        phd == 1 & rate == "demo" ~ "DE1",
        is.na(phd) & rate == "demo" ~ "DE2",
        TRUE ~ NA_character_
      )
    ) |>
    select(-c(surname, given_name, new))
  
  return(result)
}