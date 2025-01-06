#' Extract unit of study information
#'
#' @param unit string - Unit of study code, e.g. 'BIOL2022'
#' @param year numeric - Academic year, e.g. 2024
#'
#' @returns a list object
#'
#' @importFrom rvest read_html html_elements html_nodes html_text html_table
#' @importFrom dplyr filter mutate select first
#' @importFrom stringr str_trim str_extract str_detect
#' @importFrom purrr pluck
#' @importFrom huxtable hux set_bold set_all_borders set_position everywhere
#' @importFrom lubridate dmy_hm
#'
#' @export
view_outline <- function(unit, year) {

  # Construct URL
  url <- spawn_urls(unit, year) |>
    check_urls()

  # Read webpage with error handling
  webpage <- tryCatch(
    read_html(url),
    error = \(e) stop("Failed to access webpage: ", e$message)
  )

  # Extract unit info
  unit_text <- webpage %>%
    html_elements(".b-student-site__section-title") %>%
    html_text() %>%
    str_trim()

  if (length(unit_text) == 0) stop("Could not find unit information")

  parts <- strsplit(unit_text, ":")[[1]]
  if (length(parts) != 2) stop("Invalid unit information format")

  # Extract header info
  header <- webpage %>%
    html_elements("h3") %>%
    html_text() %>%
    first()

  if (is.na(header)) stop("Could not find header information")

  # Parse header components
  year <- str_extract(header, "\\d{4}")
  semester <- str_extract(header, "Semester\\s*\\d") %>% str_trim()
  location <- str_extract(header, "(?<=-).*(?=\\n)") %>% str_trim()

  if (any(is.na(c(year, semester, location)))) {
    stop("Missing required header information")
  }

  # Extract assessment information
  assessments <- webpage %>%
    html_elements("table") %>%
    html_table() %>%
    pluck(3, .default = data.frame()) %>%
    filter(!str_detect(Type, "Outcomes assessed|= group assignment")) %>%
    mutate(
      Type = str_extract(Type, "^[^\\n]+"),
      Description = str_extract(Description, "^[^\n]+"),
      Deadline = str_extract(Due, "(?<=Due date:).*") %>% str_trim() %>% dmy_hm(),
      Due = if_else(str_detect(Due, "\n"), str_extract(Due, "^[^\n]+"), Due)
    ) %>%
    select(-Length)

  if (nrow(assessments) == 0) stop("No assessment data found")

  # Create and return results
  structure(
    list(
      unit = str_trim(parts[1]),
      description = str_trim(parts[2]),
      year = year,
      semester = semester,
      location = location,
      assessments = assessments
    ),
    class = c("uos", "list")
  )
}

#' @export
summary.uos <- function(object, ...) {
  # Format table for display with error handling
  display_table <- tryCatch(
    {
      if (nrow(object$assessments) == 0) {
        stop("Cannot create display table: no assessment data available")
      }
      hux(object$assessments) %>%
        set_bold(row = 1, col = everywhere, value = TRUE) %>%
        set_all_borders(TRUE) %>%
        set_position("left")
    },
    error = function(e) {
      warning("Failed to create formatted display table: ", e$message)
      object$assessments # Return raw data frame if formatting fails
    }
  )

  # Print formatted output with error handling
  tryCatch(
    {
      with(object, {
        cat("\nUnit of Study Details\n")
        cat("-------------------\n")
        cat("Unit:", unit, "\n")
        cat("Description:", description, "\n")
        cat("Year:", year, "\n")
        cat("Semester:", semester, "\n")
        cat("Location:", location, "\n\n")
        cat("Assessment Schedule:\n")
        print(display_table, colnames = FALSE)
      })
    },
    error = function(e) {
      warning("Failed to print formatted output: ", e$message)
    }
  )

  return(invisible(object))
}
