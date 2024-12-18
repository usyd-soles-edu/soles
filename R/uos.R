#' Extract Unit of Study Information
#'
#' @description
#' Retrieves unit of study information from the University of Sydney website,
#' including unit details and assessment schedule.
#'
#' @param website_url Character string containing the URL for a University of
#'   Sydney unit of study page
#'
#' @return A list containing:
#' \itemize{
#'   \item unit: Unit code and name
#'   \item year: Academic year
#'   \item semester: Semester number
#'   \item location: Campus location
#'   \item assessments: Data frame of assessment tasks
#' }
#'
#' @importFrom rvest read_html html_nodes html_text html_table
#' @importFrom dplyr filter mutate select first
#' @importFrom stringr str_trim str_extract str_detect
#' @importFrom tidyr pluck
#' @importFrom huxtable hux set_bold set_all_borders set_position everywhere
#' @importFrom lubridate dmy_hm
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract BIOL2022 information
#' biol2022 <- uos("https://www.sydney.edu.au/units/BIOL2022/2024-S2C-ND-CC")
#'
#' # View assessment schedule
#' biol2022$assessments
#' }
uos <- function(website_url) {
  # Read the webpage
  webpage <- read_html(website_url)

  # Extract unit code and name
  unit <- webpage %>%
    html_nodes(".b-student-site__section-title") %>%
    html_text() %>%
    str_trim()

  # Extract and parse header info
  header_text <- webpage %>%
    html_nodes("h3") %>%
    html_text() %>%
    first()

  # Parse header components
  year <- str_extract(header_text, "\\d{4}")
  semester <- str_extract(header_text, "Semester\\s*\\d") %>% str_trim()
  location <- str_extract(header_text, "(?<=-).*(?=\\n)") %>% str_trim()

  # Extract and clean assessment information
  assessments <- webpage %>%
    html_nodes("table") %>%
    html_table() %>%
    pluck(3) %>%
    filter(!str_detect(Type, "Outcomes assessed|= group assignment")) %>%
    mutate(
      Type = str_extract(Type, "^[^\\n]+"),
      Description = str_extract(Description, "^[^\n]+"),
      Deadline = str_extract(Due, "(?<=Due date:).*") %>% str_trim() %>% dmy_hm(),
      Due = if_else(str_detect(Due, "\n"), str_extract(Due, "^[^\n]+"), Due)
    ) %>%
    select(-Length)

  # Format table for display
  display_table <- hux(assessments) %>%
    set_bold(row = 1, col = everywhere, value = TRUE) %>%
    set_all_borders(TRUE) %>%
    set_position("left")

  # Create results list
  out <- list(
    unit = unit,
    year = year,
    semester = semester,
    location = location,
    assessments = assessments
  )

  # Print formatted output
  with(out, {
    cat("\nUnit of Study Details\n")
    cat("-------------------\n")
    cat("Unit:", unit, "\n")
    cat("Year:", year, "\n")
    cat("Semester:", semester, "\n")
    cat("Location:", location, "\n\n")
    cat("Assessment Schedule:\n")
    print(display_table, colnames = FALSE)
  })

  return(invisible(out))
}
