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
  if (!is.character(website_url) || length(website_url) != 1) {
    stop("website_url must be a single character string")
  }

  if (!str_detect(website_url, "^https?://www\\.sydney\\.edu\\.au/units/[A-Z]{4}\\d{4}")) {
    stop("Invalid URL format. Expected Sydney University unit of study URL")
  }

  # Read the webpage with error handling
  webpage <- tryCatch(
    {
      read_html(website_url)
    },
    error = function(e) {
      stop("Failed to access webpage: ", e$message)
    }
  )

  # Extract unit code and name with validation
  unit <- tryCatch(
    {
      unit_text <- webpage %>%
        html_nodes(".b-student-site__section-title") %>%
        html_text() %>%
        str_trim()

      if (length(unit_text) == 0) {
        stop("Could not find unit code and name")
      }
      unit_text
    },
    error = function(e) {
      stop("Failed to extract unit information: ", e$message)
    }
  )

  # Extract and parse header info with validation
  header_text <- tryCatch(
    {
      header <- webpage %>%
        html_nodes("h3") %>%
        html_text()

      if (length(header) == 0) {
        stop("Could not find header information")
      }
      first(header)
    },
    error = function(e) {
      stop("Failed to extract header information: ", e$message)
    }
  )

  # Parse header components with validation
  year <- str_extract(header_text, "\\d{4}")
  if (is.na(year)) {
    stop("Could not find academic year in header")
  }

  semester <- str_extract(header_text, "Semester\\s*\\d") %>% str_trim()
  if (is.na(semester)) {
    stop("Could not find semester information in header")
  }

  location <- str_extract(header_text, "(?<=-).*(?=\\n)") %>% str_trim()
  if (is.na(location)) {
    stop("Could not find location information in header")
  }

  # Extract and clean assessment information with validation
  assessments <- tryCatch(
    {
      tables <- webpage %>%
        html_nodes("table") %>%
        html_table()

      if (length(tables) < 3) {
        stop("Assessment table not found")
      }

      assessment_data <- tables %>%
        pluck(3) %>%
        filter(!str_detect(Type, "Outcomes assessed|= group assignment"))

      if (nrow(assessment_data) == 0) {
        stop("No assessment data found")
      }

      assessment_data %>%
        mutate(
          Type = str_extract(Type, "^[^\\n]+"),
          Description = str_extract(Description, "^[^\n]+"),
          Deadline = tryCatch(
            {
              due_date <- str_extract(Due, "(?<=Due date:).*") %>% str_trim()
              if (is.na(due_date)) {
                NA
              } else {
                dmy_hm(due_date)
              }
            },
            error = function(e) NA
          ),
          Due = if_else(str_detect(Due, "\n"), str_extract(Due, "^[^\n]+"), Due)
        ) %>%
        select(-Length)
    },
    error = function(e) {
      stop("Failed to process assessment information: ", e$message)
    }
  )

  # Format table for display with error handling
  display_table <- tryCatch(
    {
      if (nrow(assessments) == 0) {
        stop("Cannot create display table: no assessment data available")
      }
      hux(assessments) %>%
        set_bold(row = 1, col = everywhere, value = TRUE) %>%
        set_all_borders(TRUE) %>%
        set_position("left")
    },
    error = function(e) {
      warning("Failed to create formatted display table: ", e$message)
      assessments # Return raw data frame if formatting fails
    }
  )

  # Create results list
  out <- list(
    unit = unit,
    year = year,
    semester = semester,
    location = location,
    assessments = assessments
  )

  # Print formatted output with error handling
  tryCatch(
    {
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
    },
    error = function(e) {
      warning("Failed to print formatted output: ", e$message)
    }
  )

  return(invisible(out))
}
