#' Extract unit of study information
#'
#' @param unit string. The URL of the unit outline page or a UoS string (e.g., "DATA1001-2024-S1C-ND-CC").
#' @param path string. Path to save/load RDS files. If NULL, uses current working directory.
#'
#' @returns a list object
#'
#' @importFrom readr read_rds write_rds
#' @importFrom stringr str_detect str_extract
#'
#' @export
uos <- function(unit, path = NULL) {
  # Use provided path or current working directory
  save_path <- if (is.null(path)) getwd() else path
  save_rds <- TRUE # Initialize save_rds flag

  # Check if input is a URL or UoS string
  is_url <- str_detect(unit, "^https?://")

  if (!is_url) {
    # Handle UoS string format
    filename <- paste0(unit, ".rds")
    filepath <- file.path(save_path, filename)

    if (!file.exists(filepath)) {
      stop(
        "No cached data found for ", unit,
        ". Please provide a URL to fetch the data."
      )
    }

    out <- read_rds(filepath)

    # Print formatted output for cached data
    tryCatch(
      {
        with(out, {
          cat("\nUnit of Study Details (Cached)\n")
          cat("---------------------\n")
          cat("Unit:", unit, description, "\n")
          cat("Year:", year, semester, "\n")
          cat("Location:", location, "\n\n")
          cat("Assessment Schedule\n")
          cat("---------------------\n")
          print(assessments)
          cat("---------------------\n")
        })
      },
      error = function(e) {
        warning("Failed to print formatted output: ", e$message)
      }
    )
    return(invisible(out))
  }

  # Create filename from URL components
  unit_code <- str_extract(unit, "(?<=/units/)[^/]+")
  semester_info <- str_extract(unit, "[^/]+$")
  filename <- paste0(unit_code, "-", semester_info, ".rds")
  filepath <- file.path(save_path, filename)

  # Check if file exists and set save_rds flag
  if (file.exists(filepath)) {
    existing_data <- read_rds(filepath)
    if (year(existing_data$accessed) == year(Sys.time())) {
      message("Unit outline data is current. Using cached version.")
      out <- existing_data
      save_rds <- FALSE
      return(invisible(out))
    }
    message("Unit outline data is outdated. Fetching new data.")
  }

  # Parse the webpage
  out <- parse_uos(unit)

  # Save the file with proper naming
  if (save_rds) {
    write_rds(out, filepath)
    message("Saved as ", filename, " in ", save_path)
  }

  # Print formatted output
  tryCatch(
    {
      with(out, {
        cat("\nUnit of Study Details\n")
        cat("---------------------\n")
        cat("Unit:", unit, description, "\n")
        cat("Year:", year, semester, "\n")
        cat("Location:", location, "\n\n")
        cat("Assessment Schedule\n")
        cat("---------------------\n")
        print(assessments)
        cat("---------------------\n")
        cat(
          "Note: The saved .rds file may be used by other functions to extract",
          "specific information and should not be deleted. See `?uos` for more",
          "details."
        )
      })
    },
    error = function(e) {
      warning("Failed to print formatted output: ", e$message)
    }
  )
  return(invisible(out))
}

#' Parse unit of study webpage
#'
#' @param url string. The URL of the unit outline page
#'
#' @returns a list object with parsed unit information
#'
#' @importFrom rvest read_html html_elements html_nodes html_text html_table
#' @importFrom dplyr filter mutate select first
#' @importFrom stringr str_trim str_extract str_detect
#' @importFrom purrr pluck
#' @importFrom lubridate dmy_hm
#'
#' @keywords internal
parse_uos <- function(url) {
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

  if (length(unit_text) == 0) stop("Could not find unit information. Is the URL correct?")

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
      accessed = Sys.time(),
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
