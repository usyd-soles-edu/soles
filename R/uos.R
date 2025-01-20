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

  # Check if input is a URL or UoS string
  is_url <- str_detect(unit, "^https?://")

  if (!is_url) {
    # Handle UoS string format
    filename <- paste0(unit, ".rds")
    filepath <- file.path(save_path, filename)

    if (!file.exists(filepath)) {
      # Convert UoS string to URL and proceed
      url <- construct_url(unit)
      out <- parse_uos(url)
      write_rds(out, filepath)
      message("Saved as ", filename, " in ", save_path)
    } else {
      out <- read_rds(filepath)
    }
  } else {
    # Create filename from URL components
    unit_code <- str_extract(unit, "(?<=/units/)[^/]+")
    semester_info <- str_extract(unit, "[^/]+$")
    uos_id <- paste0(unit_code, "-", semester_info)
    filename <- paste0(uos_id, ".rds")
    filepath <- file.path(save_path, filename)

    # Check if file exists and is current
    save_rds <- TRUE
    if (file.exists(filepath)) {
      existing_data <- read_rds(filepath)
      data_age <- difftime(Sys.time(), existing_data$accessed, units = "hours")

      if (data_age <= 24) {
        out <- existing_data
        save_rds <- FALSE
      }
    }

    if (!exists("out")) {
      # Parse the webpage
      out <- parse_uos(unit)

      # Save the file with proper naming
      if (save_rds) {
        write_rds(out, filepath)
        message("Saved as ", filename, " in ", save_path)
      }
    }
  }
  return(out)
}

#' Construct URL from UoS string
#'
#' @param uos_string string in format "UNIT-YEAR-SEM-TYPE-DEL"
#' @return URL string
#'
#' @keywords internal
construct_url <- function(uos_string) {
  parts <- strsplit(uos_string, "-")[[1]]
  if (length(parts) != 5) {
    stop("UoS string must be in format: UNIT-YEAR-SEMESTER-TYPE-DELIVERY")
  }
  paste0(
    "https://www.sydney.edu.au/units/", parts[1], "/",
    paste(parts[2:5], collapse = "-")
  )
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
  message("Downloading from URL...")
  webpage <- tryCatch(
    read_html(url),
    error = \(e) stop("Failed to access webpage: ", e$message)
  )

  # Extract unit info
  unit_text <- webpage |>
    html_elements(".b-student-site__section-title") |>
    html_text() |>
    str_trim()

  if (length(unit_text) == 0) {
    stop(
      "Could not find unit information. ",
      "Is the URL correct?"
    )
  }

  parts <- strsplit(unit_text, ":")[[1]]
  if (length(parts) != 2) stop("Invalid unit information format")

  # Extract header info
  header <- webpage |>
    html_elements("h3") |>
    html_text() |>
    first()

  if (is.na(header)) stop("Could not find header information")

  # Parse header components
  year <- str_extract(header, "\\d{4}")
  semester <- str_extract(header, "Semester\\s*\\d") |>
    str_trim()
  semester_code <- paste0("S", str_extract(semester, "\\d"), "C")
  location <- str_extract(header, "(?<=-).*(?=\\n)") |>
    str_trim()

  if (any(is.na(c(year, semester, location)))) {
    stop("Missing required header information")
  }

  # Extract assessment information
  assessments <- webpage |>
    html_elements("table") |>
    html_table() |>
    pluck(3, .default = data.frame()) |>
    filter(!str_detect(Type, "Outcomes assessed|= group assignment")) |>
    mutate(
      Type = str_extract(Type, "^[^\\n]+"),
      Description = str_extract(Description, "^[^\n]+"),
      Deadline = str_extract(Due, "(?<=Due date:).*") |>
        str_trim() |>
        dmy_hm(),
      Due = if_else(str_detect(Due, "\n"), str_extract(Due, "^[^\n]+"), Due)
    ) |>
    select(-Length)

  if (nrow(assessments) == 0) stop("No assessment data found")

  # Create and return results
  structure(
    list(
      accessed = Sys.time(),
      unit = str_trim(parts[1]),
      description = str_trim(parts[2]),
      year = year,
      semester = semester_code,
      location = location,
      assessments = assessments
    ),
    class = c("uos", "list")
  )
}


#' @export
#' @method summary uos
summary.uos <- function(object, ...) {
  # Print formatted output
  tryCatch(
    {
      cat("\nUnit of Study Details\n")
      cat("---------------------\n")
      cat("Unit:", object$unit, object$description, "\n")
      cat("Year:", object$year, "Semester:", object$semester, "\n")
      cat("Location:", object$location, "\n\n")
      cat("Assessment Schedule\n")
      cat("---------------------\n")
      print(object$assessments)
      cat("---------------------\n")
      cat(
        "Note: The saved .rds file may be used by other functions to extract",
        "specific information and should not be deleted. See `?uos` for more",
        "details.\n"
      )
    },
    error = function(e) {
      warning("Failed to print formatted output: ", e$message)
    }
  )
}
