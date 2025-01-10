#' Find relevant documents for database
#'
#' @description Scans a directory for CSV and Excel (.xlsx) files and
#' categorises them based on their header structure into one of several
#' predefined types that are often used in USYD systems:
#' * Canvas
#' * Gradescope
#' * Special Considerations
#' * Disability Academic Plans
#'
#' @param path Path to directory containing files to analyse
#'
#' @importFrom readxl read_excel
#'
#' @return A tibble
#'
#' @examples
#' \dontrun{
#' # Scan current directory
#' files <- find_files(".")
#'
#' # Show detected file types
#' table(files$type)
#'
#' # List unrecognized files
#' subset(files, type == "unknown")
#' }
#' @export
find_docs <- function(path = NULL) {
  # Define characteristic columns for each file type
  type_patterns <- list(
    canvas = c("SIS User ID", "SIS Login ID", "Section"),
    gradescope = c("First Name", "Last Name", "SID", "Email", "Sections"),
    spec_cons = c("state", "classification", "assessment_category"),
    academic_plans = c(
      ".*Category.*", ".*Assessment Adjustment.*", ".*Exam Adjustment.*"
    )
  )

  # List both CSV and Excel files
  files_info <- file.info(list.files(
    path = path,
    pattern = "\\.(csv|xlsx)$",
    full.names = TRUE
  ))

  # Create base tibble with file information
  out <- tibble(
    path = rownames(files_info),
    modified = files_info$mtime,
    type = "unknown"
  )

  # Process headers
  headers <- Map(function(f) {
    tryCatch(
      {
        if (grepl("\\.csv$", f)) {
          readLines(f, n = 1)
        } else {
          colnames(read_excel(f, .name_repair = "unique_quiet", n_max = 0))
        }
      },
      error = function(e) character(0)
    )
  }, out$path)

  # Process columns
  cols_list <- lapply(headers, function(h) {
    if (length(h) > 0) {
      if (is.character(h) && grepl(",", h[1])) {
        # Process CSV header
        gsub("\"", "", strsplit(h, ",")[[1]])
      } else {
        # Process Excel header
        h
      }
    } else {
      character(0)
    }
  })

  # Determine types
  out$type <- sapply(cols_list, function(cols) {
    if (length(cols) == 0) {
      return("unknown")
    }

    for (type_name in names(type_patterns)) {
      if (type_name == "academic_plans") {
        # Use regex matching for academic_plans
        if (all(sapply(type_patterns[[type_name]], function(pattern) {
          any(grepl(pattern, cols, ignore.case = TRUE))
        }))) {
          return(type_name)
        }
      } else {
        # Use exact matching for other types
        if (all(type_patterns[[type_name]] %in% cols)) {
          return(type_name)
        }
      }
    }
    return("unknown")
  })

  # rearrange columns
  out <- out %>%
    select(type, path, modified)

  return(out)
}
