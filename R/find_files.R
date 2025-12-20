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
    plans = c("Category", "Assessment Adjustment")
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
      if (all(type_patterns[[type_name]] %in% cols)) {
        return(type_name)
      }
    }
    return("unknown")
  })

  return(out)
}
