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

    if (detect_canvas(cols)) {
      return("canvas")
    }
    if (detect_gradescope(cols)) {
      return("gradescope")
    }
    if (detect_sres(cols)) {
      return("sres")
    }
    if (detect_spec_cons(cols)) {
      return("spec_cons")
    }
    if (detect_academic_plans(cols)) {
      return("academic_plans")
    }
    if (detect_sres(cols)) {
      return("sres")
    }
    return("unknown")
  })

  # rearrange columns
  out <- out %>%
    select(type, path, modified)

  return(out)
}

#' Detect Canvas file format
#'
#' @param cols Character vector of column names
#' @return Logical indicating if columns match Canvas pattern
detect_canvas <- function(cols) {
  required_cols <- c("SIS User ID", "SIS Login ID", "Section")
  all(required_cols %in% cols)
}

#' Detect Gradescope file format
#'
#' @param cols Character vector of column names
#' @return Logical indicating if columns match Gradescope pattern
detect_gradescope <- function(cols) {
  required_cols <- c("First Name", "Last Name", "SID", "Email", "Sections")
  all(required_cols %in% cols)
}

#' Detect Special Considerations file format
#'
#' @param cols Character vector of column names
#' @return Logical indicating if columns match Special Considerations pattern
detect_spec_cons <- function(cols) {
  # clean up column names
  cols <- tolower(cols)
  required_cols <- c("number", "state", "classification")
  all(required_cols %in% cols)
}

#' Detect Academic Plans file format
#'
#' @param cols Character vector of column names
#' @return Logical indicating if columns match Academic Plans pattern
detect_academic_plans <- function(cols) {
  patterns <- c(
    ".*Category.*",
    ".*Assessment Adjustment.*",
    ".*Exam Adjustment.*"
  )
  all(sapply(patterns, function(pattern) {
    any(grepl(pattern, cols, ignore.case = TRUE))
  }))
}

#' Detect SRES file format
#'
#' @param cols Character vector of column names
#' @return Logical indicating if columns match SRES pattern
detect_sres <- function(cols) {
  required_cols <- c(
    "Preferred name", "Given names", "Surname", "SID",
    "Email", "Username"
  )
  all(required_cols %in% cols)
}
