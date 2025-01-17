#' Parse spec cons file
#'
#' @param x Path to sc file (CSV or XLSX format)
#' @param uos Unit of study code to filter by (optional)
#' @param year Year to filter by (optional)
#' @return A data frame containing filtered sc data
#' @importFrom readr read_csv
#' @importFrom readxl read_xlsx
#' @importFrom dplyr filter
#' @importFrom tools file_ext
#' @export
parse_sc <- function(x, uos = NULL, year = NULL) {
  # Get file extension
  file_ext <- tolower(tools::file_ext(x))

  # Read file based on extension
  df <- switch(file_ext,
    "csv" = suppressWarnings(readr::read_csv(x, show_col_types = FALSE)),
    "xlsx" = suppressWarnings(readxl::read_xlsx(x)),
    stop("Unsupported file format. Only CSV and XLSX files are supported.")
  )

  # Set column name based on file type
  availability_col <- if (file_ext == "xlsx") {
    "UoS (availability)"
  } else {
    "availability"
  }

  if (!is.null(uos) && !is.null(year)) {
    pattern <- paste0(uos, ".*", year)
    df <- dplyr::filter(df, grepl(pattern, .data[[availability_col]]))
  } else if (!is.null(uos)) {
    df <- dplyr::filter(df, grepl(uos, .data[[availability_col]]))
  } else if (!is.null(year)) {
    df <- dplyr::filter(df, grepl(year, .data[[availability_col]]))
  }

  return(df)
}
