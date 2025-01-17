#' Parse Gradescope export file
#'
#' @param x Path to Gradescope export file (CSV or XLSX format)
#' @return A data frame containing parsed Gradescope data
#' @importFrom readr read_csv
#' @importFrom readxl read_xlsx
#' @importFrom dplyr select
#' @importFrom tools file_ext
#' @export
parse_gradescope <- function(x) {
  file_type <- tools::file_ext(x)

  df <- switch(tolower(file_type),
    "csv" = suppressWarnings(readr::read_csv(x, show_col_types = FALSE)),
    "xlsx" = suppressWarnings(readxl::read_xlsx(x)),
    stop("Unsupported file format. Please use CSV or XLSX file.")
  )

  out <- df |>
    dplyr::select("First Name":"Submission Time")
  return(invisible(out))
}
