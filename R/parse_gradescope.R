#' Parse Gradescope export file
#'
#' @param x Path to Gradescope export file
#' @return A data frame containing parsed Gradescope data
#' @importFrom readr read_csv
#' @importFrom dplyr select
#' @export
parse_gradescope <- function(x) {
  df <- suppressWarnings(readr::read_csv(x, show_col_types = FALSE))
  out <- df |>
    dplyr::select("First Name":"Submission Time")
  return(invisible(out))
}
