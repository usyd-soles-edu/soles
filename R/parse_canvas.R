#' Parse Canvas export file
#'
#' @description
#' Reads a Canvas export file and returns a list containg student data
#' and unit information. The unit information is useful for filtering
#' other files (e.g. academic plans) to match the Canvas unit.
#'
#' @param x Path to Canvas file
#' @return A list
#' @importFrom readr read_csv
#' @importFrom dplyr select rename all_of
#' @export
parse_canvas <- function(x) {
  col_names <- read_column_names(x)
  canvas <- suppressWarnings(
    read_csv(x,
      skip = 3,
      col_names = col_names,
      na = "N/A",
      show_col_types = FALSE
    )
  ) |>
    select(all_of(c(
      "Student", "SIS User ID", "SIS Login ID",
      setdiff(col_names, c("Student", "SIS User ID", "SIS Login ID", "ID"))
    ))) |>
    rename(
      SID = `SIS User ID`,
      Unikey = `SIS Login ID`
    )

  # Extract unit details from first section
  section <- canvas$Section[1]
  uos_code <- str_extract(section, "(?<=\\(activity\\)\\s).*?(?=/)")
  uos_details <- setNames(
    strsplit(uos_code, "-")[[1]],
    c("year", "unit", "semester", "type", "delivery")
  )

  list(canvas = canvas, uos_details = uos_details)
}
