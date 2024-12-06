#' Create unified database from multiple data sources
#'
#' @description
#' Combines data from Canvas, Gradescope, and special considerations files into
#' a single unified database. The function automatically locates and parses the
#' required files from the specified folder.
#'
#' @param folder Character string specifying the directory containing the
#'   required files (Canvas export, Gradescope export, and special
#'   considerations)
#' @param uos Character string specifying the unit of study code
#'   (e.g., "BIOL2022")
#' @param year Numeric value specifying the year (e.g., 2024)
#'
#' @return
#' A data frame containing combined student data from all sources
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Validates input parameters and folder existence
#'   \item Locates required files using specialized finder functions
#'   \item Parses each file type using corresponding parser functions
#'   \item Joins the data together into a unified database
#' }
#'
#' Both uos and year parameters must be provided together if either is
#' specified.
#'
#' @seealso
#' \code{\link{find_docs}} for locating required files
#' \code{\link{parse_canvas}} for parsing Canvas exports
#' \code{\link{parse_gradescope}} for parsing Gradescope exports
#' \code{\link{parse_spec_cons}} for parsing special considerations
#'
#' @export
create_database <- function(folder, uos = NULL, year = NULL) {
  # Input validation
  if (!dir.exists(folder)) {
    stop("Folder does not exist")
  }
  if (xor(is.null(uos), is.null(year))) {
    stop("Both uos and year must be provided together")
  }

  # Get and parse documents
  mydocs <- find_docs(folder)
  canvas <- parse_canvas(mydocs$canvas)
  gradescope <- parse_gradescope(mydocs$gradescope)
  sc <- parse_spec_cons(mydocs$arrangements, "BIOL2022", 2024)

  # Join dataframes
  join_dataframes(canvas, gradescope, sc)
}

#' Join data frames from multiple sources
#'
#' @description Combines data frames from Canvas, Gradescope, and special
#' considerations into a single unified data frame. Performs left joins on SID
#' and removes redundant columns.
#'
#' @param canvas_df Data frame containing Canvas data
#' @param gradescope_df Data frame containing Gradescope data
#' @param spec_cons_df Data frame containing special considerations data
#'
#' @return A tibble containing combined student data with redundant columns
#'   removed
#'
#' @importFrom dplyr left_join select
#' @keywords internal
join_dataframes <- function(canvas_df, gradescope_df, spec_cons_df) {
  full_df <- canvas_df |>
    left_join(gradescope_df, by = "SID") |>
    left_join(spec_cons_df, by = "SID") |>
    select(-c(
      "First Name", "Last Name", "Status", "number", "classification",
      "availability.u_school", "assessment", "assessment.u_closing_date",
      "availability.u_teacher_coordinator", "u_ticket_contact",
      "parent.ref_x_uno57_student_ap_application.affected_end",
      "parent.ref_x_uno57_student_ap_application.affected_start",
      "sys_created_on", "sys_updated_by", "sys_updated_on"
    ))
  return(full_df)
}
