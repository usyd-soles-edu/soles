#' Parse Disability Academic Plans file
#'
#' Convert a complex Excel-formatted Disability Academic Plans file into a clean
#' data frame. The input file contains two header rows that need special
#' processing to create proper column names. This function handles that
#' complexity and returns a structured tibble.
#'
#' @param path path to file
#'
#' @returns a tible
#' @export
parse_ap <- function(path) {
  # Validate input
  if (!file.exists(path)) {
    stop("File not found:", path)
  }
  # Read first two rows for column names
  raw_names <- read_excel(path, .name_repair = "unique_quiet", n_max = 2)

  # Extract and clean column names
  # Extract category names
  category <- names(raw_names) |>
    str_remove_all("\\...\\d+$")

  # Extract metadata columns
  meta <- raw_names |>
    slice(2) |>
    select(1:15) |>
    unlist() |>
    unname()

  # Extract arrangement columns
  arrangements <- raw_names |>
    slice(1) |>
    select(-(1:15)) |>
    unlist() |>
    unname()

  # Combine and clean
  col_names <- paste(category, c(meta, arrangements), sep = " - ") |>
    str_remove("^ - ") |>
    str_remove("^Category - ")

  # Read and process data
  out <-
    read_excel(path, skip = 3, col_names = col_names) |>
    slice(-n()) |> # Remove last row
    mutate(Year = as.integer(Year)) |>
    select(-`Assessment ID (ignore)`)

  return(out)
}
