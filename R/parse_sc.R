parse_sc <- function(x, uos = NULL, year = NULL) {
  df <- suppressWarnings(readr::read_csv(x, show_col_types = FALSE))

  if (!is.null(uos) && !is.null(year)) {
    pattern <- paste0(uos, ".*", year)
    df <- dplyr::filter(df, grepl(pattern, availability))
  } else if (!is.null(uos)) {
    df <- dplyr::filter(df, grepl(uos, availability))
  } else if (!is.null(year)) {
    df <- dplyr::filter(df, grepl(year, availability))
  }

  return(df)
}
