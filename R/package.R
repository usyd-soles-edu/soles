#' @import dplyr
#' @importFrom readr read_csv
#' @importFrom lubridate parse_date_time dmy_hms
NULL

#' Package dependencies
#'
#' This package requires the following packages:
#' - dplyr: For data manipulation
#' - readr: For reading CSV files
#' - lubridate: For date/time handling
#'
#' @name soles-package
"_PACKAGE"

# For info, check:
# https://github.com/tidyverse/tibble/blob/main/R/tibble-package.R#L62

#' Set logging level
#'
#' @param verbose If `TRUE`, set log level to INFO, otherwise set to SUCCESS.
#' @return Nothing
#' @importFrom logger log_threshold INFO SUCCESS
#' @export
set_log_level <- function(verbose = FALSE) {
  if (verbose) {
    logger::log_threshold(logger::INFO)
  } else {
    logger::log_threshold(logger::SUCCESS)
  }
}

# This function runs when the package is loaded.
.onLoad <- function(libname, pkgname) {
  # Set the default log threshold to OFF, making logs invisible by default.
  # Users can change this using set_log_level().
  # Ensure logger package is available
  if (requireNamespace("logger", quietly = TRUE)) {
    logger::log_threshold(logger::OFF)
  } else {
    packageStartupMessage("logger package not found. Logging configuration skipped.")
  }
  invisible() # .onLoad should return invisible()
}
