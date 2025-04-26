# R/zzz.R
# This function runs when the package is loaded.
.onLoad <- function(libname, pkgname) {
  # Set the default log threshold to OFF, making logs invisible by default.
  # Users can change this using set_soles_log_level().
  # Ensure logger package is available
  if (requireNamespace("logger", quietly = TRUE)) {
    logger::log_threshold(logger::OFF)
  } else {
    packageStartupMessage("logger package not found. Logging configuration skipped.")
  }
  invisible() # .onLoad should return invisible()
}
