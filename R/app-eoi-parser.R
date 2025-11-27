#' Launch EOI Parser Application
#'
#' This function launches the EOI Parser Shiny application.
#' @return Does not return a value; launches a Shiny application.
#' @export
#' @importFrom shiny runApp
EoiParserApp <- function() {
  appDir <- system.file("shiny/eoi_parser_app", package = "soles")
  if (appDir == "") {
    stop("Could not find Shiny app directory. Try re-installing `soles`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
