#' Launch Extensions Viewer Shiny Application
#'
#' This function launches the Shiny application for viewing extensions.
#' The application files are located in the 'inst/shiny/extensions_viewer' directory
#' of the package.
#'
#' @return Does not return a value; launches a Shiny application.
#' @export
#' @importFrom shiny runApp
#' @examples
#' \dontrun{
#' ExtensionsViewer()
#' }
ExtensionsViewer <- function() {
  appDir <- system.file("shiny", "extensions_viewer", package = "soles")
  if (appDir == "") {
    stop("Could not find the shiny app directory. Try re-installing `soles`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
