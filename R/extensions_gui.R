#' Launch Extensions GUI
#'
#' This function launches a Shiny app that allows users to browse and view
#' special considerations files. The app provides filtering options for
#' unit of study code, semester, and year.
#'
#' @export
#' @importFrom shiny shinyApp fluidPage fileInput renderDataTable textInput selectInput
#' @importFrom DT dataTableOutput renderDT DTOutput
#'
extensionsGUI <- function() {
  # Get the path to the installed package's Shiny app directory
  app_dir <- system.file("shiny/extensions", package = "soles")

  if (app_dir == "") {
    stop("Could not find Shiny app directory. Try reinstalling the package.")
  }

  shiny::runApp(app_dir)
}
