#' Launch Extensions GUI
#'
#' This function launches a Shiny app that allows users to browse and view
#' special considerations files. The app provides filtering options for
#' unit of study code and year.
#'
#' @export
#' @importFrom shiny shinyApp fluidPage fileInput renderDataTable textInput
#' @importFrom DT dataTableOutput renderDT DTOutput
#'
extensionsGUI <- function() {
  ui <- shiny::fluidPage(
    title = "Extensions Viewer",
    shiny::fileInput("file", "Choose Special Considerations File",
      accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv",
        ".xlsx"
      )
    ),
    shiny::textInput("uos", "Filter by UoS Code (optional)"),
    shiny::textInput("year", "Filter by Year (optional)"),
    DT::DTOutput("table")
  )

  server <- function(input, output) {
    output$table <- DT::renderDT({
      req(input$file)

      # Get filter values, convert empty strings to NULL
      uos <- if (input$uos == "") NULL else input$uos
      year <- if (input$year == "") NULL else input$year

      # Use parse_sc with filters
      parse_sc(input$file$datapath, uos = uos, year = year)
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}
