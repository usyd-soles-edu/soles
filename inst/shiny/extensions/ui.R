library(shiny)
library(DT)
library(bslib)
library(shinyjs)

page_sidebar(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  title = "Extensions Viewer",
  theme = bs_theme(version = 5),

  # Sidebar with file input and filters
  sidebar = sidebar(
    width = "300px",
    bg = "light",
    collapsed = TRUE,
    div(
      class = "px-2 py-2",
      # File input section
      div(
        class = "mb-3",
        h6("Data Source", class = "text-muted mb-2"),
        # Custom styles moved to www/styles.css
        div(
          class = "small",
          fileInput("file", "Choose Special Considerations File",
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv",
              ".xlsx"
            ),
            width = "100%",
            buttonLabel = "Browse..."
          )
        ),
        # Academic Plan File input section
        div(
          class = "mb-3",
          h6("Academic Plan Data (Optional)", class = "text-muted mb-2"),
          div(
            class = "small",
            fileInput("file_ap", "Choose Academic Plan File (.xlsx)",
              accept = ".xlsx",
              width = "100%",
              buttonLabel = "Browse..."
            )
          )
        )
      ),

      # Filters section
      div(
        class = "mb-3",
        h6("Filters", class = "text-muted mb-2"),
        div(
          class = "d-grid gap-2 mb-3",
          div(
            class = "small",
            selectInput("uos", "UoS Code",
              choices = NULL,
              width = "100%"
            )
          ),
          div(
            class = "small",
            selectInput("semester", "Semester",
              choices = NULL,
              width = "100%"
            )
          ),
          div(
            class = "small",
            selectInput("year", "Year",
              choices = NULL,
              width = "100%"
            )
          ),
          div(
            class = "small",
            selectInput("assessment", "Assessment",
              choices = NULL,
              width = "100%"
            )
          )
        ),
        div(
          class = "d-grid gap-2",
          actionButton("process", "Process Data",
            class = "btn-primary btn-sm",
            width = "100%"
          ),
          div(
            class = "text-muted small text-center mt-1",
            textOutput("record_count")
          )
        )
      )
    )
  ),

  # Main content area with results
  div(
    class = "container-fluid p-2",
    # Data Summary Card
    card(
      full_screen = FALSE,
      class = "mb-3",
      card_header("Data Summary"),
      card_body(
        uiOutput("data_summary")
      )
    ),

    # Results Card
    card(
      full_screen = TRUE,
      card_header(
        div(
          class = "d-flex justify-content-between align-items-center",
          span("Results"),
          conditionalPanel(
            condition = "input.process > 0",
            downloadButton("download", "Download CSV", class = "btn-sm")
          )
        )
      ),
      card_body(
        class = "p-0",
        div(
          class = "position-relative",
          conditionalPanel(
            condition = "input.process > 0",
            div(
              class = "small text-muted p-2",
              "Note: 'Section Id' and 'Student name' columns are intentionally left blank for compatibility with SEAMS2 upload."
            ),
            div(
              class = "table-responsive",
              style = "max-height: 80vh; overflow-y: auto;",
              DTOutput("table")
            ),
            uiOutput("loading_spinner")
          )
        )
      )
    )
    # (Academic Plan Card removed as data is merged into Results table)
  )
)
