library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(readr)
library(stringr)

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Shift Change Tracker"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Track Changes", tabName = "changes", icon = icon("exchange-alt")),
      menuItem("Help", tabName = "help", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Changes tab
      tabItem(tabName = "changes",
        fluidRow(
          box(
            title = "Step 1: Upload Original Roster",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            fileInput("originalFile", "Choose CSV File",
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv")
            ),
            tags$p("This is your baseline roster that future changes will be compared against.")
          ),
          
          box(
            title = "Step 2: Upload Current Roster",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            fileInput("currentFile", "Choose CSV File",
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv")
            ),
            tags$p("This is your updated roster with any shift changes.")
          )
        ),
        
        fluidRow(
          box(
            title = "Step 3: View Changes",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            uiOutput("dateSelector"),
            conditionalPanel(
              condition = "input.dateInput !== undefined && input.dateInput !== null",
              div(
                style = "margin-top: 15px;",
                downloadButton("downloadSummary", "Download Complete Summary", 
                              style = "margin-right: 10px;"),
                downloadButton("downloadSelectedDay", "Download Current Day Summary")
              )
            ),
            div(style = "margin-top: 20px;"),
            DTOutput("changesTable"),
            conditionalPanel(
              condition = "output.hasChanges !== true",
              tags$div(
                tags$br(),
                tags$p("No changes detected for the selected date.", 
                       style = "color: #888; text-align: center; font-style: italic;")
              )
            )
          )
        )
      ),
      
      # Help tab
      tabItem(tabName = "help",
        box(
          title = "How to Use This Tool",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          tags$div(
            tags$h4("Steps to Track Shift Changes:"),
            tags$ol(
              tags$li("Upload your original/baseline roster CSV in Step 1."),
              tags$li("Upload your current/updated roster CSV in Step 2."),
              tags$li("Select a date from the dropdown to see changes for that specific day."),
              tags$li("Use the download buttons to save summaries for your records.")
            ),
            tags$hr(),
            tags$h4("CSV Format Requirements:"),
            tags$p("Your CSV files should include the following columns:"),
            tags$ul(
              tags$li(tags$strong("date"), "- Format should be YYYY-MM-DD"),
              tags$li(tags$strong("employeeId"), "- Unique identifier for each employee"),
              tags$li(tags$strong("name"), "- Employee's name"),
              tags$li(tags$strong("shift"), "- The assigned shift (e.g., Morning, Evening, Night)")
            ),
            tags$p("Additional columns are allowed but not used for change detection.")
          )
        )
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  # Reactive values to store data
  originalData <- reactiveVal(NULL)
  currentData <- reactiveVal(NULL)
  changesData <- reactiveVal(NULL)
  availableDates <- reactiveVal(NULL)
  
  # Process original file upload
  observeEvent(input$originalFile, {
    req(input$originalFile)
    
    tryCatch({
      # Read the CSV file
      data <- read_csv(input$originalFile$datapath)
      
      # Check required columns exist
      required_cols <- c("date", "employeeId", "name", "shift")
      missing_cols <- required_cols[!required_cols %in% colnames(data)]
      
      if (length(missing_cols) > 0) {
        showNotification(
          paste("Missing required columns:", paste(missing_cols, collapse = ", ")),
          type = "error"
        )
        return(NULL)
      }
      
      # Store the data
      originalData(data)
      
      # Extract unique dates for the selector
      unique_dates <- unique(data$date)
      unique_dates <- sort(unique_dates)
      availableDates(unique_dates)
      
      showNotification("Original roster loaded successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
    })
  })
  
  # Process current file upload
  observeEvent(input$currentFile, {
    req(input$currentFile)
    req(originalData()) # Make sure original data is loaded first
    
    tryCatch({
      # Read the CSV file
      data <- read_csv(input$currentFile$datapath)
      
      # Check required columns exist
      required_cols <- c("date", "employeeId", "name", "shift")
      missing_cols <- required_cols[!required_cols %in% colnames(data)]
      
      if (length(missing_cols) > 0) {
        showNotification(
          paste("Missing required columns:", paste(missing_cols, collapse = ", ")),
          type = "error"
        )
        return(NULL)
      }
      
      # Store the data
      currentData(data)
      
      # Detect changes
      original <- originalData()
      current <- data
      
      changes <- current %>%
        inner_join(original, 
                  by = c("date", "employeeId"),
                  suffix = c("", "_original")) %>%
        filter(shift != shift_original) %>%
        select(date, employeeId, name, shift_original, shift) %>%
        rename(original_shift = shift_original, new_shift = shift)
      
      changesData(changes)
      
      # Update available dates if needed
      all_dates <- unique(c(availableDates(), unique(changes$date)))
      all_dates <- sort(all_dates)
      availableDates(all_dates)
      
      if (nrow(changes) > 0) {
        showNotification(paste(nrow(changes), "shift changes detected"), type = "message")
      } else {
        showNotification("No shift changes detected", type = "warning")
      }
      
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
    })
  })
  
  # Create date selector
  output$dateSelector <- renderUI({
    req(availableDates())
    dates <- availableDates()
    
    if (length(dates) > 0) {
      selectInput("dateInput", "Select Date:", 
                  choices = dates,
                  selected = dates[1])
    } else {
      tags$p("No dates available. Please upload roster files.")
    }
  })
  
  # Display changes for selected date
  output$changesTable <- renderDT({
    req(changesData())
    req(input$dateInput)
    
    changes <- changesData()
    date_changes <- changes %>% filter(date == input$dateInput)
    
    output$hasChanges <- reactive(nrow(date_changes) > 0)
    outputOptions(output, "hasChanges", suspendWhenHidden = FALSE)
    
    if (nrow(date_changes) == 0) {
      return(NULL)
    }
    
    datatable(date_changes,
              options = list(
                dom = 't',
                ordering = TRUE,
                pageLength = 25
              ),
              rownames = FALSE) %>%
      formatStyle(
        'original_shift',
        backgroundColor = '#fff3e0',
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        'new_shift',
        backgroundColor = '#e8f5e9',
        fontWeight = 'bold'
      )
  })
  
  # Download complete summary
  output$downloadSummary <- downloadHandler(
    filename = function() {
      paste("shift_changes_summary_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(changesData())
      write_csv(changesData(), file)
    }
  )
  
  # Download current day summary
  output$downloadSelectedDay <- downloadHandler(
    filename = function() {
      paste("shift_changes_", input$dateInput, ".csv", sep = "")
    },
    content = function(file) {
      req(changesData())
      req(input$dateInput)
      
      changes <- changesData()
      date_changes <- changes %>% filter(date == input$dateInput)
      write_csv(date_changes, file)
    }
  )
}

# Run the app
shinyApp(ui, server)