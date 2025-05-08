# Load necessary libraries
library(shiny)
library(bslib)
library(DT) # For data tables
library(dplyr) # For glimpse()
library(shinyjs) # For UI control
library(stringr) # For str_detect
library(logger) # For debug logging

# Define UI using bslib::page_sidebar
ui <- bslib::page_sidebar(
  title = "Extensions Viewer",
  theme = bslib::bs_theme(version = 5), # Optional: Use bslib theme
  shinyjs::useShinyjs(), # Add shinyjs for better UI control
  sidebar = bslib::sidebar(
    width = 300, # Adjust width as needed, bslib uses pixels or css units
    style = "font-size: 0.9em;", # Make all text in sidebar smaller
    shiny::p("Please upload unmodified files from both the UoS Dashboard and the Academic Plan portal. Manually edited files may not work as expected."),
    shiny::fileInput("sc_file", "Special Considerations file", multiple = FALSE, accept = c(".csv", ".xlsx")),
    shiny::fileInput("ap_file", "Academic Plan file (Optional)", multiple = FALSE, accept = c(".csv", ".xlsx")),
    shiny::fluidRow(
      shiny::column(12, shiny::selectInput("unit_filter", "Unit:", choices = NULL))
    ),
    shiny::fluidRow(
      shiny::column(6, shiny::selectInput("session_filter", "Session:", choices = NULL)),
      shiny::column(6, shiny::selectInput("year_filter", "Year:", choices = NULL))
    ),
    shiny::fluidRow(
      shiny::column(6, shiny::selectInput("mode_filter", "Mode:", choices = NULL)),
      shiny::column(6, shiny::selectInput("location_filter", "Location:", choices = NULL))
    ),
    shiny::fluidRow(
      shiny::column(12, shiny::selectInput("assessment_filter", "Assessment:", choices = NULL))
    ),
    shiny::actionButton("process_button", "Process Data"),
    shiny::radioButtons("download_type", "Download format:",
      choices = c("Raw Data" = "raw", "SEAMS2 Format" = "seams2"),
      selected = "seams2"
    ),
    shiny::downloadButton("download_csv", "Download CSV")
  ),
  # Main content area components go here
  bslib::navset_card_tab(
    height = "250px", # Adjusted height for tabs
    id = "summary_debug_tabs",
    bslib::nav_panel(
      title = "Summary",
      # Privacy notice
      shiny::div(
        class = "alert alert-secondary px-2 pt-2 pb-0",
        shiny::tags$p(
          shiny::icon("info-circle"),
          shiny::tags$strong(" Data Privacy:"),
          "All data is processed in-memory and is not stored on the app after your session, no external network connections are made, and .csv files are generated on-demand without retention.",
          style = "font-size: 0.7em; color: #666; margin-bottom: 0;"
        )
      ),
      # Statistics grid
      shiny::uiOutput("summary_stats")
    ),
    bslib::nav_panel(
      title = "Debug",
      shiny::verbatimTextOutput("debug_log_output")
    )
  ),
  # Output card with tabs, visible immediately
  bslib::navset_card_tab(
    id = "output_tabs", # Optional ID for the tabset
    bslib::nav_panel(
      title = "Raw Output",
      # Output for the raw data table
      DT::dataTableOutput("raw_data_table")
    ),
    bslib::nav_panel(
      title = "SEAMS2 Output",
      # Placeholder for table output
      DT::dataTableOutput("seams2_csv_placeholder")
    )
  )
)

# Define Server logic
server <- function(input, output, session) {
  shinyjs::disable("download_csv") # Ensure button is disabled at startup
  # Reactive values to store processed data and statistics
  processed_data <- reactiveVal(NULL)
  seams2_data <- reactiveVal(NULL)
  debug_logs <- reactiveVal("") # For storing debug logs

  # Reactive values for statistics
  stats <- reactiveValues(
    total_sc = "NA",
    total_ap = "NA",
    total_sections = "NA",
    total_students = "NA"
  )

  # Configure logger to capture logs into a reactiveVal
  # This ensures that log messages are displayed in the UI
  log_appender(function(lines) {
    # Prepend new logs to existing ones, ensuring it's a single string
    current_logs <- isolate(debug_logs())
    new_logs <- paste(lines, collapse = "\n")
    updated_logs <- paste(new_logs, current_logs, sep = "\n")

    # Limit log length to avoid performance issues (e.g., last 100 lines)
    max_log_lines <- 100
    log_lines_list <- unlist(strsplit(updated_logs, "\n"))
    if (length(log_lines_list) > max_log_lines) {
      log_lines_list <- tail(log_lines_list, max_log_lines)
    }
    debug_logs(paste(log_lines_list, collapse = "\n"))
  })
  log_threshold(INFO) # Set the minimum log level to capture (e.g., INFO, DEBUG)
  logger::log_info("Shiny session started. Logger initialized.")

  # Reactive expression to parse the special considerations file
  parsed_sc_data <- reactive({
    req(input$sc_file) # Ensure a file is uploaded

    tryCatch(
      {
        # Parse SC file
        soles::parse_sc(input$sc_file$datapath) |>
          soles::filter_sc()
      },
      error = function(e) {
        # Return a simple error message if parsing fails
        showNotification(paste("Error parsing file:", e$message), type = "error")
        return(NULL)
      }
    )
  })

  # Observer to update statistics based on SEAMS2 data
  observe({
    seams2_result <- seams2_data() # This is a reactiveVal containing the SEAMS2 output

    if (!is.null(seams2_result) && nrow(seams2_result) > 0) {
      # Total Students (Spec Cons + Plans)
      stats$total_students <- nrow(seams2_result)

      # Total students with disability plans
      # Assumes "Academic Plan" is present in 'Section name' for these students
      ap_rows <- dplyr::filter(seams2_result, stringr::str_detect(`Section name`, "Academic Plan"))
      stats$total_ap <- nrow(ap_rows)

      # Total students with special consideration (excluding those only with disability plans)
      # This counts rows that DO NOT have "Academic Plan" in their section name.
      # If a student has both SC and AP, they might be counted here if their SC-related entry
      # doesn't mention "Academic Plan" in its "Section name".
      # The user's example implies this logic: total - AP = SC.
      # Or, more directly, those whose section name does NOT contain "Academic Plan".
      sc_rows <- dplyr::filter(seams2_result, !stringr::str_detect(`Section name`, "Academic Plan"))
      stats$total_sc <- nrow(sc_rows)

      # Total sections
      stats$total_sections <- length(unique(seams2_result$`Section name`))
    } else {
      # Reset stats if no SEAMS2 data
      stats$total_sc <- "NA"
      stats$total_ap <- "NA"
      stats$total_sections <- "NA"
      stats$total_students <- "NA"
    }
  })

  # Render the summary statistics UI
  output$summary_stats <- renderUI({
    # Create formatted statistics display using reactive values
    shiny::div(
      class = "row row-cols-2 g-4 mt-0",
      # Special Considerations Count
      shiny::div(
        class = "col",
        shiny::div(
          class = "card h-100 border-0 bg-light",
          shiny::div(
            class = "card-body text-center",
            shiny::tags$h3(
              class = "card-title mb-0",
              style = "font-size: 1.2rem;",
              stats$total_sc
            ),
            shiny::tags$p(
              class = "card-text text-muted",
              style = "font-size: 0.8rem;",
              "Students with Special Considerations"
            )
          )
        )
      ),
      # Disability Plans Count
      shiny::div(
        class = "col",
        shiny::div(
          class = "card h-100 border-0 bg-light",
          shiny::div(
            class = "card-body text-center",
            shiny::tags$h3(
              class = "card-title mb-0",
              style = "font-size: 1.2rem;",
              stats$total_ap
            ),
            shiny::tags$p(
              class = "card-text text-muted",
              style = "font-size: 0.8rem;",
              "Students with Disability Plans"
            )
          )
        )
      ),
      # Total Sections
      shiny::div(
        class = "col",
        shiny::div(
          class = "card h-100 border-0 bg-light",
          shiny::div(
            class = "card-body text-center",
            shiny::tags$h3(
              class = "card-title mb-0",
              style = "font-size: 1.2rem;",
              stats$total_sections
            ),
            shiny::tags$p(
              class = "card-text text-muted",
              style = "font-size: 0.8rem;",
              "Total Sections"
            )
          )
        )
      ),
      # Total Students Count
      shiny::div(
        class = "col",
        shiny::div(
          class = "card h-100 border-0 bg-light",
          shiny::div(
            class = "card-body text-center",
            shiny::tags$h3(
              class = "card-title mb-0",
              style = "font-size: 1.2rem;",
              stats$total_students
            ),
            shiny::tags$p(
              class = "card-text text-muted",
              style = "font-size: 0.8rem;",
              "Total Students (Spec Cons + Plans)"
            )
          )
        )
      )
    )
  })

  # Ensure stats are updated when process button is clicked
  observeEvent(input$process_button, {
    # Get the parsed SC data with filters applied
    sc_data <- tryCatch(
      {
        soles::parse_sc(input$sc_file$datapath) |>
          soles::filter_sc(
            uos_filter = isolate(input$unit_filter),
            session_filter = input$session_filter,
            year_filter = input$year_filter,
            mode_filter = input$mode_filter,
            location_filter = input$location_filter
          )
      },
      error = function(e) {
        showNotification(paste("Error parsing SC file:", e$message), type = "error")
        return(NULL)
      }
    )

    # Only proceed if we have valid data
    req(sc_data)

    # Process extensions for the selected assessment
    result <- soles::process_extensions(sc_data, assessment = input$assessment_filter)

    # If AP file is provided, merge with AP extensions
    if (!is.null(input$ap_file)) {
      result <- soles::merge_ap_extensions(result, input$ap_file$datapath)
    }

    # Store the processed data
    processed_data(result)

    # Generate SEAMS2 data
    seams2_result <- soles::create_seams2_csv(result, write_csv = FALSE)
    seams2_data(seams2_result)

    # Note: The summary statistics UI is now rendered reactively outside this observer.

    # Update the SEAMS2 table
    output$seams2_csv_placeholder <- DT::renderDataTable({
      DT::datatable(seams2_result, options = list(pageLength = 25))
    })

    # Show notification that processing is complete
    showNotification("Processing complete. SEAMS2 output updated.", type = "message")
  })

  # Add validation checks
  observe({
    # Disable process button if no SC file or no assessment selected
    shinyjs::toggleState(
      "process_button",
      !is.null(input$sc_file) &&
        !is.null(input$assessment_filter)
    )

    # Disable download button if no data for the selected type
    download_ready <- FALSE
    if (!is.null(input$download_type)) { # Ensure input$download_type is available
      if (input$download_type == "seams2") {
        s2_data <- seams2_data()
        download_ready <- !is.null(s2_data) && nrow(s2_data) > 0
      } else { # Assumes "raw" if not "seams2"
        p_data <- processed_data()
        download_ready <- !is.null(p_data) && nrow(p_data) > 0
      }
    }
    shinyjs::toggleState("download_csv", download_ready)
  })

  # Reactive expression for filtered data based on current selections
  filtered_data <- reactive({
    data <- parsed_sc_data()
    req(data)

    # Apply filters based on selections
    if (!is.null(input$unit_filter)) {
      data <- data[data$uos == input$unit_filter, ]
    }
    if (!is.null(input$session_filter)) {
      data <- data[data$session == input$session_filter, ]
    }
    if (!is.null(input$year_filter)) {
      data <- data[data$year == input$year_filter, ]
    }
    if (!is.null(input$mode_filter)) {
      data <- data[data$mode == input$mode_filter, ]
    }
    if (!is.null(input$location_filter)) {
      data <- data[data$location == input$location_filter, ]
    }
    if (!is.null(input$assessment_filter)) {
      data <- data[data$assessment == input$assessment_filter, ]
    }

    return(data)
  })

  # Render the raw data table
  output$raw_data_table <- DT::renderDataTable({
    data <- filtered_data()
    if (is.data.frame(data) && nrow(data) > 0) {
      DT::datatable(data, options = list(pageLength = 10))
    } else {
      return(NULL)
    }
  })

  # Function to extract unique values and remove NAs
  get_unique_values <- function(data, column) {
    values <- unique(data[[column]])
    values <- values[!is.na(values)]
    return(values)
  }

  # Observer to update filter choices when SC file is uploaded
  observe({
    sc_data <- parsed_sc_data()
    req(sc_data)

    # Extract unique values for each filter
    uos_values <- get_unique_values(sc_data, "uos")

    # Update the Unit of Study filter
    updateSelectInput(session, "unit_filter",
      choices = uos_values,
      selected = if (length(uos_values) > 0) uos_values[1] else NULL
    )

    # Trigger the unit filter observer to cascade updates to other filters
    if (length(uos_values) > 0) {
      # This will trigger the observeEvent for unit_filter
      updateSelectInput(session, "unit_filter", selected = uos_values[1])
    }
  })

  # Observer for Unit of Study filter
  observeEvent(input$unit_filter, {
    data <- parsed_sc_data()
    req(data, input$unit_filter)

    # Filter data for the selected unit
    filtered <- data[data$uos == input$unit_filter, ]

    # Update session filter
    session_values <- get_unique_values(filtered, "session")
    updateSelectInput(session, "session_filter",
      choices = session_values,
      selected = if (length(session_values) > 0) session_values[1] else NULL
    )

    # Update assessment filter based on current selection
    assessment_values <- get_unique_values(filtered, "assessment")
    updateSelectInput(session, "assessment_filter",
      choices = assessment_values,
      selected = if (length(assessment_values) > 0) assessment_values[1] else NULL
    )
  })

  # Observer for Session filter
  observeEvent(input$session_filter, {
    data <- parsed_sc_data()
    req(data, input$unit_filter, input$session_filter)

    # Filter data for the selected unit and session
    filtered <- data[data$uos == input$unit_filter &
      data$session == input$session_filter, ]

    # Update year filter
    year_values <- get_unique_values(filtered, "year")
    updateSelectInput(session, "year_filter",
      choices = year_values,
      selected = if (length(year_values) > 0) year_values[1] else NULL
    )

    # Update assessment filter based on current selection
    assessment_values <- get_unique_values(filtered, "assessment")
    updateSelectInput(session, "assessment_filter",
      choices = assessment_values,
      selected = if (length(assessment_values) > 0) assessment_values[1] else NULL
    )
  })

  # Observer for Year filter
  observeEvent(input$year_filter, {
    data <- parsed_sc_data()
    req(data, input$unit_filter, input$session_filter, input$year_filter)

    # Filter data for the selected unit, session, and year
    filtered <- data[data$uos == input$unit_filter &
      data$session == input$session_filter &
      data$year == input$year_filter, ]

    # Update mode filter
    mode_values <- get_unique_values(filtered, "mode")
    updateSelectInput(session, "mode_filter",
      choices = mode_values,
      selected = if (length(mode_values) > 0) mode_values[1] else NULL
    )

    # Update assessment filter based on current selection
    assessment_values <- get_unique_values(filtered, "assessment")
    updateSelectInput(session, "assessment_filter",
      choices = assessment_values,
      selected = if (length(assessment_values) > 0) assessment_values[1] else NULL
    )
  })

  # Observer for Mode filter
  observeEvent(input$mode_filter, {
    data <- parsed_sc_data()
    req(data, input$unit_filter, input$session_filter, input$year_filter, input$mode_filter)

    # Filter data for the selected unit, session, year, and mode
    filtered <- data[data$uos == input$unit_filter &
      data$session == input$session_filter &
      data$year == input$year_filter &
      data$mode == input$mode_filter, ]

    # Update location filter
    location_values <- get_unique_values(filtered, "location")
    updateSelectInput(session, "location_filter",
      choices = location_values,
      selected = if (length(location_values) > 0) location_values[1] else NULL
    )

    # Update assessment filter based on current selection
    assessment_values <- get_unique_values(filtered, "assessment")
    updateSelectInput(session, "assessment_filter",
      choices = assessment_values,
      selected = if (length(assessment_values) > 0) assessment_values[1] else NULL
    )
  })

  # Observer for Location filter
  observeEvent(input$location_filter, {
    data <- parsed_sc_data()
    req(
      data, input$unit_filter, input$session_filter, input$year_filter,
      input$mode_filter, input$location_filter
    )

    # Filter data for all selected filters
    filtered <- data[data$uos == input$unit_filter &
      data$session == input$session_filter &
      data$year == input$year_filter &
      data$mode == input$mode_filter &
      data$location == input$location_filter, ]

    # Update assessment filter
    assessment_values <- get_unique_values(filtered, "assessment")
    updateSelectInput(session, "assessment_filter",
      choices = assessment_values,
      selected = if (length(assessment_values) > 0) assessment_values[1] else NULL
    )
  })

  # Download handler for the CSV button
  output$download_csv <- downloadHandler(
    filename = function() {
      if (input$download_type == "seams2") {
        paste0(format(Sys.Date(), "%Y%m%d"), "-seams2_upload.csv")
      } else {
        paste0("extensions-raw-", Sys.Date(), ".csv")
      }
    },
    content = function(file) {
      if (input$download_type == "seams2") {
        data <- seams2_data()
        if (is.null(data) || nrow(data) == 0) {
          showNotification("No SEAMS2 data to download. Please process data first.", type = "error")
          return()
        }
      } else {
        data <- processed_data()
        if (is.null(data) || nrow(data) == 0) {
          showNotification("No data to download. Please process data first.", type = "error")
          return()
        }
      }

      # Write to CSV
      write.csv(data, file, row.names = FALSE, na = "")
    }
  )

  # Render the debug log output
  output$debug_log_output <- renderPrint({
    req(debug_logs())
    cat(debug_logs())
  })
}

# Return the shiny app object
shiny::shinyApp(ui = ui, server = server)
