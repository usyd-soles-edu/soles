
 
 # Load necessary libraries
library(shiny)
library(bslib)
library(DT) # For data tables
library(dplyr) # For glimpse()
library(shinyjs) # For UI control

# Define UI using bslib::page_sidebar
ui <- bslib::page_sidebar(
  title = "Extensions Viewer",
  theme = bslib::bs_theme(version = 5), # Optional: Use bslib theme
  shinyjs::useShinyjs(), # Add shinyjs for better UI control
  sidebar = bslib::sidebar(
    width = 300, # Adjust width as needed, bslib uses pixels or css units
    style = "font-size: 0.9em;", # Make all text in sidebar smaller
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
  bslib::card(
    bslib::card_header("Summary"),
    bslib::card_body(
      # Privacy notice
      shiny::div(
        class = "alert alert-secondary p-2",
        shiny::tags$p(
          shiny::icon("info-circle"),
          shiny::tags$strong(" Data Privacy:"),
          "All data is processed in-memory and is not stored on the app after your session, no external network connections are made, and .csv files are generated on-demand without retention.",
          style = "font-size: 0.7em; color: #666; margin-bottom: 0;"
        )
      ),
      # Statistics grid
      shiny::uiOutput("summary_stats")
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
  # Reactive values to store processed data and statistics
  processed_data <- reactiveVal(NULL)
  seams2_data <- reactiveVal(NULL)

  # Reactive values for statistics
  stats <- reactiveValues(
    total_sc = "NA",
    total_ap = "NA",
    total_sections = "NA",
    total_students = "NA"
  )

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

  # Observer to update statistics when data or filters change
  observe({
    # Get unfiltered SC data first (before assessment filter)
    sc_data <- parsed_sc_data()
    # Get SEAMS2 data if available
    seams2_result <- seams2_data()

    # Update Special Considerations count
    if (!is.null(sc_data) && nrow(sc_data) > 0) {
      # Apply only UoS/Session/Year filters for accurate total
      filtered_sc <- sc_data
      if (!is.null(input$unit_filter)) {
        filtered_sc <- filtered_sc[filtered_sc$uos == input$unit_filter, ]
      }
      if (!is.null(input$session_filter)) {
        filtered_sc <- filtered_sc[filtered_sc$session == input$session_filter, ]
      }
      if (!is.null(input$year_filter)) {
        filtered_sc <- filtered_sc[filtered_sc$year == input$year_filter, ]
      }
      if (nrow(filtered_sc) > 0) {
        # Check for SID column variations and handle missing case
        if ("SID" %in% names(filtered_sc)) {
          stats$total_sc <- length(unique(filtered_sc$SID))
        } else if ("sid" %in% names(filtered_sc)) {
          stats$total_sc <- length(unique(filtered_sc$sid))
        } else {
          stats$total_sc <- "NA"
          warning("No SID column found in special considerations data")
        }
      }
    }

    # Update Total Sections count
    if (!is.null(seams2_result) && nrow(seams2_result) > 0) {
      if ("Section name" %in% names(seams2_result) &&
        any(!is.na(seams2_result$`Section name`) &
          nzchar(trimws(seams2_result$`Section name`)))) {
        stats$total_sections <- length(unique(seams2_result$`Section name`[
          !is.na(seams2_result$`Section name`) &
            nzchar(trimws(seams2_result$`Section name`))
        ]))
      }
    }

    # Update AP stats
    if (!is.null(input$ap_file) && !is.null(input$ap_file$datapath)) {
      ap_data <- tryCatch(
        {
          # Parse and filter AP data using package functions
          ap_base <- soles::parse_ap(input$ap_file$datapath)
          soles::filter_ap(ap_base,
            uos_filter = input$unit_filter,
            session_filter = input$session_filter,
            year_filter = input$year_filter
          )
        },
        error = function(e) {
          showNotification(paste("Error processing AP file for stats:", e$message), type = "warning")
          NULL
        }
      )

      if (!is.null(ap_data) && nrow(ap_data) > 0) {
        # Check for SID column variations and handle missing case
        if ("SID" %in% names(ap_data)) {
          stats$total_ap <- length(unique(ap_data$SID))
        } else if ("sid" %in% names(ap_data)) {
          stats$total_ap <- length(unique(ap_data$sid))
        } else {
          stats$total_ap <- "NA"
          warning("No SID column found in academic plans data")
        }
      }
    }

    # Update total students
    # If both are "NA", result is "NA"
    if (!is.numeric(stats$total_sc) && !is.numeric(stats$total_ap)) {
      stats$total_students <- "NA"
    } else {
      # If at least one is numeric, add them (treating non-numeric as 0)
      sc_count <- if (is.numeric(stats$total_sc)) stats$total_sc else 0
      ap_count <- if (is.numeric(stats$total_ap)) stats$total_ap else 0
      # Only show total if it's greater than 0
      stats$total_students <- if (sc_count + ap_count > 0) sc_count + ap_count else "NA"
    }
  })

  # Render the summary statistics UI
  output$summary_stats <- renderUI({
    # Create formatted statistics display using reactive values
    shiny::div(
      class = "row row-cols-2 g-4 mt-2",
      # Special Considerations Count
      shiny::div(
        class = "col",
        shiny::div(
          class = "card h-100 border-0 bg-light",
          shiny::div(
            class = "card-body text-center",
            shiny::tags$h3(
              class = "card-title mb-0",
              style = "font-size: 1.5rem;",
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
              style = "font-size: 1.5rem;",
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
              style = "font-size: 1.5rem;",
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
              style = "font-size: 1.5rem;",
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

    # Disable download button if no processed data
    shinyjs::toggleState("download_csv", !is.null(processed_data()))
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
      write.csv(data, file, row.names = FALSE)
    }
  )
}

# Return the shiny app object
shiny::shinyApp(ui = ui, server = server)
