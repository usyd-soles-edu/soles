# Load necessary libraries
library(shiny)
library(bslib)
library(soles) # Added for eoi_extract and other soles functions
# Ensure soles is available, though not explicitly loaded via library() here
# as functions are called with soles::

# Define the UI
ui <- bslib::page_fillable(
  title = "EOI Parser",
  theme = bslib::bs_theme(version = 5),
  shiny::tags$head(
    shiny::tags$style(shiny::HTML(
      ""
    ))
  ),
  bslib::layout_columns(
    col_widths = c(6, 6),
    bslib::card(
      height = "200px",
      bslib::card_header("Settings"),
      bslib::card_body(
        shiny::fileInput("eoi_file",
          "Select CSV file",
          multiple = FALSE,
          width = "100%", # Adjusted for better fit in column
          accept = c(
            ".csv", "text/csv",
            "text/comma-separated-values,text/plain"
          )
        ),
        shiny::selectInput("unit_filter",
          "Filter by Unit:",
          choices = "ALL", # Initial choice
          selected = "ALL",
          width = "100%" # Adjusted for better fit in column
        )
      )
    ),
    bslib::card(
      height = "200px",
      bslib::card_header("Summary"),
      bslib::card_body(
        bslib::layout_columns(
          col_widths = c(6, 6), # Making value boxes appear 2 per row in the summary card
          bslib::value_box(
            title = "Total Applications",
            value = shiny::textOutput("total_applications_output"),
            showcase = bsicons::bs_icon("people-fill")
          ),
          bslib::value_box(
            title = "Unique units offered",
            value = shiny::textOutput("unique_units_output"),
            showcase = bsicons::bs_icon("card-list")
          ),
          bslib::value_box(
            title = "Applicants with PhD",
            value = shiny::textOutput("phd_applicants_output"),
            showcase = bsicons::bs_icon("mortarboard-fill")
          ),
          bslib::value_box(
            title = "No Prior SOLES Exp.",
            value = shiny::textOutput("no_soles_experience_output"),
            showcase = bsicons::bs_icon("person-badge")
          )
        )
      )
    )
  ),
  bslib::card(
    height = "500px", # Set a fixed height for the card
    bslib::card_header("Parsed data"),
    bslib::card_body(
      # scrollable = TRUE, # Removed as per prompt's guidance to apply to tab content
      bslib::navset_card_tab(
        id = "parsed_data_tabs",
        bslib::nav_panel(
          title = "Result",
          shiny::p("Results will be displayed here.")
        ),
        bslib::nav_panel(
          title = "Raw output",
          shiny::div(
            style = "overflow-y: auto; max-height: 400px;", # Example for scrollability
            shiny::verbatimTextOutput("parsed_eoi_data")
          )
        )
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Reactive value to store processed data
  # Reactive values
  parsed_data_reactive <- shiny::reactiveVal(NULL)
  extracted_data_reactive <- shiny::reactiveVal(NULL)
  processed_data_reactive <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$eoi_file, {
    shiny::req(input$eoi_file)
    file_path <- input$eoi_file$datapath

    # Reset reactive values and filter on new file upload
    parsed_data_reactive(NULL) # Reset parsed data
    extracted_data_reactive(NULL)
    processed_data_reactive(NULL)
    # Explicitly provide label to ensure it's consistent
    shiny::updateSelectInput(session, "unit_filter", label = "Filter by Unit:", choices = "ALL", selected = "ALL")

    tryCatch(
      {
        current_parsed_data <- soles::parse_eoi(file_path)
        parsed_data_reactive(current_parsed_data) # Store parsed data

        extracted <- soles::eoi_extract(current_parsed_data) # Use current_parsed_data
        extracted_data_reactive(extracted) # Store extracted data

        # Update filter choices
        current_extracted_val <- extracted_data_reactive()
        if (!is.null(current_extracted_val) &&
          is.character(current_extracted_val) &&
          length(current_extracted_val) > 0) {
          shiny::updateSelectInput(session, "unit_filter",
            label = "Filter by Unit:", # Keep label consistent
            choices = c("ALL", current_extracted_val), # Use the vector directly
            selected = "ALL"
          )
        } else {
          # Handles cases where eoi_extract might return NULL, non-character, or an empty vector
          shiny::updateSelectInput(session, "unit_filter", label = "Filter by Unit:", choices = "ALL", selected = "ALL")
        }

        # Process data using both parsed and extracted data
        final_data <- soles::process_eoi_data(parsed_data_reactive(), extracted_data_reactive())
        processed_data_reactive(final_data)
      },
      error = function(e) {
        shiny::showNotification(paste("Error processing file:", e$message), type = "error")
        processed_data_reactive(list(error = paste("Error processing file:", e$message)))
        # Reset filter and clear extracted data on error
        shiny::updateSelectInput(session, "unit_filter", label = "Filter by Unit:", choices = "ALL", selected = "ALL")
        extracted_data_reactive(NULL)
        parsed_data_reactive(NULL) # Also clear parsed data on error
      }
    )
  })

  output$parsed_eoi_data <- shiny::renderPrint({
    # Wait for processed data and the filter input to be available and non-NULL
    shiny::req(processed_data_reactive(), input$unit_filter)

    data_to_render <- processed_data_reactive()

    # Display error if present
    if (!is.null(data_to_render$error)) {
      return(paste("Error:", data_to_render$error))
    }

    # Handle case where data might be NULL (e.g. after error, before new file)
    if (is.null(data_to_render)) {
      return("No data to display. Please upload and process a file.")
    }

    # Apply filter
    if (input$unit_filter == "ALL") {
      return(data_to_render)
    } else {
      # Ensure data_to_render is a list and the selected unit exists as a name
      if (is.list(data_to_render) && input$unit_filter %in% names(data_to_render)) {
        # Display the specific element for the selected unit
        return(data_to_render[[input$unit_filter]])
      } else {
        # This case indicates an issue: selected unit not in data names, or data not a list.
        msg <- paste0(
          "Could not display data for '", input$unit_filter, "'. ",
          "It may not be present in the currently processed data, or the data structure is not as expected. ",
          "Please try selecting 'ALL' or re-processing the file."
        )
        return(msg)
      }
    }
  })
  # Summary statistics outputs
  output$total_applications_output <- shiny::renderText({
    data <- parsed_data_reactive()
    if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
      return("N/A")
    }
    as.character(nrow(data))
  })

  output$unique_units_output <- shiny::renderText({
    data <- parsed_data_reactive()
    if (is.null(data) || !is.data.frame(data) || nrow(data) == 0 || !("preferred_units" %in% names(data))) {
      return("N/A")
    }
    as.character(length(unique(data$preferred_units)))
  })

  output$phd_applicants_output <- shiny::renderText({
    data <- parsed_data_reactive()
    total_apps <- nrow(data)
    if (is.null(data) || !is.data.frame(data) || total_apps == 0 || !("phd_conferred" %in% names(data))) {
      return("N/A")
    }
    phd_yes_data <- data[which(data$phd_conferred == "Yes"), ]
    paste0(nrow(phd_yes_data), " / ", total_apps)
  })

  output$no_soles_experience_output <- shiny::renderText({
    data <- parsed_data_reactive()
    total_apps <- nrow(data)
    if (is.null(data) || !is.data.frame(data) || total_apps == 0 || !("previous_demonstrator" %in% names(data))) {
      return("N/A")
    }
    no_exp_data <- data[which(data$previous_demonstrator == "No"), ]
    paste0(nrow(no_exp_data), " / ", total_apps)
  })
}

# Create and return the Shiny app object
shiny::shinyApp(ui = ui, server = server)
