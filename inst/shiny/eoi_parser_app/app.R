# Load necessary libraries
library(shiny)
library(bslib)
library(soles) # Added for eoi_extract and other soles functions
library(dplyr) # For mutate and filter operations
library(zip) # For zipping files
library(stringr) # For str_detect
library(tidyr) # For replace_na
library(htmltools) # For htmlEscape
# Ensure soles is available, though not explicitly loaded via library() here
# as functions are called with soles::

# Define the UI
ui <- bslib::page_fillable(
  title = "EOI Parser",
  theme = bslib::bs_theme(version = 5),
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      .small-box .inner > h3, .small-box .inner > p {
        text-align: center;
      }
      .bslib-value-box .value-box-title {
        text-align: center;
        width: 100%;
      }
      .bslib-value-box .value-box-value {
        text-align: center;
        width: 100%; /* Ensure the container takes full width for centering */
        font-family: 'Open Sans', sans-serif;
        font-size: 150%;
      }
      /* Additional rule to ensure the textOutput itself is centered if it's an inline element */
      .bslib-value-box .value-box-value > span {
        display: inline-block; /* Allows text-align to work as expected */
        width: 100%;
      }
    "))
  ),
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Controls & Summary",
      shiny::fileInput("eoi_file",
        "Select CSV file",
        multiple = FALSE,
        width = "100%",
        accept = c(
          ".csv", "text/csv",
          "text/comma-separated-values,text/plain"
        )
      ),
      shiny::selectInput("unit_filter",
        "Filter by Unit:",
        choices = "ALL", # Initial choice
        selected = "ALL",
        width = "100%"
      ),
      shiny::hr(),
      shiny::div(
        style = "text-align: center;",
        shiny::h5("Statistics")
      ),
      shiny::div(
        style = "text-align: center; padding: 2px; display: flex; flex-direction: column; justify-content: center; align-items: center; margin-bottom: 10px;",
        bsicons::bs_icon("people-fill", size = "1.5em"),
        shiny::textOutput("total_applications_output"),
        shiny::p("Total applications", style = "font-size: 0.8em; margin-bottom: 0; margin-top: 0.2em; line-height: 1.2;")
      ),
      shiny::div(
        style = "text-align: center; padding: 2px; display: flex; flex-direction: column; justify-content: center; align-items: center; margin-bottom: 10px;",
        bsicons::bs_icon("card-list", size = "1.5em"),
        shiny::textOutput("unique_units_output"),
        shiny::p("Total units in list", style = "font-size: 0.8em; margin-bottom: 0; margin-top: 0.2em; line-height: 1.2;")
      ),
      shiny::div(
        style = "text-align: center; padding: 2px; display: flex; flex-direction: column; justify-content: center; align-items: center; margin-bottom: 10px;",
        bsicons::bs_icon("person-badge", size = "1.5em"),
        shiny::textOutput("no_soles_experience_output"),
        shiny::p("Returning staff", style = "font-size: 0.8em; margin-bottom: 0; margin-top: 0.2em; line-height: 1.2;")
      ),
      shiny::div(
        style = "text-align: center; padding: 2px; display: flex; flex-direction: column; justify-content: center; align-items: center; margin-bottom: 10px;",
        bsicons::bs_icon("mortarboard-fill", size = "1.5em"),
        shiny::textOutput("phd_applicants_output"),
        shiny::p("PhD holders", style = "font-size: 0.8em; margin-bottom: 0; margin-top: 0.2em; line-height: 1.2;")
      ),
      shiny::div(
        style = "text-align: center; padding: 2px; display: flex; flex-direction: column; justify-content: center; align-items: center;", # No margin-bottom for the last item
        bsicons::bs_icon("person-check-fill", size = "1.5em"),
        shiny::textOutput("completed_training_output"),
        shiny::p("Completed training", style = "font-size: 0.8em; margin-bottom: 0; margin-top: 0.2em; line-height: 1.2;")
      )
    ),
    # Main content area
    bslib::card(
      fillable = TRUE, # Allow card content (card_body) to fill the card
      bslib::card_header("Parsed output"),
      bslib::card_body(
        fillable = TRUE, # Allow card_body content (navset_tab) to fill card_body
        bslib::navset_card_tab(
          id = "parsed_data_tabs",
          bslib::nav_panel(
            title = "Console output",
            shiny::p("Raw results will be displayed here."),
            shiny::div( # Flex container for the entire tab panel content
              style = "display: flex; flex-direction: column; height: 100%;", # Fill height, column layout
              shiny::div( # Flex item (grows and scrolls)
                style = "flex-grow: 1; overflow-y: auto; min-height: 0;", # Grow to fill, scroll if needed
                shiny::verbatimTextOutput("parsed_eoi_data")
              )
            )
          ),
          bslib::nav_panel(
            title = "Summary",
            shiny::p("Summary statistics for the selected unit:"),
            shiny::uiOutput("summary_output_markdown")
          ),
          bslib::nav_panel(
            title = "Profiles",
            shiny::div( # Flex container for the entire tab panel content
              style = "display: flex; flex-direction: column; height: 100%;", # Fill height, column layout
              shiny::selectizeInput("filter_name_input", # Flex item 1 (fixed size)
                "Filter by Name:",
                choices = c("Select a name" = ""), # Initial empty choice
                selected = "", # Default to empty
                width = "100%",
                options = list(dropdownParent = "body")
              ),
              shiny::div( # Flex item 2 (grows and scrolls)
                style = "flex-grow: 1; overflow-y: auto; min-height: 0;", # Grow to fill, scroll if needed
                shiny::htmlOutput("profile_display_html")
              )
            )
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

  # Reactive expression for data filtered by the selected unit
  filtered_data_reactive <- shiny::reactive({
    data <- parsed_data_reactive()
    unit <- input$unit_filter
    shiny::req(data, unit) # Require data and filter selection

    # Return NULL if no specific unit is selected, as the summary is per-unit
    if (unit == "ALL") {
      return(NULL)
    }

    # Ensure preferred_units exists
    if (!"preferred_units" %in% names(data)) {
      shiny::showNotification("Column 'preferred_units' not found in parsed data. Cannot filter for unit summary.", type = "warning")
      return(NULL)
    }

    # Filter based on preferred_units containing the selected unit
    # Handle potential NAs in preferred_units before filtering
    data |>
      dplyr::mutate(preferred_units = tidyr::replace_na(preferred_units, "")) |>
      dplyr::filter(stringr::str_detect(preferred_units, fixed(unit)))
  })

  shiny::observeEvent(input$eoi_file, {
    shiny::req(input$eoi_file)
    file_path <- input$eoi_file$datapath

    # Reset reactive values and filter on new file upload
    parsed_data_reactive(NULL) # Reset parsed data
    extracted_data_reactive(NULL)
    processed_data_reactive(NULL)
    # Explicitly provide label to ensure it's consistent
    shiny::updateSelectInput(session, "unit_filter", label = "Filter by Unit:", choices = "ALL", selected = "ALL")
    shiny::updateSelectizeInput(session, "filter_name_input",
      label = "Filter by Name:",
      choices = c("Select a name" = ""),
      selected = "",
      server = TRUE
    ) # Reset name filter on new file

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

  # Observe parsed_data_reactive to update name filter choices
  shiny::observe({
    data <- parsed_data_reactive()

    if (!is.null(data) && is.data.frame(data) && nrow(data) > 0 &&
      all(c("given_name", "surname") %in% names(data))) { # Check for column existence

      data_with_fullname <- tryCatch(
        {
          data |> dplyr::mutate(full_name = paste(given_name, surname))
        },
        error = function(e) {
          shiny::showNotification(paste("Error creating full_name column for name filter:", e$message), type = "warning")
          return(NULL)
        }
      )

      if (!is.null(data_with_fullname)) {
        name_choices <- unique(data_with_fullname$full_name)
        current_selection <- shiny::isolate(input$filter_name_input)
        selected_choice <- if (!is.null(current_selection) && current_selection %in% name_choices) current_selection else ""

        shiny::updateSelectizeInput(session, "filter_name_input",
          choices = c("Select a name" = "", sort(name_choices)),
          selected = selected_choice,
          server = TRUE
        )
      } else {
        # If data_with_fullname is NULL (error in mutate), reset choices
        shiny::updateSelectizeInput(session, "filter_name_input",
          choices = c("Select a name" = ""),
          selected = "",
          server = TRUE
        )
      }
    } else {
      # Reset/clear choices if data is not valid or name columns are missing
      shiny::updateSelectizeInput(session, "filter_name_input",
        choices = c("Select a name" = ""),
        selected = "",
        server = TRUE
      )
      if (!is.null(data) && is.data.frame(data) && nrow(data) > 0 &&
        !all(c("given_name", "surname") %in% names(data))) {
        shiny::showNotification("given_name and/or surname column not found in parsed data. Cannot populate name filter.", type = "warning")
      }
    }
  })

  # Reactive expression for the selected EOI profile
  selected_profile_output <- shiny::reactive({
    eoi_data <- parsed_data_reactive() # This is the direct output of parse_eoi()

    # Guard clause for no data
    if (is.null(eoi_data) || !is.data.frame(eoi_data) || nrow(eoi_data) == 0) {
      return("Please upload and parse EOI data first.")
    }

    # Guard clause for missing essential name columns in the raw parsed data
    if (!all(c("given_name", "surname") %in% names(eoi_data))) {
      return("Essential columns (given_name, surname) are missing from the EOI data. Cannot proceed with name filtering or profile generation.")
    }

    # Guard clause for no name selected
    if (is.null(input$filter_name_input) || input$filter_name_input == "") {
      return("Please select a name from the settings to view their profile.")
    }

    # Create full_name column from the raw parsed_eoi_data for filtering
    profile_data_base <- tryCatch(
      {
        eoi_data |> dplyr::mutate(full_name = paste(given_name, surname))
      },
      error = function(e) {
        shiny::showNotification(paste("Error preparing data for profile generation (mutating full_name):", e$message), type = "error")
        return(NULL)
      }
    )

    if (is.null(profile_data_base)) {
      return("An error occurred while preparing data for the profile. Check console for details.")
    }

    # Filter based on the selected full_name
    profile_data_filtered <- profile_data_base |>
      dplyr::filter(full_name == input$filter_name_input)

    if (nrow(profile_data_filtered) == 1) {
      tryCatch(
        {
          soles::create_eoi_profile(profile_data_filtered) # Pass the single filtered row
        },
        error = function(e) {
          paste("Error generating profile with soles::create_eoi_profile:", e$message)
        }
      )
    } else if (nrow(profile_data_filtered) == 0) {
      "Selected name not found in the current data. The name list might be outdated or the name does not exist in the uploaded file."
    } else {
      "Multiple entries found for the selected name. This indicates an issue with data uniqueness or the filtering logic."
    }
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
    paste0(nrow(phd_yes_data), " (", round(nrow(phd_yes_data) / total_apps * 100, 0), " %)")
  })

  output$no_soles_experience_output <- shiny::renderText({
    data <- parsed_data_reactive()
    total_apps <- nrow(data)
    if (is.null(data) || !is.data.frame(data) || total_apps == 0 || !("previous_demonstrator" %in% names(data))) {
      return("N/A")
    }
    returning_staff_data <- data[which(data$previous_demonstrator == "Yes"), ]
    paste0(nrow(returning_staff_data), " (", round(nrow(returning_staff_data) / total_apps * 100, 0), " %)")
  })

  output$completed_training_output <- shiny::renderText({
    data <- parsed_data_reactive()
    total_apps <- nrow(data)
    if (is.null(data) || !is.data.frame(data) || total_apps == 0 || !("completed_training" %in% names(data))) {
      return("N/A")
    }
    completed_training_yes_data <- data[which(data$completed_training == "Yes"), ]
    completed_count <- nrow(completed_training_yes_data)
    percentage <- if (total_apps > 0) (completed_count / total_apps) * 100 else 0
    sprintf("%d (%.1f%%)", completed_count, percentage)
  })

  output$profile_display_html <- shiny::renderUI({
    profile_content <- selected_profile_output()
    if (is.character(profile_content)) {
      # Ensure it's a single string first if it's a vector
      profile_string <- paste(profile_content, collapse = "\n")
      # Escape HTML special characters and then replace newlines with <br>
      formatted_html <- gsub("\\n", "<br>", htmltools::htmlEscape(profile_string))
      return(shiny::HTML(formatted_html))
    } else if (is.null(profile_content)) {
      return(shiny::HTML("<p><em>No profile to display.</em></p>")) # Using HTML entities for <p><em>
    } else {
      # Fallback for non-character types
      return(shiny::HTML("<p><em>Profile content is not in a displayable text format.</em></p>")) # Using HTML entities
    }
  })

  # Dynamic Markdown summary output based on selected unit
  output$summary_output_markdown <- shiny::renderUI({
    unit_name <- input$unit_filter
    shiny::req(unit_name) # Require unit selection

    if (unit_name == "ALL") {
      # Using shiny::HTML to ensure <p> is rendered correctly.
      return(shiny::HTML("<p>Select a unit to view its summary.</p>"))
    }

    elist_data <- processed_data_reactive()
    shiny::req(elist_data) # Require elist_data to be available

    # Handle cases where elist_data might be an error message from processing
    if (is.list(elist_data) && !is.null(elist_data$error)) {
      error_message <- htmltools::htmlEscape(elist_data$error)
      return(shiny::HTML(paste0("<p>Cannot generate summary: ", error_message, "</p>")))
    }

    # Handle cases where elist_data is NULL or not a list (e.g., before file processing or unexpected structure)
    if (is.null(elist_data) || !is.list(elist_data)) {
      return(shiny::HTML("<p>No data processed yet, or data is not in the expected list format for summary generation.</p>"))
    }

    # Call the generate_unit_summary function from the soles package
    # This function is expected to return a Markdown formatted string.
    summary_md <- soles::generate_unit_summary(elist = elist_data, unit_name = unit_name)

    # Render the Markdown string to HTML
    shiny::markdown(summary_md)
  })
}

# Create and return the Shiny app object
shiny::shinyApp(ui = ui, server = server)
