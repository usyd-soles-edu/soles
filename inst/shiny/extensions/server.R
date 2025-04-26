library(shiny)
library(DT)
library(dplyr)
library(stringr)
library(readr) # Added for header reading
library(readxl) # Added for header reading
library(tools) # Added for file extension checking
library(lubridate) # Added for date calculations

function(input, output, session) {
  # Reactive value to store the validated & parsed SC data
  validated_data <- reactiveVal(NULL)

  # Reactive value to store the raw academic plan data
  data_ap <- reactive({
    req(input$file_ap)
    # Assuming parse_ap is available in the soles package namespace
    # If it's not exported, use soles:::parse_ap
    tryCatch(
      {
        soles::parse_ap(input$file_ap$datapath)
      },
      error = function(e) {
        # Handle potential parsing errors gracefully
        showNotification(paste("Error parsing Academic Plan file:", e$message), type = "error")
        return(NULL) # Return NULL or an empty data frame on error
      }
    )
  })

  # Helper functions moved to R/shiny_extensions_helpers.R

  # Validate SC file input and parse if valid
  observeEvent(input$file, {
    inFile <- input$file
    if (is.null(inFile)) {
      validated_data(NULL) # Clear data if file is deselected
      return()
    }

    # Define expected columns
    expected_cols <- c("availability", "assessment", "u_ticket_contact")
    file_path <- inFile$datapath
    file_ext <- tolower(tools::file_ext(inFile$name))

    # Read header
    header_data <- tryCatch(
      {
        if (file_ext == "csv") {
          readr::read_csv(file_path, n_max = 0, show_col_types = FALSE)
        } else if (file_ext == "xlsx") {
          readxl::read_excel(file_path, n_max = 0)
        } else {
          stop("Unsupported file type. Please upload a CSV or XLSX file.")
        }
      },
      error = function(e) {
        showNotification(paste("Error reading file header:", e$message), type = "error", duration = 10)
        shinyjs::reset("file")
        validated_data(NULL)
        return(NULL)
      }
    )

    if (is.null(header_data)) {
      return()
    } # Exit if header reading failed

    actual_cols <- names(header_data)

    # Validate columns
    is_valid <- all(expected_cols %in% actual_cols)

    if (!is_valid) {
      missing_cols <- setdiff(expected_cols, actual_cols)
      showNotification(
        paste("Invalid Special Considerations file. Missing required columns:", paste(missing_cols, collapse = ", ")),
        type = "error", duration = 10
      )
      shinyjs::reset("file")
      validated_data(NULL)
    } else {
      # If valid, parse the full file
      parsed_data <- tryCatch(
        {
          soles::parse_sc(file_path)
        },
        error = function(e) {
          showNotification(paste("Error parsing Special Considerations file:", e$message), type = "error", duration = 10)
          shinyjs::reset("file")
          return(NULL)
        }
      )

      validated_data(parsed_data) # Store parsed data (or NULL if parsing failed)

      # If parsing succeeded, enable the first filter
      if (!is.null(parsed_data)) {
        shinyjs::enable("uos")
      }
    }
  })

  # Show notification if AP file is uploaded without SC file
  observeEvent(input$file_ap, {
    # Only show notification if AP file is present and SC data is missing
    if (!is.null(input$file_ap) && is.null(validated_data())) {
      showNotification(
        "Special Considerations file is required for filtering and processing. Please upload a valid SC file.",
        type = "warning",
        duration = 10
      )
    }
  })

  # Observe validated data to update UoS choices
  observe({
    df <- validated_data()
    if (is.null(df)) {
      # Disable and clear filters if data is NULL (invalid file or deselected)
      shinyjs::disable("uos")
      shinyjs::disable("assessment")
      shinyjs::disable("semester")
      shinyjs::disable("year")
      shinyjs::disable("process")
      updateSelectInput(session, "uos", choices = NULL, selected = NULL)
      updateSelectInput(session, "assessment", choices = NULL, selected = NULL)
      updateSelectInput(session, "semester", choices = NULL, selected = NULL)
      updateSelectInput(session, "year", choices = NULL, selected = NULL)
    } else {
      # Update UoS based on valid data
      uos_codes <- soles:::extract_uos_codes(df)
      updateSelectInput(session, "uos",
        choices = uos_codes,
        selected = uos_codes[1]
      )
      # Ensure uos is enabled (might have been disabled if previous file was invalid)
      shinyjs::enable("uos")
    }
  })

  # Enable semester when UoS is selected
  observe({
    req(input$uos)
    shinyjs::enable("semester")
  })

  # Enable year when semester is selected
  observe({
    req(input$semester)
    shinyjs::enable("year")
  })

  # Enable assessment when year is selected
  observe({
    req(input$year)
    shinyjs::enable("assessment")
  })

  # Update assessment choices
  observe({
    req(validated_data()) # React to validated data
    req(input$uos, input$semester, input$year)
    df <- validated_data() # Use validated data

    filtered_indices <- vapply(df$availability, function(x) {
      parts <- unlist(str_split(x, "-"))
      if (length(parts) >= 3) {
        parts[1] == input$uos &&
          parts[2] == input$semester &&
          parts[3] == input$year
      } else {
        FALSE
      }
    }, logical(1))

    assessments <- soles:::extract_assessments(df[filtered_indices, ])
    updateSelectInput(session, "assessment",
      choices = assessments,
      selected = assessments[1]
    )
  })

  # Update semester choices
  observe({
    req(validated_data()) # React to validated data
    df <- validated_data() # Use validated data
    selected_uos <- input$uos

    filtered_semesters <- soles:::extract_semesters(df, selected_uos = selected_uos)

    updateSelectInput(session, "semester",
      choices = filtered_semesters,
      selected = filtered_semesters[1]
    )
  })

  # Update year choices
  observe({
    req(validated_data()) # React to validated data
    req(input$uos, input$semester)
    df <- validated_data() # Use validated data

    years <- soles:::extract_years(df, selected_uos = input$uos, selected_semester = input$semester)
    updateSelectInput(session, "year",
      choices = years,
      selected = years[1]
    )
  })

  # Filtered data based on selections
  filtered_data <- eventReactive(input$process, {
    req(validated_data()) # Require validated data before processing
    df <- validated_data() # Use validated data

    # Filter data using all selections
    indices <- vapply(df$availability, function(x) {
      parts <- unlist(str_split(x, "-"))
      if (length(parts) >= 3) {
        parts[1] == input$uos &&
          parts[2] == input$semester &&
          parts[3] == input$year
      } else {
        FALSE
      }
    }, logical(1))

    filtered_df <- df[indices, ]
    filtered_df[filtered_df$assessment == input$assessment, ]
  })

  # Live filtered SC data for summary card (reacts instantly to filters)
  live_filtered_data <- reactive({
    # Require validated data and all filter inputs to be selected
    req(
      validated_data(),
      input$uos, input$uos != "",
      input$semester, input$semester != "",
      input$year, input$year != "",
      input$assessment, input$assessment != ""
    )
    df <- validated_data()

    # Filter data using all selections (same logic as filtered_data)
    indices <- vapply(df$availability, function(x) {
      parts <- unlist(str_split(x, "-"))
      if (length(parts) >= 3) {
        parts[1] == input$uos &&
          parts[2] == input$semester &&
          parts[3] == input$year
      } else {
        FALSE
      }
    }, logical(1))

    filtered_df <- df[indices, ]
    filtered_df[filtered_df$assessment == input$assessment, ]
  })

  # Filtered Academic Plan data based on main filter selections
  filtered_data_ap <- reactive({
    # Require the raw AP data and all filter inputs to be available
    req(data_ap(), input$uos, input$semester, input$year, input$assessment)

    df <- data_ap()
    # Ensure input$year is integer for comparison
    selected_year <- as.integer(input$year)

    # Apply filters - handle potential missing columns gracefully
    if ("UoS Code" %in% names(df)) {
      df <- df %>% filter(`UoS Code` == input$uos)
    }
    if ("Session" %in% names(df)) {
      df <- df %>% filter(`Session` == input$semester)
    }
    if ("Year" %in% names(df)) {
      df <- df %>% filter(`Year` == selected_year)
    }
    if ("Assessment" %in% names(df)) {
      df <- df %>% filter(`Assessment` == input$assessment)
    }

    return(df)
  })

  # Loading state
  loading <- reactiveVal(FALSE)

  # Loading spinner
  output$loading_spinner <- renderUI({
    if (loading()) {
      div(
        class = "position-absolute w-100 h-100 d-flex justify-content-center align-items-center",
        style = "top: 0; left: 0; background: rgba(255,255,255,0.7); z-index: 1000;",
        div(
          class = "spinner-border text-primary",
          role = "status",
          span(class = "visually-hidden", "Loading...")
        )
      )
    }
  })

  # Data Summary Output
  output$data_summary <- renderUI({
    # Use the live filtered data for the summary
    req(live_filtered_data())
    df <- live_filtered_data()

    # Format dates
    orig_due_date <- as.POSIXct(df$due_date[1], format = "%d-%m-%Y %H:%M:%S")
    # Calculate counts per extension date
    date_counts <- table(as.Date(df$extension_in_calendar_days, format = "%d-%m-%Y"))
    sorted_dates <- sort(as.Date(names(date_counts)))

    # Format date strings with counts
    formatted_date_strings <- vapply(sorted_dates, function(date) {
      count <- date_counts[as.character(date)]
      sprintf(
        "%s (%d student%s)",
        format(date, "%d-%b-%Y"),
        count,
        ifelse(count == 1, "", "s")
      )
    }, character(1))

    div(
      class = "small",
      tags$dl(
        class = "row mb-0",
        tags$dt(class = "col-sm-4", "Total Students:"),
        tags$dd(class = "col-sm-8", nrow(df)),
        tags$dt(class = "col-sm-4", "Original Due:"),
        tags$dd(class = "col-sm-8", format(orig_due_date, "%d-%b-%Y")),
        tags$dt(class = "col-sm-4", "Extended Dates:"),
        tags$dd(
          class = "col-sm-8",
          paste0(
            length(sorted_dates), " date(s): ",
            paste(formatted_date_strings, collapse = ", ")
          )
        )
      )
    )
  })

  # Render the table
  output$table <- renderDT(
    {
      if (input$process > 0) {
        loading(TRUE)
        on.exit(loading(FALSE))
        Sys.sleep(0.5)

        # Get filtered data
        df <- filtered_data()

        # Define desired columns for initial selection
        desired_cols <- c(
          "assessment", "u_outcome_type", "due_date",
          "u_ticket_contact", "extension_in_calendar_days"
        )

        # Define columns for the standardized 4-column format
        final_cols <- c(
          "Section Id", "Section name", "Student name", "UniKey"
        )

        # Check which columns exist in the data
        available_cols <- intersect(names(df), desired_cols)

        # Select only available columns from the desired list
        if (length(available_cols) > 0) {
          df <- df %>% select(all_of(available_cols))

          # Format extension_in_calendar_days column if it exists
          if ("extension_in_calendar_days" %in% names(df)) {
            df <- df %>% mutate(
              extension_in_calendar_days = format(
                as.Date(extension_in_calendar_days, format = "%d-%m-%Y"),
                "%d-%b"
              )
            )
          }

          # Split u_ticket_contact into student_name and unikey if it exists
          if ("u_ticket_contact" %in% names(df)) {
            df <- df %>% mutate(
              unikey = stringr::str_trim(stringr::str_extract(u_ticket_contact, "[a-z0-9]+$"))
            )
          }

          # Create the standardized 4-column format
          result_df <- df %>%
            mutate(
              # Column 1: Section Id (always blank)
              `Section Id` = NA_character_,

              # Column 2: Section name (combination of assessment, extension_in_calendar_days, and u_outcome_type)
              `Section name` = paste(
                assessment,
                extension_in_calendar_days,
                u_outcome_type
              ),

              # Column 3: Student name (always blank)
              `Student name` = NA_character_,

              # Column 4: UniKey (from unikey)
              `UniKey` = unikey
            ) %>%
            # Select only the 4 standardized columns
            select(`Section Id`, `Section name`, `Student name`, `UniKey`)

          # --- Combine with Academic Plan Data ---
          ap_data <- ap_data_for_binding()
          if (!is.null(ap_data) && nrow(ap_data) > 0) {
            # Ensure column types are compatible before binding (safer)
            # Convert both to character before binding to avoid type issues
            ap_data <- ap_data %>% mutate(across(everything(), as.character))
            result_df <- result_df %>% mutate(across(everything(), as.character))
            result_df <- dplyr::bind_rows(result_df, ap_data)
          }
          # --- End Combine ---

          result_df
        } else {
          # If none of the desired columns exist, return a dataframe with the standardized columns
          data.frame(
            `Section Id` = character(0),
            `Section name` = character(0),
            `Student name` = character(0),
            `UniKey` = character(0)
          )
        }
      }
    },
    options = list(
      pageLength = 12,
      scrollX = TRUE,
      dom = "frtip", # f=filtering, r=processing, t=table, i=information, p=pagination
      ordering = TRUE,
      searchHighlight = TRUE
    )
  )

  # Render the Academic Plan table
  # Helper function to calculate offset days from Decision string
  calculate_offset_days <- function(decision_string) {
    if (is.na(decision_string)) {
      return(NA_integer_)
    }
    # Match weeks or days
    match_weeks <- str_match(decision_string, "Up to (\\d+)\\s+week(s)?")
    match_days <- str_match(decision_string, "Up to (\\d+)\\s+day(s)?")

    offset <- NA_integer_ # Default to NA

    # Check weeks first
    if (!is.na(match_weeks[1, 2])) {
      num_weeks <- as.integer(match_weeks[1, 2])
      if (num_weeks > 0) {
        offset <- num_weeks * 7
      }
    }
    # If no week offset found, check days
    else if (!is.na(match_days[1, 2])) {
      num_days <- as.integer(match_days[1, 2])
      if (num_days > 0) {
        offset <- num_days
      }
    }

    # Return the calculated offset (NA if no match > 0 found)
    return(offset)
  }

  # --- Reactive for Processed Academic Plan Data ---
  processed_ap_data <- reactive({
    # Require necessary inputs: filtered AP data and live SC data (for original due date)
    req(filtered_data_ap(), live_filtered_data())

    df_ap <- filtered_data_ap()
    df_sc <- live_filtered_data()

    # Check if data is available
    if (is.null(df_ap) || nrow(df_ap) == 0 || is.null(df_sc) || nrow(df_sc) == 0) {
      return(NULL) # Return NULL if data is missing
    }

    # Get the original due date
    orig_due_date_str <- df_sc$due_date[1]
    orig_due_date <- dmy_hms(orig_due_date_str, quiet = TRUE)

    # Check if original due date was parsed correctly
    if (is.na(orig_due_date)) {
      showNotification("Could not parse original due date from Special Considerations file.", type = "warning")
      return(NULL) # Return NULL if date parsing fails
    }
    if (!"Adjustment" %in% names(df_ap)) {
      showNotification("Missing 'Adjustment' column in Academic Plan file needed for 'Decision'.", type = "warning")
      return(NULL) # Return NULL if Adjustment column is missing
    }

    # Calculate extended due date and final columns
    df_ap_final <- df_ap %>%
      rename(Decision = Adjustment) %>%
      rowwise() %>%
      mutate(offset_days = calculate_offset_days(Decision)) %>%
      ungroup() %>%
      filter(!is.na(offset_days)) %>%
      {
        if (nrow(.) > 0) {
          mutate(.,
            ExtendedDueDateRaw = orig_due_date + days(offset_days),
            `Extended Due Date` = format(ExtendedDueDateRaw, "%d-%b")
          )
        } else {
          .
        }
      } %>%
      {
        if (nrow(.) > 0 && all(c("Assessment", "Extended Due Date") %in% names(.))) {
          mutate(., `Section name` = paste(Assessment, `Extended Due Date`, "Academic Plan", sep = " "))
        } else if (nrow(.) > 0) {
          mutate(., `Section name` = "Error: Missing required columns for Section name")
        } else {
          mutate(., `Section name` = character(0))
        }
      } %>%
      select(any_of(c("Unikey", "Section name"))) %>%
      # Rename Unikey to UniKey to match SC data for binding
      rename(UniKey = Unikey)

    # Return NULL if the final df is empty after processing
    if (nrow(df_ap_final) == 0) {
      return(NULL)
    }

    return(df_ap_final)
  })

  # --- Reactive to Prepare AP Data for Binding ---
  ap_data_for_binding <- reactive({
    ap_data <- processed_ap_data()

    if (is.null(ap_data) || nrow(ap_data) == 0) {
      return(NULL)
    }

    # Add missing columns and select in standard order
    ap_data %>%
      mutate(
        `Section Id` = NA_character_,
        `Student name` = NA_character_
      ) %>%
      select(
        any_of(c("Section Id", "Section name", "Student name", "UniKey"))
      )
  })

  # (output$table_ap is removed)

  # Download handler for CSV export
  output$download <- downloadHandler(
    filename = function() {
      # Create filename with UoS code, assessment name, and timestamp
      paste0(
        input$uos, "_",
        gsub("[^a-zA-Z0-9]", "_", input$assessment), "_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".csv"
      )
    },
    content = function(file) {
      # Get the filtered data
      df <- filtered_data()

      # Define desired columns for initial selection
      desired_cols <- c(
        "assessment", "u_outcome_type", "due_date",
        "u_ticket_contact", "extension_in_calendar_days"
      )

      # Define columns for the standardized 4-column format
      final_cols <- c(
        "Section Id", "Section name", "Student name", "UniKey"
      )

      # Check which columns exist in the data
      available_cols <- intersect(names(df), desired_cols)

      # Select only available columns from the desired list
      if (length(available_cols) > 0) {
        df <- df %>% select(all_of(available_cols))

        # Format extension_in_calendar_days column if it exists
        if ("extension_in_calendar_days" %in% names(df)) {
          df <- df %>% mutate(
            extension_in_calendar_days = format(
              as.Date(extension_in_calendar_days, format = "%d-%m-%Y"),
              "%d-%b"
            )
          )
        }

        # Split u_ticket_contact into student_name and unikey if it exists
        if ("u_ticket_contact" %in% names(df)) {
          df <- df %>% mutate(
            unikey = stringr::str_trim(stringr::str_extract(u_ticket_contact, "[a-z0-9]+$"))
          )
        }

        # Create the standardized 4-column format
        export_data <- df %>%
          mutate(
            # Column 1: Section Id (always blank)
            `Section Id` = NA_character_,

            # Column 2: Section name (combination of assessment, extension_in_calendar_days, and u_outcome_type)
            `Section name` = paste(
              assessment,
              extension_in_calendar_days,
              u_outcome_type
            ),

            # Column 3: Student name (always blank)
            `Student name` = NA_character_,

            # Column 4: UniKey (from unikey)
            `UniKey` = unikey
          ) %>%
          # Select only the 4 standardized columns
          select(`Section Id`, `Section name`, `Student name`, `UniKey`)
      } else {
        # If none of the desired columns exist, create an empty dataframe with the standardized columns
        export_data <- data.frame(
          `Section Id` = character(0),
          `Section name` = character(0),
          `Student name` = character(0),
          `UniKey` = character(0)
        )
      }

      # Write to CSV file
      write.csv(export_data, file, row.names = FALSE)
    }
  )

  # Record count and process button state
  output$record_count <- renderText({
    all_selected <- !is.null(input$file) &&
      !is.null(input$uos) && input$uos != "" &&
      !is.null(input$assessment) && input$assessment != "" &&
      !is.null(input$semester) && input$semester != "" &&
      !is.null(input$year) && input$year != ""

    if (all_selected) {
      shinyjs::enable("process")
    } else {
      shinyjs::disable("process")
    }

    # Check file states and filter selections
    sc_file_missing <- is.null(validated_data())
    ap_file_present <- !is.null(input$file_ap)

    if (sc_file_missing) {
      shinyjs::disable("process") # Ensure process is disabled
      if (ap_file_present) {
        # Specific message if only AP file is present
        "Special Considerations file is required for filtering and processing."
      } else {
        # Default message if no SC file is present
        "Please select the Special Considerations data file."
      }
    } else if (!all_selected) {
      shinyjs::disable("process") # Ensure process is disabled
      "Select all filters to enable processing."
    } else if (input$process == 0) {
      "Click 'Process Data' to view results"
    } else {
      filtered <- filtered_data()
      sprintf(
        "Found %d record%s",
        nrow(filtered),
        if (nrow(filtered) == 1) "" else "s"
      )
    }
  })
}
