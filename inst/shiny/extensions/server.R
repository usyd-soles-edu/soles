library(shiny)
library(DT)
library(dplyr)
library(stringr)

function(input, output, session) {
  # Reactive value to store the raw data
  data <- reactive({
    req(input$file)
    soles::parse_sc(input$file$datapath)
  })

  # Helper function to extract UoS codes from availability column
  extract_uos_codes <- function(df) {
    availability_col <- if ("UoS (availability)" %in% names(df)) {
      "UoS (availability)"
    } else {
      "availability"
    }

    codes <- vapply(df[[availability_col]], function(x) {
      parts <- unlist(str_split(x, "-"))
      if (length(parts) >= 1) parts[1] else NA_character_
    }, character(1))
    sort(unique(codes[!is.na(codes)]))
  }

  # Helper function to extract semesters
  extract_semesters <- function(df) {
    availability_col <- if ("UoS (availability)" %in% names(df)) {
      "UoS (availability)"
    } else {
      "availability"
    }

    semesters <- vapply(df[[availability_col]], function(x) {
      parts <- unlist(str_split(x, "-"))
      if (length(parts) >= 2) parts[2] else NA_character_
    }, character(1))
    sort(unique(semesters[!is.na(semesters)]))
  }

  # Helper function to extract years
  extract_years <- function(df) {
    availability_col <- if ("UoS (availability)" %in% names(df)) {
      "UoS (availability)"
    } else {
      "availability"
    }

    years <- vapply(df[[availability_col]], function(x) {
      parts <- unlist(str_split(x, "-"))
      if (length(parts) >= 3) parts[3] else NA_character_
    }, character(1))
    sort(unique(years[!is.na(years)]))
  }

  # Helper function to extract assessments
  extract_assessments <- function(df) {
    if (!"assessment" %in% names(df)) {
      return(character(0))
    }
    sort(unique(df$assessment[!is.na(df$assessment)]))
  }

  # Handle filter states based on file input
  observe({
    if (is.null(input$file)) {
      shinyjs::disable("uos")
      shinyjs::disable("assessment")
      shinyjs::disable("semester")
      shinyjs::disable("year")
      shinyjs::disable("process")

      updateSelectInput(session, "uos", choices = NULL)
      updateSelectInput(session, "assessment", choices = NULL)
      updateSelectInput(session, "semester", choices = NULL)
      updateSelectInput(session, "year", choices = NULL)
    } else {
      shinyjs::enable("uos")
      df <- data()
      uos_codes <- extract_uos_codes(df)
      updateSelectInput(session, "uos",
        choices = uos_codes,
        selected = uos_codes[1]
      )
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
    req(data())
    req(input$uos, input$semester, input$year)
    df <- data()

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

    assessments <- extract_assessments(df[filtered_indices, ])
    updateSelectInput(session, "assessment",
      choices = assessments,
      selected = assessments[1]
    )
  })

  # Update semester choices
  observe({
    req(data())
    df <- data()
    selected_uos <- input$uos

    semesters <- vapply(df$availability, function(x) {
      parts <- unlist(str_split(x, "-"))
      if (length(parts) >= 2 && parts[1] == selected_uos) {
        parts[2]
      } else {
        NA_character_
      }
    }, character(1))
    filtered_semesters <- sort(unique(semesters[!is.na(semesters)]))

    updateSelectInput(session, "semester",
      choices = filtered_semesters,
      selected = filtered_semesters[1]
    )
  })

  # Update year choices
  observe({
    req(data())
    req(input$uos, input$semester)
    df <- data()

    years <- vapply(df$availability, function(x) {
      parts <- unlist(str_split(x, "-"))
      if (length(parts) >= 3 &&
        parts[1] == input$uos &&
        parts[2] == input$semester) {
        parts[3]
      } else {
        NA_character_
      }
    }, character(1))

    years <- sort(unique(years[!is.na(years)]))
    updateSelectInput(session, "year",
      choices = years,
      selected = years[1]
    )
  })

  # Filtered data based on selections
  filtered_data <- eventReactive(input$process, {
    req(data())
    df <- data()

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
    req(filtered_data())
    df <- filtered_data()

    # Format dates
    orig_due_date <- as.POSIXct(df$due_date[1], format = "%d-%m-%Y %H:%M:%S")
    extension_dates <- as.Date(unique(df$extension_in_calendar_days), format = "%d-%m-%Y")

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
            length(extension_dates), " date(s): ",
            paste(format(sort(extension_dates), "%d-%b-%Y"), collapse = ", ")
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

    if (is.null(input$file)) {
      "Please select a data file"
    } else if (!all_selected) {
      "Select all filters to enable processing"
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
