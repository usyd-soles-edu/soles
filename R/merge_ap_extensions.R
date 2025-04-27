#' Merge Academic Plan Extensions with Student Data
#'
#' Reads Academic Plan (AP) extension data, processes it based on context derived
#' from an input student data frame, calculates revised due dates, standardizes
#' columns, and merges it with the original student data.
#'
#' @param df A data frame containing student data, typically from Canvas or
#'   similar sources. Must include columns like 'uos', 'year', 'session',
#'   'assessment', 'assessment_due_date', 'uo_s_availability', 'mode', 'location'.
#'   The function derives context (most common values) from these columns.
#' @param ap_path A character string specifying the file path to the Academic Plan
#'   spreadsheet (e.g., an Excel file).
#'
#' @return A single data frame combining the original `df` rows and the processed
#'   rows from the Academic Plan data. Columns from the AP data are standardized
#'   to match or augment the columns in `df`. A new column `outcome_type`
#'   distinguishes the origin ("Academic Plan" for AP rows, potentially NA or
#'   existing values for original rows). A `revised_due_date` column is added
#'   based on AP extensions.
#'
#' @importFrom dplyr filter mutate case_when select bind_rows if_else %>%
#' @importFrom stringr str_detect str_extract
#' @importFrom lubridate as_date days
#' @importFrom logger log_info
#' @importFrom stats setNames
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'student_data' is a data frame with student records
#' # and 'academic_plan.xlsx' contains the extension details.
#' combined_data <- merge_ap_extensions(student_data, "path/to/academic_plan.xlsx")
#' print(head(combined_data))
#' }
merge_ap_extensions <- function(df, ap_path = NULL) {
  # --- Input Validation ---
  # --- Helper function to ensure standard output columns exist ---
  ensure_output_cols <- function(input_df) {
    cols_to_ensure <- c(
      "revised_due_date" = as.Date(NA),
      "state" = NA_character_,
      "outcome_type" = NA_character_
    )
    for (col_name in names(cols_to_ensure)) {
      if (!col_name %in% names(input_df)) {
        input_df[[col_name]] <- cols_to_ensure[[col_name]]
      }
    }
    return(input_df)
  }

  if (is.null(ap_path)) {
    logger::log_warn("Academic Plan file path ('ap_path') was not provided. Returning original data frame.")
    return(ensure_output_cols(df))
  }
  if (!file.exists(ap_path)) {
    logger::log_warn("Academic Plan file not found at: {ap_path}. Returning original data frame.")
    return(ensure_output_cols(df))
  }
  # Basic check for required columns in df (can be expanded)
  required_cols <- c(
    "uos", "year", "session", "assessment", "assessment_due_date",
    "uo_s_availability", "mode", "location"
  )
  if (!all(required_cols %in% names(df))) {
    missing_cols <- required_cols[!required_cols %in% names(df)]
    stop("Input data frame 'df' is missing required columns: ", paste(missing_cols, collapse = ", "))
  }


  # --- Helper function to get the most frequent value ---
  get_most_common <- function(vec) {
    if (length(vec) > 0 & !all(is.na(vec))) {
      vec_char <- as.character(vec[!is.na(vec)]) # Ensure character and remove NAs for table
      if (length(vec_char) > 0) {
        counts <- table(vec_char)
        return(names(counts)[which.max(counts)])
      }
    }
    return(NA_character_) # Return NA character if no common value found
  }

  # --- Extract Context from Input Data (df) ---
  uos <- get_most_common(df$uos)
  year <- get_most_common(df$year) # Note: This will be character
  session <- get_most_common(df$session)
  assessment <- get_most_common(df$assessment)
  most_common_due_date <- get_most_common(df$assessment_due_date)

  # Safely parse the due date
  due_date <- as.Date(NA)
  if (!is.na(most_common_due_date)) {
    tryCatch(
      {
        due_date <- lubridate::as_date(most_common_due_date)
      },
      warning = function(w) {
        logger::log_warn("Could not parse most common due date '{most_common_due_date}' as Date. Proceeding without due date context for AP.", conditionMessage(w))
      },
      error = function(e) {
        logger::log_error("Error parsing most common due date '{most_common_due_date}': {conditionMessage(e)}")
        stop("Failed to parse assessment due date from input data.") # Optional: stop execution
      }
    )
  }

  logger::log_info("Context values - uos: {uos}, year: {year}, session: {session}, assessment: {assessment}, due_date: {due_date}")

  # --- Read and Process Academic Plan Data ---
  logger::log_info("Reading Academic Plan file from: {ap_path}")
  ap_raw <- tryCatch(
    {
      parse_ap(ap_path) # Assumes parse_ap handles read errors internally or returns structure
    },
    error = function(e) {
      logger::log_error("Failed to parse Academic Plan file at '{ap_path}': {conditionMessage(e)}")
      stop("Error reading or parsing the Academic Plan file.")
    }
  )

  # Check if essential columns exist in ap_raw before proceeding
  required_ap_cols <- c(
    "Year", "Session", "UoS Code", "Assessment",
    "Assessment Adjustment - Assignment Extension",
    "Preferred Name", "Family Name", "Unikey"
  )
  if (!all(required_ap_cols %in% names(ap_raw))) {
    missing_ap_cols <- required_ap_cols[!required_ap_cols %in% names(ap_raw)]
    stop("Academic Plan data is missing required columns: ", paste(missing_ap_cols, collapse = ", "))
  }


  # Filter AP data to only include rows with quantifiable extensions
  initial_ap_rows <- nrow(ap_raw)
  ap_filtered_extensions <- ap_raw |>
    dplyr::filter(
      stringr::str_detect(`Assessment Adjustment - Assignment Extension`, stringr::regex("Up to \\\\d+ (day|week)s?", ignore_case = TRUE))
    )
  rows_removed <- initial_ap_rows - nrow(ap_filtered_extensions)
  if (rows_removed > 0) {
    logger::log_info("Removed {rows_removed} AP entries lacking specific 'Up to [number] day/week' extension format.")
  }


  logger::log_info("Processing Academic Plan data...")
  ap_processed <- ap_filtered_extensions |>
    # Filter AP data based on context derived from df
    dplyr::filter(
      Year == as.integer(year), # Convert context year back to integer for filtering AP data
      Session == session,
      `UoS Code` == uos,
      Assessment == assessment
    ) |>
    # Calculate extension duration and revised due date
    dplyr::mutate(
      extension_in_days = dplyr::case_when(
        stringr::str_detect(`Assessment Adjustment - Assignment Extension`, stringr::regex("Up to", ignore_case = TRUE)) ~ {
          num <- suppressWarnings(as.numeric(stringr::str_extract(`Assessment Adjustment - Assignment Extension`, "\\d+")))
          unit <- stringr::str_extract(`Assessment Adjustment - Assignment Extension`, stringr::regex("week|day", ignore_case = TRUE))
          unit <- tolower(unit) # Ensure lowercase for comparison

          dplyr::case_when(
            !is.na(num) & !is.na(unit) & unit == "week" ~ num * 7,
            !is.na(num) & !is.na(unit) & unit == "day" ~ num,
            TRUE ~ NA_real_ # Handle cases where number or unit extraction failed
          )
        },
        TRUE ~ NA_real_ # Default if "Up to" pattern doesn't match
      ),
      revised_due_date = dplyr::if_else(!is.na(due_date) & !is.na(extension_in_days),
        due_date + as.difftime(extension_in_days, units = "days"),
        as.Date(NA)
      )
    ) |>
    # Standardize columns to match/augment df structure
    dplyr::mutate(
      state = `Assessment Adjustment - Assignment Extension`, # Keep original text
      # Use first value from df as context for these potentially constant columns
      uo_s_availability = if ("uo_s_availability" %in% names(df)) df$uo_s_availability[1] else NA_character_,
      mode = if ("mode" %in% names(df)) df$mode[1] else NA_character_,
      location = if ("location" %in% names(df)) df$location[1] else NA_character_,
      assessment_due_date = due_date, # Assign the common due date from df
      outcome_type = "Academic Plan", # Hardcode origin
      name = paste0(`Preferred Name`, " ", `Family Name`),
      # Ensure year is character for binding with df$year (which came from get_most_common)
      year = as.character(Year)
    ) |>
    # Select and rename columns for the final structure before binding
    dplyr::select(
      state,
      uo_s_availability,
      uos = `UoS Code`,
      session = Session,
      year, # Already converted to character
      mode,
      location,
      assessment = Assessment,
      assessment_due_date,
      outcome_type,
      revised_due_date,
      name,
      unikey = Unikey
    )

  logger::log_info("Finished processing {nrow(ap_processed)} Academic Plan entries matching context.")

  # --- Combine Original Data and Processed AP Data ---
  # Ensure df also has the columns added to ap_processed if they don't exist
  # (e.g., revised_due_date, outcome_type, state) to avoid type issues during bind
  # Ensure df has the standard output columns before binding
  # This handles cases where ap_processed might be empty after filtering
  df <- ensure_output_cols(df)


  # Ensure column types are compatible before binding (especially important columns)
  # We already handled 'year' (character). Check others if needed.
  # Example: Ensure 'assessment_due_date' is Date in both
  if (!inherits(df$assessment_due_date, "Date")) {
    df$assessment_due_date <- suppressWarnings(lubridate::as_date(df$assessment_due_date))
    logger::log_info("Converted 'assessment_due_date' in original df to Date type.")
  }
  if (!inherits(ap_processed$assessment_due_date, "Date")) {
    ap_processed$assessment_due_date <- suppressWarnings(lubridate::as_date(ap_processed$assessment_due_date))
    logger::log_info("Converted 'assessment_due_date' in ap_processed to Date type.")
  }


  logger::log_info("Combining original data ({nrow(df)} rows) and processed AP data ({nrow(ap_processed)} rows).")
  combined_data <- dplyr::bind_rows(df, ap_processed)

  logger::log_info("Finished combining data. Total rows: {nrow(combined_data)}.")

  return(combined_data)
}
