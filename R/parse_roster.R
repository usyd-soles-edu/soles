#' Parse Roster File for a Specific Unit
#'
#' This function provides a unified interface for parsing roster files from different units.
#' It routes to unit-specific parsing functions based on the unit parameter.
#'
#' @param path Path to the roster file.
#' @param unit Character string specifying the unit (e.g., "BIOL1007").
#' @param output_path Path for CSV output. If NULL (default), automatically writes to a logs directory
#'   with filename format "{unit}-{timestamp}.csv". If specified, writes to the provided path.
#'   If set to FALSE, no file is written.
#' @param verbose Logical, whether to print informational messages. Default TRUE.
#' @return A data frame with standardized roster data.
#' @importFrom glue glue
#' @importFrom utils write.csv
#' @import lgr
#' @export
parse_roster <- function(path, unit, output_path = FALSE, verbose = TRUE) {
  # Validate inputs
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }
  
  # Convert unit to uppercase for consistency
  unit <- toupper(unit)
  
  # Helper function to get relative path
  get_relative_path <- function(absolute_path) {
    tryCatch({
      wd <- getwd()
      # Normalize paths to forward slashes for comparison
      norm_wd <- gsub("\\\\", "/", wd)
      norm_path <- gsub("\\\\", "/", absolute_path)
      
      # If path is within working directory, make it relative
      if (grepl(paste0("^", norm_wd), norm_path)) {
        gsub(paste0("^", norm_wd, "/?"), "", norm_path)
      } else {
        absolute_path
      }
    }, error = function(e) absolute_path)
  }
  
  relative_path <- get_relative_path(path)
  if (verbose) lgr$info(glue::glue("Parsing roster for {unit} from '{relative_path}'..."))
  
  # Route to unit-specific parser
  result <- switch(unit,
    "BIOL1007" = parse_biol1007_roster(path, verbose = verbose),
    # Add more units as needed
    stop("Unit not supported: ", unit)
  )
  
  # Add source file attribute
  attr(result, "source_file") <- path
  
  # Add unit attribute for comparison
  attr(result, "unit") <- unit
  
  # Write to CSV if output_path is specified or NULL (default behavior)
  if (identical(output_path, FALSE)) {
    if (verbose) lgr$info("Processing complete and saved in memory...")
  } else if (is.null(output_path)) {
    # Create logs directory in the same directory as the input file
    input_dir <- dirname(path)
    logs_dir <- file.path(input_dir, "logs")
    
    # Get relative path for logs directory
    relative_logs_dir <- get_relative_path(logs_dir)
    
    if (!dir.exists(logs_dir)) {
      if (verbose) lgr$info(glue::glue("Creating logs directory at '{relative_logs_dir}'..."))
      dir.create(logs_dir, recursive = TRUE)
    }
    
    # Create timestamp
    timestamp <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
    filename <- glue::glue("{unit}-{timestamp}.csv")
    output_path <- file.path(logs_dir, filename)
  if (verbose) lgr$info(glue::glue("Generated output filename: '{filename}'..."))
    
    # Get relative path for output file
    relative_output_path <- get_relative_path(output_path)
    
    # Write result to CSV
  if (verbose) lgr$info(glue::glue("Writing parsed roster to '{relative_output_path}'..."))
    utils::write.csv(result, output_path, row.names = FALSE)
  if (verbose) lgr$info(glue::glue("Successfully wrote parsed roster to '{relative_output_path}'..."))
  } else {
    # Get relative path for output file
    relative_output_path <- get_relative_path(output_path)
    
    if (verbose) lgr$info(glue::glue("Using specified output path: '{relative_output_path}'..."))
    
    # Write result to CSV
  if (verbose) lgr$info(glue::glue("Writing parsed roster to '{relative_output_path}'..."))
    utils::write.csv(result, output_path, row.names = FALSE)
  if (verbose) lgr$info(glue::glue("Successfully wrote parsed roster to '{relative_output_path}'..."))
  }
  
  return(result)
}