#' Check if URLs exist - optimized for 404 detection only
#'
#' @param urls character vector - URLs to check
#'
#' @import httr2
#'
#' @return Character vector of non-404 URLs
check_urls <- function(urls) {
  # Input validation
  if (!is.character(urls)) {
    stop("Input must be a character vector of URLs")
  }

  # Pre-allocate results vector
  n <- length(urls)
  results <- logical(n)

  # Check URLs
  error_pattern <- "Error \\(404\\)|Page not found"

  for (i in seq_len(n)) {
    req <- request(urls[i])
    result <- tryCatch(
      {
        resp <- req |>
          req_timeout(seconds = 10) |>
          req_retry(max_tries = 2) |>
          req_perform()

        status <- resp_status(resp)

        if (status == 200) {
          content <- resp_body_string(resp)
          !grepl(error_pattern, content)
        } else {
          FALSE
        }
      },
      error = function(e) FALSE
    )
    results[i] <- result
  }

  # Get valid URLs
  valid_urls <- urls[results]

  # If no valid URLs found, inform about limitations and stop
  if (length(valid_urls) == 0) {
    stop(paste(
      "No valid URLs found. Note: This function supports only Normal",
      "Day (ND) units.\n  For Block Intensive, OLE or other special",
      "semester Units, please supply the full web address to parse."
    ))
  }

  # If only one URL, return it
  if (length(valid_urls) == 1) {
    return(valid_urls)
  }

  # If multiple URLs, ask user to choose
  cat("Multiple valid Unit outlines found. Please select one:\n\n")
  for (i in seq_along(valid_urls)) {
    # Parse URL components
    url_parts <- strsplit(valid_urls[i], "/")[[1]]
    unit_code <- url_parts[length(url_parts) - 1]

    # Parse the last component
    details <- strsplit(url_parts[length(url_parts)], "-")[[1]]
    year <- details[1]
    semester <- ifelse(details[2] == "S1C", "Semester 1", "Semester 2")
    mode <- ifelse(details[4] == "CC", "On-Campus", "Remote")

    # Format output
    formatted_text <- sprintf("%s (%s) %s %s", unit_code, year, semester, mode)
    cat(i, ": ", formatted_text, "\n")
  }

  # Get user input
  choice <- as.integer(readline("Enter the number of your choice: "))

  # Validate input
  while (is.na(choice) || choice < 1 || choice > length(valid_urls)) {
    cat("Invalid choice. Please enter a number between 1 and", length(valid_urls), "\n")
    choice <- as.integer(readline("Enter the number of your choice: "))
  }

  # Return selected URL
  valid_urls[choice]
}
