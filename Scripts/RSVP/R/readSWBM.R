#' Read a Block from the SWBM Input File
#'
#' This function reads a specific block from the Soil Water Budget Model (SWBM) input file and returns the key-value pairs as a named list.
#' The function searches for the specified block, which can appear anywhere in the file, and parses the key-value pairs in that block.
#'
#' @param file_path Character. The path to the SWBM input file.
#' @param block_name Character. The name of the block to read.
#'
#' @return A named list where the keys are the parameter names from the block and the values are the corresponding settings.
#' The values are returned as numeric if they can be coerced to numeric, otherwise as characters.
#'
#' @details
#' This function searches for a block in the SWBM input file using the block's "BEGIN" and "END" delimiters (e.g., `BEGIN DISCRETIZATION` and `END DISCRETIZATION`).
#' It collects all the key-value pairs inside the block, ignoring comments and empty lines. Key-value pairs can appear in any order.
#'
#' @examples
#' # Example usage
#' run_dir <- file.path('../../Run/svihm.swbm')
#' file_path <- file.path(run_dir,'/SWBM/)
#' discretization_settings <- read_swbm_block(file_path, block_name = "DISCRETIZATION")
#'
#' @export
read_swbm_block <- function(file_path, block_name) {
  # Read the file into a character vector
  lines <- readLines(file_path)

  # Initialize variables to store the block lines and status flags
  block_lines <- c()
  in_block <- FALSE

  # Loop through the lines to find the target block
  for (line in lines) {
    # Check if we've found the start of the target block
    if (grepl(paste0("BEGIN ", block_name), line, ignore.case = TRUE)) {
      in_block <- TRUE
      next
    }

    # Check if we've reached the end of the block
    if (grepl(paste0("END ", block_name), line, ignore.case = TRUE) && in_block) {
      break
    }

    # If we're inside the block, collect the lines
    if (in_block) {
      block_lines <- c(block_lines, line)
    }
  }

  # Remove comments and empty lines from the block
  block_lines <- block_lines[!grepl("^\\s*#|^\\s*$", block_lines)]

  # Parse the block lines into key/value pairs
  key_value_pairs <- list()
  for (line in block_lines) {
    # Split by whitespace to extract key and value
    matches <- strsplit(trimws(line), "\\s+", perl = TRUE)[[1]]

    # If there is only one element, treat it as a key-only statement
    if (length(matches) == 1) {
      key <- matches[1]
      key_value_pairs[[key]] <- TRUE  # Store the value as "true"

      # If there are two or more elements, treat the first as the key and the second as the value
    } else if (length(matches) >= 2) {
      key <- matches[1]
      value <- matches[2]

      # Try to convert value to numeric; if it fails, store it as a character
      numeric_value <- suppressWarnings(as.numeric(value))
      if (!is.na(numeric_value)) {
        key_value_pairs[[key]] <- numeric_value
      } else {
        key_value_pairs[[key]] <- value
      }
    }
  }

  return(key_value_pairs)
}
