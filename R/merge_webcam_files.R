#' Read and merge webcam files
#'
#' This function reads in multiple webcam files, merges them, and cleans up column names.
#'
#' @param file_paths A list of file paths to webcam files (in .xlsx format).
#' @export
#'
#' @return A dataframe containing the merged and processed data from the webcam files.
#' @import janitor
#'


merge_webcam_files <- function(file_paths, screen_index=NULL) {

  ensure_packages_installed <- function(packages) {
    installed <- installed.packages()[, "Package"]
    to_install <- packages[!(packages %in% installed)]
    if (length(to_install)) {
      install.packages(to_install)
    }
  }

  # Ensure required packages are installed
  required_packages <- c("janitor", "tidyverse", "readxl")
  ensure_packages_installed(required_packages)

  # Load required libraries
  lapply(required_packages, require, character.only = TRUE)

  # Ensure required packages are installed
  required_packages <- c("janitor", "readxlsx", "tidyverse", "readxl")
  ensure_packages_installed(required_packages)

  # Read in the files, bind them together, and clean up
  merged_data <- lapply(file_paths, readxl::read_excel) %>%
    dplyr::bind_rows() %>%
    janitor::clean_names() %>%
    dplyr::filter(type == "prediction", screen_index == screen_index) %>%  # Filter for type and screen
    dplyr::rename("trial" = "spreadsheet_row",
                  "time" = "time_elapsed") %>%
    dplyr::mutate(subject = factor(participant_id),
                  trial = factor(trial))

  return(merged_data)
}
