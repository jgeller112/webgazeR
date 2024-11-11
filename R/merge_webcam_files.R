#' Extract and Merge Gorilla Webcam Files with Optional AOI Extraction
#'
#' This function reads in multiple Gorilla webcam files, merges them, and optionally extracts area of interest (AOI) data.
#' It cleans up column names, and allows filtering by screen index. If `extract_aois` is TRUE, it extracts specific AOI-related
#' columns (`zone_name`, `zone_x_normalized`, `zone_y_normalized`, `zone_width_normalized`, and `zone_height_normalized`) and
#' returns distinct rows for these columns.
#'
#' @param file_paths A list of file paths to webcam files (in .xlsx format).
#' @param screen_index An optional screen index to filter the data by. If NULL, the filter will be ignored.
#' @param extract_aois Logical. If TRUE, extracts AOI-related columns and returns distinct rows for them.
#'
#' @return A dataframe containing the merged and processed data from the webcam files. If `extract_aois` is TRUE, it returns
#' a dataframe with distinct AOI-related columns.
#' @import janitor
#' @import dplyr
#' @import readxl
#' @export
#'
#' @examples
#' # Example usage:
#' # file_paths <- c("file1.xlsx", "file2.xlsx")
#' # merged_data <- extract_gorilla_aois(file_paths, screen_index = 4, extract_aois = FALSE)
#' # aoi_data <- extract_gorilla_aois(file_paths, screen_index = 4, extract_aois = TRUE)

merge_webcam_files <- function(file_paths, screen_index = NULL) {

  # Function to ensure required packages are installed
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

  # Read in the files, bind them together, and clean up
  merged_data <- lapply(file_paths, readxl::read_excel) %>%
    dplyr::bind_rows() %>%
    janitor::clean_names() %>%
    dplyr::filter(
      type == "prediction",
      if (!is.null(screen_index)) screen_index %in% screen_index else TRUE
    ) %>%
    dplyr::rename("trial" = "spreadsheet_row",
                  "time" = "time_elapsed") %>%
    dplyr::mutate(subject = as.factor(participant_id),
                  trial = as.factor(trial)) %>%
    dplyr::select(-participant_id)

  return(merged_data)
}
