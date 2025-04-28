#' Merge and Process Webcam Eye-Tracking Files
#'
#' This function reads, merges, and processes webcam eye-tracking files.
#' It standardizes column names (subject, trial, time, x, y) for universal use.
#' Supports .csv, .tsv, and .xlsx formats.
#'
#' @param file_paths A list of file paths to webcam files.
#' @param screen_index Optional. If provided, filters data by one or more screen indices (for Gorilla).
#' @param kind The data collection platform. Options: "gorilla" (default), "labvanced".
#' @param col_map A named list mapping your current columns to WebGazer names: `subject`, `trial`, `time`, `x`, `y`.
#' @return A dataframe with standardized columns depending on the platform.
#' @export

merge_webcam_files <- function(file_paths, screen_index = NULL, kind = "gorilla",
                               col_map = list(subject = "participant_id",
                                              trial = "spreadsheet_row",
                                              time = "time_elapsed",
                                              x = "x",
                                              y = "y")) {

  # Read and merge
  merged_data <- purrr::map_dfr(file_paths, ~ {
    if (stringr::str_detect(.x, "\\.csv$")) {
      readr::read_csv(.x, show_col_types = FALSE)
    } else if (stringr::str_detect(.x, "\\.tsv$")) {
      readr::read_tsv(.x, show_col_types = FALSE)
    } else if (stringr::str_detect(.x, "\\.xlsx$")) {
      readxl::read_excel(.x)
    } else {
      stop("Unsupported file type: ", .x)
    }
  }) %>%
    janitor::clean_names()

  # Match col_map to cleaned names
  cleaned_col_map <- purrr::map_chr(col_map, janitor::make_clean_names)

  # Platform-specific cleaning
  if (kind == "gorilla") {
    if (!"type" %in% names(merged_data)) {
      stop("For Gorilla data, the dataset must contain a 'type' column to filter for predictions.")
    }

    merged_data <- merged_data %>%
      dplyr::filter(type == "prediction") %>%
      dplyr::rename(
        subject = all_of(cleaned_col_map[["subject"]]),
        trial   = all_of(cleaned_col_map[["trial"]]),
        time    = all_of(cleaned_col_map[["time"]]),
        x       = all_of(cleaned_col_map[["x"]]),
        y       = all_of(cleaned_col_map[["y"]])
      ) %>%
      dplyr::mutate(
        subject = as.factor(subject),
        trial = as.factor(trial),
        time = as.numeric(time)
      )

    # Remove duplicate times gorilla has duplicate times for some reason can use first instance
    merged_data <- merged_data %>%
      dplyr::group_by(subject, trial, time) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()

    # Apply screen_index filter if requested
    if (!is.null(screen_index) && "screen_index" %in% names(merged_data)) {
      merged_data <- merged_data %>%
        dplyr::filter(screen_index %in% !!screen_index)
    }

    # Final selection of columns (specific for Gorilla)
    merged_data <- merged_data %>%
      dplyr::select(subject, trial, time, x, y, screen_index, type,  face_conf, convergence)

  } else if (kind == "labvanced") {
    merged_data <- merged_data %>%
      dplyr::rename(
        subject = all_of(cleaned_col_map[["subject"]]),
        trial   = all_of(cleaned_col_map[["trial"]]),
        time    = all_of(cleaned_col_map[["time"]]),
        x       = all_of(cleaned_col_map[["x"]]),
        y       = all_of(cleaned_col_map[["y"]])
      ) %>%
      dplyr::mutate(
        subject = as.factor(subject),
        trial = as.factor(trial),
        time = as.numeric(time)
      )

    # Remove duplicate times
    merged_data <- merged_data %>%
      dplyr::group_by(subject, trial, time) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()

    # Final selection of columns for Labvanced
    merged_data <- merged_data %>%
      dplyr::select(subject, trial, time, x, y)

  } else {
    stop("Currently only 'gorilla' and 'labvanced' kinds are implemented.")
  }

  return(merged_data)
}
