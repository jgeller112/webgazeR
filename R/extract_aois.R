##' Extract AOI-related Columns from Webcam Files and Calculate Locations
#'
#' This function reads in multiple Gorilla webcam files, extracts the
#' `loc`, `x_normalised`, `y_normalised`, `width_normalised`,
#' and `height_normalised` columns, and calculates the bounding box coordinates
#' for the AOIs. It also rounds all numeric columns to 3 decimal places.
#'
#' @param file_paths A list of file paths to webcam files (in .xlsx format).
#' @return A dataframe containing distinct rows with AOI-related columns and calculated coordinates.
#' @export
#'
#' @examples
#' # Example usage:
#' # file_paths <- c("file1.xlsx", "file2.xlsx")
#' # aoi_data <- extract_aois(file_paths)

extract_aois <- function(file_paths, zone_names=NULL) {

  # Read in the files, bind them together, and extract AOI columns
  aoi_data <- lapply(file_paths, readxl::read_excel) %>%
    dplyr::bind_rows() %>%
    janitor::clean_names() %>%
    dplyr::filter(zone_name %in% zone_names)%>%
    dplyr::select(loc = zone_name,
                  x_normalized = zone_x_normalised,
                  y_normalized = zone_y_normalised,
                  width_normalized = zone_width_normalised,
                  height_normalized = zone_height_normalised) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 2))) %>%

    dplyr::distinct()  # Get distinct rows to remove duplicates

  # Create the aoi_loc dataframe and calculate the bounding box coordinates
  aoi_loc <- aoi_data %>%
    dplyr::mutate(
      xmin = x_normalized,
      ymin = y_normalized,
      xmax = x_normalized + width_normalized,
      ymax = y_normalized + height_normalized
    )
  return(aoi_loc)
}
