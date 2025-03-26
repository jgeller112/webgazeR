#' Compute Gaze Dispersion
#'
#' Calculates centroid anddispersion from gaze data.
#'
#' @param data A data frame or tibble.
#' @param x A string: name of the column with X gaze coordinates.
#' @param y A string: name of the column with Y gaze coordinates.
#' @param grouping_vars A character vector of column names to group by (e.g., subject, condition, trial).
#'
#' @return A tibble with centroid_x, centroid_y, dispersion, and log_dispersion for each group.
#' @export
#'
#'
gaze_dispersion <- function(data, x, y, grouping_vars) {
  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) %>%
    dplyr::summarize(
      centroid_x = mean(.data[[x]], na.rm = TRUE),
      centroid_y = mean(.data[[y]], na.rm = TRUE),
      dispersion = mean(sqrt((.data[[x]] - centroid_x)^2 + (.data[[y]] - centroid_y)^2), na.rm = TRUE),
      log_dispersion = log(dispersion),
      .groups = "drop"
    )
}

