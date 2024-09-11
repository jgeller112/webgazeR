#' Calculate Out-of-Bounds Proportion by Subject
#'
#' This function calculates the number and percentage of points that fall outside a specified range
#' (0, 1) for both X and Y coordinates, grouped by subject.
#'
#' @param data A data frame containing gaze data.
#' @param subject_col A string specifying the name of the column that contains the subject identifier. Default is "subject".
#' @param x_col A string specifying the name of the column that contains the X coordinate. Default is "x_pred_normalised".
#' @param y_col A string specifying the name of the column that contains the Y coordinate. Default is "y_pred_normalised".
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{subject}{The subject identifier.}
#'   \item{total_points}{The total number of points for the subject.}
#'   \item{outside_count}{The number of points outside the range for both X and Y coordinates.}
#'   \item{x_outside_count}{The number of points outside the range for the X coordinate.}
#'   \item{y_outside_count}{The number of points outside the range for the Y coordinate.}
#'   \item{x_outside_percentage}{The percentage of points outside the range for the X coordinate.}
#'   \item{y_outside_percentage}{The percentage of points outside the range for the Y coordinate.}
#' }
#'
#' @examples
#' \dontrun{
#'   # Example data
#'   data <- data.frame(
#'     subject = rep(1:2, each = 100),
#'     x_pred_normalised = runif(200, -0.5, 1.5),
#'     y_pred_normalised = runif(200, -0.5, 1.5)
#'   )
#'
#'   # Calculate out-of-bounds proportion by subject
#'   results <- calculate_out_of_bounds_by_subject(data)
#'   print(results)
#' }
#' @export
gaze_oob <- function(data, subject_col = "subject", x_col = "x_pred_normalised", y_col = "y_pred_normalised") {
  # Group by subject and calculate the number of points within and outside the range
  results <- data %>%
    dplyr::group_by(.data[[subject_col]]) %>%
    dplyr::summarise(
      total_points = n(),
      outside_count = sum((.data[[x_col]] <= 0 | .data[[x_col]] >= 1 |
                             .data[[y_col]] <= 0 | .data[[y_col]] >= 1)),
      total_missing_percentage = (outside_count / total_points) * 100,
      x_outside_count = sum(.data[[x_col]] <= 0 | .data[[x_col]] >= 1),
      y_outside_count = sum(.data[[y_col]] <= 0 | .data[[y_col]] >= 1),
      x_outside_percentage = (x_outside_count / total_points) * 100,
      y_outside_percentage = (y_outside_count / total_points) * 100
    )

  # Print the percentages by subject
  results %>%
    dplyr::mutate(
      message = paste0(
        "Subject: ", .data[[subject_col]], "\n",
        "Total points: ", total_points, "\n",
        "Points outside the range: ", outside_count, "\n",
        "Total missing percentage: ", round(total_missing_percentage, 2), "%\n",
        "X: ", round(x_outside_percentage, 2), "%\n",
        "Y: ", round(y_outside_percentage, 2), "%\n"
      )
    ) %>%
    dplyr::pull(message) %>%
    cat(sep = "\n\n")

  return(results)
}
