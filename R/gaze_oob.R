#' Calculate Out-of-Bounds Proportion by Subject and Trial
#'
#' This function calculates the number and percentage of gaze points that fall outside a specified range (0,1)
#' for both X and Y coordinates, grouped by subject and trial.
#'
#' @param data A data frame containing gaze data.
#' @param subject_col A string specifying the name of the column that contains the subject identifier. Default is "subject".
#' @param trial_col A string specifying the name of the column that contains the trial identifier. Default is "trial".
#' @param x_col A string specifying the name of the column that contains the X coordinate. Default is "x_pred_normalised".
#' @param y_col A string specifying the name of the column that contains the Y coordinate. Default is "y_pred_normalised".
#'
#' @return A list containing two data frames:
#' \describe{
#'   \item{subject_results}{Summary of missingness at the subject level, including total trials, total points, and percentages.}
#'   \item{trial_results}{Summary of missingness at the trial level, including total points, and percentages.}
#' }
#'
#' @examples
#' \dontrun{
#'   # Example data
#'   data <- data.frame(
#'     subject = rep(1:2, each = 10),
#'     trial = rep(1:5, times = 4),
#'     x_pred_normalised = runif(20, -0.5, 1.5),
#'     y_pred_normalised = runif(20, -0.5, 1.5)
#'   )
#'
#'   # Calculate out-of-bounds proportion by subject and trial
#'   results <- gaze_oob(data)
#'
#'   # View results
#'   print(results$subject_results)
#'   print(results$trial_results)
#' }
#'
#' @export
gaze_oob <- function(data, subject_col = "subject", trial_col = "trial",
                     x_col = "x_pred_normalised", y_col = "y_pred_normalised") {
  library(dplyr)

  # Compute missingness at the trial level
  trial_results <- data %>%
    group_by(.data[[subject_col]], .data[[trial_col]]) %>%
    summarise(
      total_points = n(),
      outside_count = sum((.data[[x_col]] <= 0 | .data[[x_col]] >= 1 |
                             .data[[y_col]] <= 0 | .data[[y_col]] >= 1)),
      trial_missing_percentage = (outside_count / total_points) * 100,
      x_outside_count = sum(.data[[x_col]] <= 0 | .data[[x_col]] >= 1),
      y_outside_count = sum(.data[[y_col]] <= 0 | .data[[y_col]] >= 1),
      x_outside_percentage = (x_outside_count / total_points) * 100,
      y_outside_percentage = (y_outside_count / total_points) * 100,
      .groups = "drop"
    )

  # Compute missingness at the subject level
  subject_results <- trial_results %>%
    group_by(.data[[subject_col]]) %>%
    summarise(
      total_trials = n(),
      total_points = sum(total_points),
      outside_count = sum(outside_count),
      subject_missing_percentage = (outside_count / total_points) * 100,
      x_outside_count = sum(x_outside_count),
      y_outside_count = sum(y_outside_count),
      x_outside_percentage = (x_outside_count / total_points) * 100,
      y_outside_percentage = (y_outside_count / total_points) * 100,
      .groups = "drop"
    )

  # Print trial-level missingness summary
  trial_results %>%
    mutate(
      message = paste0(
        "Subject: ", .data[[subject_col]], ", Trial: ", .data[[trial_col]], "\n",
        "Total points: ", total_points, "\n",
        "Points outside range: ", outside_count, "\n",
        "Trial missing percentage: ", round(trial_missing_percentage, 2), "%\n",
        "X: ", round(x_outside_percentage, 2), "%\n",
        "Y: ", round(y_outside_percentage, 2), "%\n"
      )
    )
  # Print subject-level missingness summary
  subject_results %>%
    mutate(
      message = paste0(
        "Subject: ", .data[[subject_col]], "\n",
        "Total trials: ", total_trials, "\n",
        "Total points: ", total_points, "\n",
        "Total missing percentage: ", round(subject_missing_percentage, 2), "%\n",
        "X: ", round(x_outside_percentage, 2), "%\n",
        "Y: ", round(y_outside_percentage, 2), "%\n"
      )
    ) %>%
    pull(message) %>%
    cat(sep = "\n\n")

  return(list(subject_results = subject_results, trial_results = trial_results))
}
