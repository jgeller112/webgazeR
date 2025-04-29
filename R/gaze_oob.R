#' Calculate Out-of-Bounds Proportion by Subject and Trial
#'
#' This function calculates the number and percentage of gaze points that fall outside the screen dimensions,
#' and optionally removes only the out-of-bounds gaze points.
#'
#' @param data A data frame containing gaze data.
#' @param subject_col A string specifying the name of the column that contains the subject identifier. Default is "subject".
#' @param trial_col A string specifying the name of the column that contains the trial identifier. Default is "trial".
#' @param x_col A string specifying the name of the column that contains the X coordinate. Default is "x".
#' @param y_col A string specifying the name of the column that contains the Y coordinate. Default is "y".
#' @param screen_size A numeric vector of length 2 specifying the screen width and height.
#'        Default is c(1, 1) assuming normalized coordinates.
#' @param remove Logical; if TRUE, removes points outside of screen dimensions. Default is FALSE.
#'
#' @return A list containing:
#' \describe{
#'   \item{subject_results}{Summary of missingness at the subject level.}
#'   \item{trial_results}{Summary of missingness at the trial level.}
#'   \item{data_clean}{Dataset with optional removal of out-of-bounds points and missingness annotations.}
#' }
#'
#' @export
gaze_oob <- function(data,
                     subject_col = "subject",
                     trial_col = "trial",
                     x_col = "x",
                     y_col = "y",
                     screen_size = c(1, 1),
                     remove = FALSE) {

  # Unpack screen size
  screen_width <- screen_size[1]
  screen_height <- screen_size[2]

  # Compute missingness at the trial level
  trial_results <- data %>%
    group_by(.data[[subject_col]], .data[[trial_col]]) %>%
    summarise(
      total_points = n(),
      outside_count = sum((.data[[x_col]] < 0 | .data[[x_col]] > screen_width |
                             .data[[y_col]] < 0 | .data[[y_col]] > screen_height)),
      trial_missing_percentage = (outside_count / total_points) * 100,
      x_outside_count = sum(.data[[x_col]] < 0 | .data[[x_col]] > screen_width),
      y_outside_count = sum(.data[[y_col]] < 0 | .data[[y_col]] > screen_height),
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

  # Annotate missingness per row
  data_annotated <- data %>%
    left_join(trial_results %>% select(all_of(c(subject_col, trial_col, "trial_missing_percentage"))),
              by = c(subject_col, trial_col)) %>%
    left_join(subject_results %>% select(all_of(c(subject_col, "subject_missing_percentage"))),
              by = subject_col)

  # Remove only gaze points outside screen bounds if requested
  if (remove) {
    data_clean <- data_annotated %>%
      filter(.data[[x_col]] >= 0, .data[[x_col]] <= screen_width,
             .data[[y_col]] >= 0, .data[[y_col]] <= screen_height)
  } else {
    data_clean <- data_annotated
  }

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

  return(list(
    subject_results = subject_results,
    trial_results = trial_results,
    data_clean = data_clean
  ))
}
