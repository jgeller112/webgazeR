#' Apply a moving average smoothing function to gaze data (X and Y).
#' This is generally recommended after up-sampling the data.
#'
#' @param x A data frame containing gaze data.
#' @param n The window size (in samples) for the moving average.
#' @param x_col The name of the X gaze column (as string).
#' @param y_col The name of the Y gaze column (as string).
#' @param trial_col The name of the trial column used for grouping (default = "Trial").
#' @param subject_col The name of the subject column used for grouping (default = "Subject").
#'
#' @return A tibble with smoothed gaze X and Y columns (replacing originals).
#' @export
#'
smooth_gaze <- function(x, n = 5, x_col = "Gaze_X", y_col = "Gaze_Y",
                        trial_col = "Trial", subject_col = "Subject") {
  x <- dplyr::as_tibble(x)

  gaze_smooth <- function(df, gaze_col) {
    df %>%
      dplyr::mutate(
        gaze_val_before = .data[[gaze_col]],
        smoothed = zoo::rollapply(.data[[gaze_col]],
                                  width = n,
                                  FUN = mean,
                                  na.rm = TRUE,
                                  partial = TRUE),
        # Preserve NAs in original data
        !!gaze_col := ifelse(is.na(gaze_val_before), NA_real_, smoothed)
      ) %>%
      dplyr::select(-gaze_val_before, -smoothed)
  }

  x <- x %>%
    dplyr::group_by(.data[[subject_col]], .data[[trial_col]]) %>%
    gaze_smooth(x_col) %>%
    gaze_smooth(y_col) %>%
    dplyr::ungroup()

  return(x)
}
