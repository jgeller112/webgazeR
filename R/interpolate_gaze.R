#' Interpolate missing gaze data (X and Y) within trials, with optional max gap
#'
#' @param x A data frame containing gaze data.
#' @param x_col The name of the X gaze column (as string).
#' @param y_col The name of the Y gaze column (as string).
#' @param trial_col The name of the trial column (default = "Trial").
#' @param subject_col The name of the subject column (default = "Subject").
#' @param time_col The name of the time column used for sorting (default = "Time").
#' @param max_gap Maximum number of consecutive missing samples to interpolate. Gaps larger than this remain NA (default = Inf).
#'
#' @return A tibble with interpolated gaze X and Y columns (replacing originals).
#' @export
#'


interpolate_gaze <- function(x, x_col = "Gaze_X", y_col = "Gaze_Y",
                             trial_col = "Trial", subject_col = "Subject",
                             time_col = "Time", max_gap = Inf) {
  x <- dplyr::as_tibble(x)

  interpolate_column <- function(vec, max_gap) {
    if (all(is.na(vec))) return(rep(NA_real_, length(vec)))
    zoo::na.approx(vec, x = seq_along(vec), na.rm = FALSE, maxgap = max_gap)
  }

  x <- x %>%
    dplyr::group_by(.data[[subject_col]], .data[[trial_col]]) %>%
    dplyr::arrange(.data[[time_col]], .by_group = TRUE) %>%
    dplyr::mutate(
      !!x_col := interpolate_column(.data[[x_col]], max_gap),
      !!y_col := interpolate_column(.data[[y_col]], max_gap)
    ) %>%
    dplyr::ungroup()

  return(x)
}


