#' Standardize a Dataframe to WebGazer Conventions
#'
#' This function takes a dataframe and renames columns to match WebGazer conventions:
#' subject, trial, time, x, y. All other columns are preserved.
#' Standardize a Dataframe to WebGazer Conventions
#'
#' This function takes a dataframe and renames columns to match WebGazer conventions:
#' subject, trial, time, x, y. All other columns are preserved.
#'
#' @param data A dataframe containing gaze data.
#' @param col_map A named list mapping your current columns to WebGazer names: `subject`, `trial`, `time`, `x`, `y`.
#' @return A dataframe with renamed columns but preserves all other original columns.
#' @import dplyr
#' @import janitor
#' @export
make_webgazer <- function(data,
                          col_map = list(
                            subject = "subject",
                            trial = "trial",
                            time = "time",
                            x = "x",
                            y = "y"
                          )) {

  # Clean column names
  data <- data %>%
    janitor::clean_names()

  # Rename specified columns, keep everything else
  data <- data %>%
    dplyr::rename(
      subject = !!sym(col_map$subject),
      trial   = !!sym(col_map$trial),
      time    = !!sym(col_map$time),
      x       = !!sym(col_map$x),
      y       = !!sym(col_map$y)
    ) %>%
    dplyr::mutate(
      subject = as.factor(subject),
      trial   = as.factor(trial),
      time    = as.numeric(time)
    )

  return(data)
}
