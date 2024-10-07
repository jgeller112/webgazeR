#' Downsample gaze data
#'
#' This function will combine gaze samples into time bins.
#' @param dataframe DataFrame containing gaze data.
#' @param bin.length Length of time bins (in milliseconds).
#' @param timevar Column name representing time.
#' @param aggvars Vector of variable names to group by for aggregation.
#' @return Downsampled DataFrame.
#' @export
downsample_gaze <- function(dataframe, bin.length = 50, timevar = "time",
                            aggvars = c("subject", "condition", "target", "trial", "object", "timebins")) {

  # Create time bins
  dataframe <- dataframe %>%
    mutate(timebins = round(.data[[timevar]] / bin.length) * bin.length)

  # Downsample the gaze data
  if (length(aggvars) > 0) {
    downsample <- dataframe %>%
      group_by(across(all_of(aggvars))) %>%
      summarize(Fix = mean(Looks))
  } else {
    downsample <- dataframe
  }

  return(downsample)
}
