#' Downsample gaze data
#'
#' This function combines gaze samples into time bins and optionally aggregates the data.
#' @param dataframe DataFrame containing gaze data.
#' @param bin.length Length of time bins (in milliseconds).
#' @param timevar Column name representing time.
#' @param aggvars Vector of variable names to group by for aggregation. Use "none" to skip aggregation.
#' @return DataFrame with time bins added and optionally aggregated data.
#' @export
downsample_gaze <- function(dataframe, bin.length = 50, timevar = "time",
                            aggvars = c("subject", "condition", "target", "trial", "object", "time_bin")) {
  # Add time bins
  dataframe <- dataframe %>%
    mutate(time_bin = floor(.data[[timevar]] / bin.length) * bin.length)

  # Skip aggregation if aggvars = "none"
  if (aggvars == "none") {
    return(dataframe)
  }

  # Perform aggregation if aggvars is specified
  downsample <- dataframe %>%
    group_by(across(all_of(aggvars))) %>%
    summarize(Fix = mean(Looks, na.rm = TRUE), .groups = "drop")

  return(downsample)
}
