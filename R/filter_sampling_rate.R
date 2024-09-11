
#' Filter or Label Data Based on Sampling Rate Threshold
#'
#' This function allows users to set a sampling rate threshold and choose to
#' either remove the data that falls below the threshold or label it as "bad."
#' Users can apply this threshold either at the subject level, the trial level,
#' or both.
#'
#' @section Output:
#'
#' The function will either return a dataset with rows removed based on the
#' sampling rate threshold or add a new column, `is_bad`, to the dataset,
#' which indicates whether the data is considered "bad" (i.e., below the
#' sampling rate threshold).
#'
#' @section Threshold:
#'
#' A user-defined threshold is provided to check the sampling rate at the
#' subject level (median sampling rate), trial level (sampling rate per trial),
#' or both levels. If both levels are selected, the function checks that the
#' data meets the threshold for both conditions.
#'
#' @section Remove or Label:
#'
#' Users can specify whether they want to remove the data below the threshold or
#' simply label it as "bad." If `action = "remove"`, rows with a sampling rate
#' below the threshold will be excluded from the dataset. If `action = "label"`,
#' a column `is_bad` will be added to indicate rows that fail to meet the
#' threshold.
#'
#' @param data A dataframe that contains the data to be processed. The dataframe
#' should include the columns `med_SR` (subject-level median sampling rate) and
#' `SR` (trial-level sampling rate).
#' @param threshold Numeric value specifying the sampling rate threshold.
#' @param action Character string specifying whether to "remove" data that falls
#' below the threshold or "label" it as bad. Acceptable values are `"remove"` or
#' `"label"`.
#' @param by Character string specifying whether the threshold should be applied
#' at the "subject" level, the "trial" level, or "both". Acceptable values are
#' `"subject"`, `"trial"`, or `"both"`.
#' @return A dataframe with either rows removed or a new column `is_bad` added
#' to indicate whether the data is below the threshold.
#' @export
#' @examples
#' # Example usage of the filter_or_label_bad_data function
#' result <- filter_or_label_bad_data(data = data_with_sr, threshold = 500,
#'                                    action = "remove", by = "both")
#' # Example usage to label data as "bad"
#' result <- filter_or_label_bad_data(data = data_with_sr, threshold = 500,
#'                                    action = "label", by = "trial")

# Define the function to filter or label bad data based on existing sampling rates
filter_sampling_rate <- function(data,
                                 threshold=NA,
                                 action = c("remove", "label"),
                                 by = c("subject", "trial", "both")) {
  # Ensure the action and by parameters are valid
  action <- match.arg(action)
  by <- match.arg(by)

  # Apply threshold logic based on the action parameter (either remove or label)
  if (action == "remove") {
    if (by == "subject") {
      # Remove data where the subject-level sampling rate is below the threshold
      cleaned_data <- data %>%
        filter(med_SR >= threshold)
    } else if (by == "trial") {
      # Remove data where the trial-level sampling rate is below the threshold
      cleaned_data <- data %>%
        filter(SR >= threshold)
    } else if (by == "both") {
      # Remove data where either subject or trial sampling rate is below the threshold
      cleaned_data <- data %>%
        filter(med_SR >= threshold & SR >= threshold)
    }

    message("Data below the sampling rate threshold has been removed.")
    return(cleaned_data)

  } else if (action == "label") {
    if (by == "subject") {
      # Label data where the subject-level sampling rate is below the threshold
      labeled_data <- data %>%
        mutate(is_bad = ifelse(med_SR < threshold, TRUE, FALSE))
    } else if (by == "trial") {
      # Label data where the trial-level sampling rate is below the threshold
      labeled_data <- data %>%
        mutate(is_bad = ifelse(SR < threshold, TRUE, FALSE))
    } else if (by == "both") {
      # Label data where either subject or trial sampling rate is below the threshold
      labeled_data <- data %>%
        mutate(is_bad = ifelse(med_SR < threshold | SR < threshold, TRUE, FALSE))
    }

    message("Data below the sampling rate threshold has been labeled as 'bad'.")
    return(labeled_data)
  }
}
