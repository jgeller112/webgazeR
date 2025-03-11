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
#' sampling rate threshold or add new columns, `is_bad_subject` and/or `is_bad_trial` to the dataset,
#' which indicates whether the data is considered "bad" (i.e., below the
#' sampling rate threshold).
#'
#' @param data A dataframe that contains the data to be processed. The dataframe
#' should include the columns:
#' \describe{
#'   \item{`subject`}{Unique identifier for each participant in the dataset.}
#'   \item{`med_SR`}{Subject-level median sampling rate (Hz). This represents the median sampling rate for a subject across trials.}
#'   \item{`SR`}{Trial-level sampling rate (Hz). This represents the sampling rate for each specific trial.}
#' }
#' @param threshold Numeric value specifying the sampling rate threshold. Data falling below this threshold will either be removed or labeled as "bad".
#' @param action Character string specifying whether to "remove" data that falls below the threshold or "label" it as bad. Acceptable values are `"remove"` or `"label"`.
#' \describe{
#'   \item{`"remove"`}{Removes rows from the dataset where the sampling rate falls below the threshold.}
#'   \item{`"label"`}{Adds new columns `is_bad_subject` and/or `is_bad_trial` that flag rows where the sampling rate falls below the threshold.}
#' }
#' @param by Character string specifying whether the threshold should be applied
#' at the "subject" level, the "trial" level, or "both". Acceptable values are
#' `"subject"`, `"trial"`, or `"both"`.
#' \describe{
#'   \item{`"subject"`}{Applies the threshold to the subject-level median sampling rate (`med_SR`).}
#'   \item{`"trial"`}{Applies the threshold to the trial-level sampling rate (`SR`).}
#'   \item{`"both"`}{Applies the threshold to both the subject-level (`med_SR`) and trial-level (`SR`) rates. Data is removed/labeled if either rate falls below the threshold.}
#' }
#'
#' @return A dataframe with either rows removed or new columns (`is_bad_subject`, `is_bad_trial`) added
#' to indicate whether the data is below the threshold. Additionally, messages will inform the user how many subjects and trials were removed or labeled as "bad."
#' @export

filter_sampling_rate <- function(data,
                                 threshold = NA,
                                 action = c("remove", "label"),
                                 by = c("subject", "trial", "both")) {

  # Ensure the action and by parameters are valid
  action <- match.arg(action)
  by <- match.arg(by)

  # Count total subjects and trials before filtering/labeling
  total_subjects <- length(unique(data$subject))
  total_trials <- nrow(data)

  # Apply threshold logic based on the action parameter (either remove or label)
  if (action == "remove") {
    if (by == "subject") {
      # Remove data where the subject-level median sampling rate is below the threshold
      cleaned_data <- data %>%
        group_by(subject) %>%  # Group by subject to apply the subject-level filter
        filter(sampling_rate_subject >= threshold) %>%
        ungroup()
      subjects_removed <- total_subjects - length(unique(cleaned_data$subject))
      trials_removed <- total_trials - nrow(cleaned_data)
    } else if (by == "trial") {
      # Remove data where the trial-level sampling rate is below the threshold
      cleaned_data <- data %>%
        group_by(trial) %>%  # Group by trial to apply the trial-level filter
        filter(sampling_rate_trial >= threshold) %>%
        ungroup()
      subjects_removed <- total_subjects - length(unique(cleaned_data$subject))
      trials_removed <- total_trials - nrow(cleaned_data)
    } else if (by == "both") {
      # Remove data where either subject or trial sampling rate is below the threshold
      cleaned_data <- data %>%
        group_by(subject, trial) %>%
        filter(sampling_rate_subject >= threshold & sampling_rate_trial >= threshold) %>%
        ungroup()
      subjects_removed <- total_subjects - length(unique(cleaned_data$subject))
      trials_removed <- total_trials - nrow(cleaned_data)
    }

    # Message how many subjects and trials have been removed
    message(paste0(subjects_removed, " subjects and ", trials_removed, " trials have been removed due to sampling rate thresholds."))
    return(cleaned_data)

  } else if (action == "label") {
    if (by == "subject") {
      # Label data where the subject-level median sampling rate is below the threshold
      flagged_data <- data %>%
        group_by(subject) %>%  # Group by subject
        mutate(is_bad_subject = ifelse(sampling_rate_subject < threshold, TRUE, FALSE)) %>%
        ungroup()
      subjects_bad <- length(unique(flagged_data$subject[flagged_data$is_bad_subject == TRUE]))
      trials_bad <- sum(flagged_data$is_bad_subject)

      # Message how many subjects have been flagged as bad
      message(paste0(subjects_bad, " subjects have been flagged as 'bad' due to sampling rate thresholds."))

    } else if (by == "trial") {
      # Label data where the trial-level sampling rate is below the threshold, handling NA values
      flagged_data <- data %>%
        group_by(trial) %>%  # Group by trial
        mutate(is_bad_trial = ifelse(is.na(sampling_rate_trial) | sampling_rate_trial < threshold, TRUE, FALSE)) %>%
        ungroup()
      subjects_bad <- length(unique(flagged_data$subject[flagged_data$is_bad_trial == TRUE]))
      trials_bad <- sum(flagged_data$is_bad_trial, na.rm = TRUE)  # Use na.rm to handle NAs

      # Message how many trials have been flagged as bad
      message(paste0(trials_bad, " trials have been flagged as 'bad' due to sampling rate thresholds."))

    } else if (by == "both") {
      # Label data where either subject or trial sampling rate is below the threshold, handling NA values
      flagged_data <- data %>%
        group_by(subject, trial) %>%
        mutate(is_bad_subject = ifelse(is.na(sampling_rate_subject) | sampling_rate_subject < threshold, TRUE, FALSE),
               is_bad_trial = ifelse(is.na(sampling_rate_trial) | sampling_rate_trial < threshold, TRUE, FALSE)) %>%
        ungroup()
      subjects_bad <- length(unique(flagged_data$subject[flagged_data$is_bad_subject == TRUE]))
      trials_bad <- sum(flagged_data$is_bad_trial, na.rm = TRUE)  # Use na.rm to handle NAs

      # Message how many subjects and trials have been flagged as bad
      message(paste0(subjects_bad, " subjects and ", trials_bad, " trials have been flagged as 'bad' due to sampling rate thresholds."))
    }

    return(flagged_data)
  }
}
