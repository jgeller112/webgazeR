#' Filter or Label Data Based on Sampling Rate Threshold
#'
#' This function applies a sampling rate threshold and either removes
#' or labels "bad" subjects/trials based on their sampling rates.
#'
#' @param data A dataframe with columns: subject, trial, SR_subject, SR_trial.
#' @param threshold Numeric. Sampling rate threshold to apply.
#' @param action "remove" (default) to delete bad data or "label" to flag bad data.
#' @param by "subject", "trial", or "both" to specify where to apply the threshold.
#'
#' @return A dataframe with either rows removed or bad subjects/trials labeled.
#' @export
filter_sampling_rate <- function(data,
                                 threshold = NA,
                                 action = c("remove", "label"),
                                 by = c("subject", "trial", "both")) {

  # Validate arguments
  action <- match.arg(action)
  by <- match.arg(by)

  # Counts before filtering
  total_subjects <- length(unique(data$subject))
  total_trials <- nrow(data)

  # Perform action
  if (action == "remove") {

    if (by == "subject") {
      cleaned_data <- data %>%
        group_by(subject) %>%
        filter(SR_subject >= threshold) %>%
        ungroup()

      subjects_removed <- total_subjects - length(unique(cleaned_data$subject))
      message(subjects_removed, " subjects have been removed due to sampling rate thresholds.")

    } else if (by == "trial") {
      cleaned_data <- data %>%
        group_by(subject, trial) %>%
        filter(SR_trial >= threshold) %>%
        ungroup()

      trials_removed <- total_trials - nrow(cleaned_data)
      message(trials_removed, " trials have been removed due to sampling rate thresholds.")

    } else if (by == "both") {
      cleaned_data <- data %>%
        group_by(subject, trial) %>%
        filter(SR_subject >= threshold & SR_trial >= threshold) %>%
        ungroup()

      subjects_removed <- total_subjects - length(unique(cleaned_data$subject))
      trials_removed <- total_trials - nrow(cleaned_data)
      message(subjects_removed, " subjects and ", trials_removed, " trials have been removed due to sampling rate thresholds.")
    }

    return(cleaned_data)

  } else if (action == "label") {

    if (by == "subject") {
      flagged_data <- data %>%
        mutate(is_bad_subject = SR_subject < threshold)

      subjects_bad <- length(unique(flagged_data$subject[flagged_data$is_bad_subject]))
      message(subjects_bad, " subjects have been flagged as 'bad'.")

    } else if (by == "trial") {
      flagged_data <- data %>%
        mutate(is_bad_trial = SR_trial < threshold)

      trials_bad <- sum(flagged_data$is_bad_trial, na.rm = TRUE)
      message(trials_bad, " trials have been flagged as 'bad'.")

    } else if (by == "both") {
      flagged_data <- data %>%
        mutate(
          is_bad_subject = SR_subject < threshold,
          is_bad_trial = SR_trial < threshold
        )

      subjects_bad <- length(unique(flagged_data$subject[flagged_data$is_bad_subject]))
      trials_bad <- sum(flagged_data$is_bad_trial, na.rm = TRUE)
      message(subjects_bad, " subjects and ", trials_bad, " trials have been flagged as 'bad'.")
    }

    return(flagged_data)
  }
}
