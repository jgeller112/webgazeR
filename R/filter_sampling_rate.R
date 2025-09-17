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
#'
#'
#'

filter_sampling_rate <- function(data,
                                 threshold,
                                 action = c("remove", "label"),
                                 by = c("subject", "trial", "both")) {
  action <- match.arg(action)
  by <- match.arg(by)

  # Helpers
  n_unique_trials <- function(df) dplyr::n_distinct(df$subject, df$trial)

  # Totals (for consistent reporting)
  total_subjects <- dplyr::n_distinct(data$subject)
  total_trials <- n_unique_trials(data)

  # Build flags ONCE so remove/label see the exact same logic
  flagged <- data %>%
    mutate(
      is_bad_subject = case_when(
        by %in% c("subject", "both") ~ is.na(SR_subject) | SR_subject < threshold,
        TRUE ~ FALSE
      ),
      is_bad_trial = case_when(
        by %in% c("trial", "both") ~ is.na(SR_trial) | SR_trial < threshold,
        TRUE ~ FALSE
      ),
      # A row is bad if the relevant unit(s) are bad
      is_bad_row = case_when(
        by == "subject" ~ is_bad_subject,
        by == "trial" ~ is_bad_trial,
        by == "both" ~ (is_bad_subject | is_bad_trial)
      )
    )

  if (action == "remove") {
    cleaned <- flagged %>% filter(!is_bad_row)

    subjects_removed <- total_subjects - dplyr::n_distinct(cleaned$subject)
    trials_removed <- total_trials - n_unique_trials(cleaned)

    if (by == "subject") {
      message(subjects_removed, " subjects have been removed due to sampling rate thresholds.")
    } else if (by == "trial") {
      message(trials_removed, " trials have been removed due to sampling rate thresholds.")
    } else { # both
      message(
        subjects_removed, " subjects and ", trials_removed,
        " trials have been removed due to sampling rate thresholds."
      )
    }

    # Return the same columns as input (drop helper flags)
    return(cleaned %>% select(-is_bad_subject, -is_bad_trial, -is_bad_row))
  }

  # action == "label"
  subjects_bad <- dplyr::n_distinct(flagged$subject[flagged$is_bad_subject])
  trials_bad <- n_unique_trials(dplyr::filter(flagged, is_bad_row))

  if (by == "subject") {
    message(subjects_bad, " subjects have been flagged as 'bad'.")
  } else if (by == "trial") {
    message(trials_bad, " trials have been flagged as 'bad'.")
  } else { # both
    message(subjects_bad, " subjects and ", trials_bad, " trials have been flagged as 'bad'.")
  }

  return(flagged)
}
