#' Analyze Sampling Rates (Trial and Subject Levels, with Histogram)
#'
#' Computes sampling rate for each trial (subject × trial) and summarizes at the subject level.
#' Uses distinct timepoints to avoid duplicate time values. Plots histogram of subject-level sampling rates.
#'
#' @param eye_data A dataframe with subject, trial, time columns.
#' @param summary_stat Either "median" (default) or "mean".
#'
#' @return A tibble with subject, trial, SR_trial, SR_subject
#' @export
analyze_sampling_rate <- function(eye_data, summary_stat = "median") {

  # Check input
  if (!summary_stat %in% c("median", "mean")) {
    stop("summary_stat must be 'median' or 'mean'.")
  }

  # Step 1: Compute trial-level sampling rate
  SR_by_trial <- eye_data %>%
    dplyr::distinct(subject, trial, time, .keep_all = TRUE) %>%
    dplyr::group_by(subject, trial) %>%
    dplyr::summarise(
      n_times = dplyr::n(),
      max_time = max(time, na.rm = TRUE),
      SR_trial = ifelse(max_time > 0, 1000 / (max_time / n_times), NA_real_),
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(SR_trial))

  # Step 2: Compute subject-level summary of trial SRs
  if (summary_stat == "median") {
    SR_by_subject <- SR_by_trial %>%
      dplyr::group_by(subject) %>%
      dplyr::summarise(SR_subject = median(SR_trial, na.rm = TRUE), .groups = "drop")
  } else if (summary_stat == "mean") {
    SR_by_subject <- SR_by_trial %>%
      dplyr::group_by(subject) %>%
      dplyr::summarise(SR_subject = mean(SR_trial, na.rm = TRUE), .groups = "drop")
  }

  # Step 3: Merge trial and subject summaries
  final_SR <- SR_by_trial %>%
    dplyr::left_join(SR_by_subject, by = "subject")

  # Step 4: Calculate and print overall summary
  if (summary_stat == "median") {
    overall_summary <- median(SR_by_subject$SR_subject, na.rm = TRUE)
  } else if (summary_stat == "mean") {
    overall_summary <- mean(SR_by_subject$SR_subject, na.rm = TRUE)
  }
  overall_sd <- sd(SR_by_subject$SR_subject, na.rm = TRUE)

  cat("Overall", summary_stat, "Sampling Rate (Hz):", round(overall_summary, 2), "\n")
  cat("Overall SD of Sampling Rate (Hz):", round(overall_sd, 2), "\n\n")

  # Step 5: Plot histogram
  plot_hist <- ggplot(SR_by_subject, aes(x = SR_subject)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
    geom_density(aes(y = after_stat(count)), fill = "orange", alpha = 0.3) +
    geom_vline(xintercept = overall_summary, color = "red", linetype = "dashed", linewidth = 1) +
    annotate("text", x = overall_summary, y = Inf,
             label = paste0(summary_stat, ": ", round(overall_summary, 2),
                            "\nSD: ", round(overall_sd, 2)),
             vjust = 1.5, hjust = -0.1, color = "red", size = 5, fontface = "bold") +
    labs(
      title = paste("Histogram of", summary_stat, "Sampling Rates (by Subject)"),
      x = paste(summary_stat, "Sampling Rate (Hz)"),
      y = "Frequency / Density"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
      axis.title = element_text(size = 15)
    )

  print(plot_hist)

  # Step 6: Return final data
  return(final_SR)
}
