#' Analyze Sampling Rates for Eye-Tracking Data (Removes Duplicate Times)
#'
#' Calculates the sampling rate for each subject and trial using distinct timepoints,
#' ignoring duplicates. Provides overall summary statistics and histogram.
#'
#' @param eye_data A dataframe with columns: `subject`, `trial`, and `time` (in ms).
#' @param summary_stat Either "median" (default) or "mean" to summarize subject-level SR.
#'
#' @return A list with:
#' \describe{
#'   \item{overall_summary_SR}{Overall median or mean sampling rate.}
#'   \item{overall_sd_SR}{Standard deviation of subject-level SR.}
#'   \item{summary_SR_by_subject}{Summary SR by subject.}
#'   \item{SR_by_trial}{Trial-level SR estimates.}
#' }
#' @export
#' @import tidyverse
analyze_sampling_rate <- function(eye_data, summary_stat = "median") {

  if (!summary_stat %in% c("median", "mean")) {
    stop("Invalid summary_stat. Please choose either 'median' or 'mean'.")
  }

  summary_fn <- if (summary_stat == "median") median else mean

  # Step 1: Calculate SR using distinct timepoints only
  edatSR <- eye_data %>%
    group_by(subject, trial) %>%
    summarise(
      max_time = max(time, na.rm = TRUE),
      n_unique_times = n_distinct(time),
      SR = ifelse(max_time > 0, 1000 / (max_time / n_unique_times), NA_real_),
      .groups = "drop"
    ) %>%
    filter(!is.na(SR))  # Drop invalid SRs

  # Step 2: Summarize per subject
  samp_summary <- edatSR %>%
    group_by(subject) %>%
    summarise(summary_SR = summary_fn(SR, na.rm = TRUE), .groups = "drop")

  # Step 3: Overall summaries
  overall_summary_SR <- summary_fn(samp_summary$summary_SR, na.rm = TRUE)
  overall_sd_SR <- sd(samp_summary$summary_SR, na.rm = TRUE)

  # Step 4: Print summaries
  cat("Overall", summary_stat, "Sampling Rate (Hz):", overall_summary_SR, "\n")
  cat("Overall Standard Deviation of Sampling Rate (Hz):", overall_sd_SR, "\n\n")

  cat("Sampling Rate by Trial:\n")
  print(edatSR)

  cat("\n", summary_stat, "Sampling Rate by Subject:\n")
  print(samp_summary)

  # Step 5: Plot histogram
  histogram_plot <- ggplot(samp_summary, aes(x = summary_SR)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
    geom_density(aes(y = after_stat(count)), fill = "orange", alpha = 0.3) +
    geom_vline(aes(xintercept = overall_summary_SR), color = "red", linetype = "dashed", size = 1) +
    annotate("text", x = overall_summary_SR, y = Inf,
             label = paste0(summary_stat, ": ", round(overall_summary_SR, 2),
                            "\nSD: ", round(overall_sd_SR, 2)),
             vjust = 1.5, hjust = -0.1, color = "red", size = 5, fontface = "bold") +
    labs(
      title = paste("Histogram and Density of", summary_stat, "Sampling Rates"),
      x = paste(summary_stat, "Sampling Rate (Hz)"),
      y = "Frequency / Density"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
      axis.title = element_text(size = 15)
    )

  print(histogram_plot)

  # Step 6: Return list of outputs
  return(list(
    overall_summary_SR = overall_summary_SR,
    overall_sd_SR = overall_sd_SR,
    summary_SR_by_subject = samp_summary,
    SR_by_trial = edatSR
  ))
}
