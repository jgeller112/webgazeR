#' Analyze Sampling Rates for Eye-Tracking Data
#'
#' This function calculates the sampling rate for each subject and trial in an eye-tracking dataset.
#' Users can specify whether to compute the mean or median sampling rate. It provides overall statistics,
#' including the selected measure and standard deviation, and generates a histogram of sampling rates.
#'
#' @param eye_data A dataframe containing eye-tracking data with columns `subject`, `trial`, and `time`.
#' The column `time` should represent the time in milliseconds for each trial.
#' @param summary_stat A character string indicating the summary statistic to use. Must be either `"median"`
#' (default) or `"mean"`.
#'
#' @return A list containing:
#' \describe{
#'   \item{overall_summary_SR}{The overall mean or median sampling rate (Hz).}
#'   \item{overall_sd_SR}{The standard deviation of the sampling rate.}
#'   \item{summary_SR_by_subject}{A dataframe with the mean or median sampling rate by subject.}
#'   \item{SR_by_trial}{A dataframe with the sampling rate by subject and trial.}
#' }
#'
#' @examples
#' \dontrun{
#'   # Assuming eye_data is a dataframe with appropriate columns
#'   result <- analyze_sampling_rate(eye_data, summary_stat = "mean")
#'   print(result)
#' }
#' @export
#' @import tidyverse
analyze_sampling_rate <- function(eye_data, summary_stat = "median") {

  # Step 1: Validate the summary_stat argument
  if (!summary_stat %in% c("median", "mean")) {
    stop("Invalid summary_stat. Please choose either 'median' or 'mean'.")
  }

  # Define the summary function dynamically
  summary_fn <- if (summary_stat == "median") median else mean

  # Step 2: Calculate Sampling Rate (SR) for Each Subject and Trial
  # We determine the sampling rate as: 1000 / (max time in trial / number of time points)
  edatSR <- eye_data %>%
    dplyr::group_by(subject, trial) %>%
    dplyr::summarise(
      max_time = max(time),  # Maximum time in the trial
      n_times = length(time),  # Number of samples in the trial
      SR = ifelse(max_time > 0, 1000 / (max_time / n_times), NA_real_),  # Compute SR in Hz
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(SR))  # Remove rows where SR could not be computed

  # Step 3: Compute Subject-Level Summary (Mean or Median)
  samp_summary <- edatSR %>%
    dplyr::group_by(subject) %>%
    dplyr::summarise(summary_SR = summary_fn(SR, na.rm = TRUE), .groups = "drop")

  # Step 4: Compute Overall Summary Statistics
  overall_summary_SR <- summary_fn(samp_summary$summary_SR, na.rm = TRUE)
  overall_sd_SR <- sd(samp_summary$summary_SR, na.rm = TRUE)

  # Step 5: Print Summary Statistics
  cat("Overall", summary_stat, "Sampling Rate (Hz):", overall_summary_SR, "\n")
  cat("Overall Standard Deviation of Sampling Rate (Hz):", overall_sd_SR, "\n")

  # Print individual-level sampling rate data
  cat("\nSampling Rate by Trial:\n")
  print(edatSR)

  cat("\n", summary_stat, "Sampling Rate by Subject:\n")
  print(samp_summary)

  # Plot the histogram of sampling rates with density overlay
  histogram_plot <- ggplot(samp_summary, aes(x = summary_SR)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +

    # Updated density plot to use after_stat(count) instead of ..count..
    geom_density(aes(y = after_stat(count)), fill = "orange", alpha = 0.3) +

    geom_vline(aes(xintercept = overall_summary_SR), color = "red", linetype = "dashed", size = 1) +  # Vertical line
    annotate("text", x = overall_summary_SR, y = Inf,
             label = paste0(summary_stat, ": ", round(overall_summary_SR, 2),
                            "\nSD: ", round(overall_sd_SR, 2)),
             vjust = 1.5, hjust = -0.1, color = "red", size = 5, fontface = "bold") +
    labs(title = paste("Histogram and Density of", summary_stat, "Sampling Rates"),
         x = paste(summary_stat, "Sampling Rate (Hz)"),
         y = "Frequency / Density") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
      axis.title = element_text(size = 15)
    )

  # Print the histogram
  print(histogram_plot)


  # Step 7: Return Data as a List
  return(list(
    overall_summary_SR = overall_summary_SR,
    overall_sd_SR = overall_sd_SR,
    summary_SR_by_subject = samp_summary,
    SR_by_trial = edatSR
  ))
}
