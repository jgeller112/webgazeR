#' Analyze Sampling Rates for Eye-Tracking Data
#'
#' This function calculates the sampling rate for each subject and trial in an eye-tracking dataset.
#' It provides overall statistics, including the median and standard deviation of sampling rates,
#' and also generates a histogram of median sampling rates by subject, with a density plot overlayed,
#' and a vertical line showing the overall median sampling rate and the standard deviation displayed.
#'
#' @param eye_data A dataframe containing eye-tracking data with columns `subject`, `trial`, and `time`.
#' The column `time` should represent the time in milliseconds for each trial.
#'
#' @return A list containing:
#' \describe{
#'   \item{overall_median_SR}{The overall median sampling rate (Hz).}
#'   \item{overall_sd_SR}{The overall standard deviation of sampling rates.}
#'   \item{median_SR_by_subject}{A dataframe with the median sampling rate by subject.}
#'   \item{SR_by_trial}{A dataframe with the sampling rate by subject and trial.}
#' }
#'
#' @examples
#' \dontrun{
#'   # Assuming eye_data is a dataframe with appropriate columns
#'   result <- analyze_sampling_rate(eye_data)
#'   print(result)
#' }
#' @export
#' @import dplyr ggplot2
analyze_sampling_rate <- function(eye_data) {

  # Calculate sampling rate (SR) for each subject and trial
  edatSR <- eye_data %>%
    dplyr::group_by(subject, trial) %>%
    dplyr::summarise(
      max_time = max(time),
      n_times = length(time),
      SR = ifelse(max_time > 0, 1000 / (max_time / n_times), NA_real_)
    ) %>%
    dplyr::filter(!is.na(SR))  # Remove rows with NA SR

  # Summarize the median sampling rate for each subject
  samp_med <- edatSR %>%
    dplyr::group_by(subject) %>%
    dplyr::summarise(med_SR = median(SR, na.rm = TRUE))

  # Calculate overall median and standard deviation of sampling rates
  overall_med_SR <- median(samp_med$med_SR, na.rm = TRUE)
  overall_sd_SR <- sd(samp_med$med_SR, na.rm = TRUE)

  # Print the overall median and standard deviation
  cat("Overall Median Sampling Rate (Hz):", overall_med_SR, "\n")
  cat("Overall Standard Deviation of Sampling Rate (Hz):", overall_sd_SR, "\n")

  # Print the sampling rate by trial
  cat("\nSampling Rate by Trial:\n")
  print(edatSR)

  # Print the median sampling rate by subject
  cat("\nMedian Sampling Rate by Subject:\n")
  print(samp_med)

  # Plot the histogram of median sampling rates by subject, with a density plot and a vertical line at the overall median
  histogram_plot <- ggplot(samp_med, aes(x = med_SR)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
    geom_density(aes(y = ..count..), fill = "orange", alpha = 0.3) +  # Overlayed density plot
    geom_vline(aes(xintercept = overall_med_SR), color = "red", linetype = "dashed", size = 1) +  # Vertical line for median
    annotate("text", x = overall_med_SR, y = Inf, label = paste0("Median: ", round(overall_med_SR, 2), "\nSD: ", round(overall_sd_SR, 2)),
             vjust = 1.5, hjust = -0.1, color = "red", size = 5, fontface = "bold") +  # Annotate with median and SD
    labs(title = "Histogram and Density of Median Sampling Rates",
         x = "Median Sampling Rate (Hz)",
         y = "Frequency / Density") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
      axis.title = element_text(size = 15)
    )

  # Print the histogram
  print(histogram_plot)

  # Return the overall median, standard deviation, median SR by subject, and SR by trial
  return(list(overall_median_SR = overall_med_SR,
              overall_sd_SR = overall_sd_SR,
              median_SR_by_subject = samp_med,
              SR_by_trial = edatSR))
}
