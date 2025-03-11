#' Compute Intersubject Correlation (ISC)
#'
#' Computes ISC using either the **pairwise correlation** method or the **leave-one-out** method.
#' The pairwise method computes a full correlation matrix and averages Fisher-transformed values.
#' The leave-one-out method computes the correlation of each participant's time series with the mean of all others.
#'
#' @param data_matrix A numeric matrix where rows represent time points and columns represent participants.
#' @param method A string specifying the ISC computation method. Options are `"pairwise"` (default) or `"leave-one-out"`.
#'
#' @return A numeric vector of ISC values, one per participant.
#'
#' @export
calculate_isc <- function(data_matrix, method = "pairwise") {
  # Validate input
  if (!is.matrix(data_matrix)) {
    stop("`data_matrix` must be a numeric matrix.")
  }

  n_participants <- ncol(data_matrix)
  if (n_participants <= 1) {
    stop("There must be more than one participant to calculate ISC.")
  }

  if (method == "pairwise") {
    # Compute full correlation matrix
    correlation_matrix <- cor(data_matrix, use = "pairwise.complete.obs")

    # Remove self-correlations (diagonal)
    diag(correlation_matrix) <- NA

    # Convert to Fisher's Z, average per participant, and convert back
    z_values <- atanh(correlation_matrix)  # Fisher's Z transformation
    isc_values <- tanh(rowMeans(z_values, na.rm = TRUE))  # Convert back to r

  } else if (method == "leave-one-out") {
    leave_one_out_isc <- numeric(n_participants)

    for (subject in 1:n_participants) {
      # Compute the leave-one-out mean time series (excluding current subject)
      leave_one_out_mean <- rowMeans(data_matrix[, -subject, drop = FALSE], na.rm = TRUE)

      # Extract the full time series for the current subject
      subject_time_series <- data_matrix[, subject]

      # Ensure valid time points exist before computing correlation
      valid_indices <- !is.na(subject_time_series) & !is.na(leave_one_out_mean)

      if (sum(valid_indices) > 1) {  # Ensure enough valid time points
        leave_one_out_isc[subject] <- cor(subject_time_series[valid_indices],
                                          leave_one_out_mean[valid_indices],
                                          use = "pairwise.complete.obs")
      } else {
        leave_one_out_isc[subject] <- NA  # Not enough valid data
      }
    }

    isc_values <- leave_one_out_isc
  } else {
    stop("Invalid method. Use 'pairwise' or 'leave-one-out'.")
  }

  return(isc_values)  # Ensure only one return statement
}
