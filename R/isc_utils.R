# Internal ISC helper functions (not exported)

#' Compute leave-one-out correlation for a single subject
#'
#' Correlates one subject's time series against the mean of all others,
#' with Fisher z-transformation.
#'
#' @param data_matrix Numeric matrix (rows = time, cols = participants).
#' @param subject Integer index of the focal subject.
#' @param cor_method Correlation method ("pearson" or "spearman").
#' @param min_valid Minimum number of valid (non-NA) paired observations.
#' @return A single r value (back-transformed from Fisher z), or NA.
#' @noRd
.loo_cor <- function(data_matrix, subject, cor_method = "pearson", min_valid = 2) {
  others_mean <- rowMeans(data_matrix[, -subject, drop = FALSE], na.rm = TRUE)
  subj_ts <- data_matrix[, subject]

  ok <- is.finite(subj_ts) & is.finite(others_mean)
  if (sum(ok) < min_valid) return(NA_real_)

  r <- suppressWarnings(cor(subj_ts[ok], others_mean[ok], method = cor_method))
  if (is.na(r) || !is.finite(r)) return(NA_real_)
  r
}

#' Clamp a correlation to avoid Inf from atanh
#'
#' Values of exactly +/-1 produce Inf under Fisher z. This clamps to a
#' near-boundary value so they can still be included in averages.
#'
#' @param r Numeric vector of correlations.
#' @param bound Upper clamp value (default 0.9999999).
#' @return Clamped vector.
#' @noRd
.clamp_r <- function(r, bound = 1 - 1e-7) {
  pmin(pmax(r, -bound), bound)
}

#' Get participant names from a matrix
#' @noRd
.participant_names <- function(data_matrix) {
  colnames(data_matrix) %||% paste0("S", seq_len(ncol(data_matrix)))
}
