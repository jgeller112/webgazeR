#' Compute Intersubject Correlation (ISC)
#'
#' Computes ISC using either the **pairwise correlation** method or the **leave-one-out** method.
#' The pairwise method computes a full correlation matrix and averages Fisher z-transformed values.
#' The leave-one-out method computes the correlation of each participant's time series with the mean of all others.
#'
#' @param data_matrix A numeric matrix or data.frame where rows represent time points and columns represent participants.
#' @param method A string specifying the ISC computation method. Options are `"pairwise"` (default) or `"leave-one-out"`.
#' @param cor_method Correlation type passed to [stats::cor()]. One of `"pearson"` (default) or `"spearman"`.
#' @param return_matrix Logical. If `TRUE` and `method = "pairwise"`, return the full
#'   participant-by-participant correlation matrix (self-correlations set to NA) instead of
#'   a summary vector. Ignored for `method = "leave-one-out"`.
#'
#' @return A named numeric vector of ISC values (one per participant), or a correlation matrix
#'   if `return_matrix = TRUE`.
#'
#' @export
calculate_isc <- function(data_matrix,
                          method = c("pairwise", "leave-one-out"),
                          cor_method = c("pearson", "spearman"),
                          return_matrix = FALSE) {
  method <- match.arg(method)
  cor_method <- match.arg(cor_method)

  # Accept data.frames
  if (is.data.frame(data_matrix)) data_matrix <- as.matrix(data_matrix)
  if (!is.matrix(data_matrix) || !is.numeric(data_matrix)) {
    stop("`data_matrix` must be a numeric matrix or data.frame.")
  }

  n_participants <- ncol(data_matrix)
  if (n_participants <= 1) {
    stop("There must be more than one participant to calculate ISC.")
  }

  p_names <- .participant_names(data_matrix)

  if (method == "pairwise") {
    # Compute full correlation matrix
    correlation_matrix <- cor(data_matrix, use = "pairwise.complete.obs", method = cor_method)
    diag(correlation_matrix) <- NA
    rownames(correlation_matrix) <- p_names
    colnames(correlation_matrix) <- p_names

    if (return_matrix) {
      return(correlation_matrix)
    }

    # Fisher z-transform, average, back-transform
    z_values <- atanh(.clamp_r(correlation_matrix))
    isc_values <- tanh(rowMeans(z_values, na.rm = TRUE))

  } else {
    # Leave-one-out
    if (return_matrix) {
      warning("`return_matrix` is ignored for method = 'leave-one-out'.")
    }

    raw_r <- vapply(seq_len(n_participants), function(s) {
      .loo_cor(data_matrix, s, cor_method = cor_method)
    }, numeric(1))

    # Fisher z round-trip for consistency with pairwise
    z <- atanh(.clamp_r(raw_r))
    isc_values <- tanh(z)
  }

  names(isc_values) <- p_names
  isc_values
}
