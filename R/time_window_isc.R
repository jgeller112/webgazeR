#' Time-windowed Intersubject Correlation (ISC) per subject
#'
#' Computes ISC for each subject by correlating that subject's time series
#' against the mean of all *other* subjects within sliding windows, then
#' Fisher z-transforms correlations, averages them per subject, and converts
#' back to r.
#'
#' Correlations of exactly +/-1 are clamped to +/-0.9999999 before Fisher
#' z-transformation so they are included in the average rather than silently
#' dropped.
#'
#' @param data A numeric matrix or data.frame with rows = time points, columns = participants.
#' @param window_size Integer, window length in time points (default 10).
#' @param step Integer, step size between consecutive windows (default 1).
#' @param min_overlap Integer, minimum number of non-NA paired points within a window
#'   required to compute a correlation (default 3).
#' @param method Correlation method passed to [stats::cor()], usually "pearson" (default),
#'   or "spearman".
#' @param return_per_window Logical; if TRUE, also return a data.frame of per-window
#'   Fisher-z correlations per subject (default FALSE).
#'
#' @return If `return_per_window = FALSE`, a named numeric vector of ISC values (length = n subjects).
#'   If `TRUE`, a list with:
#'   - `isc`: named numeric vector of per-subject ISC (as above)
#'   - `per_window`: data.frame with columns `window_start`, `window_end`, `subject`, `z`
#'
#' @export
#'
time_window_isc <- function(data,
                            window_size = 10,
                            step = 1,
                            min_overlap = 3,
                            method = c("pearson", "spearman"),
                            return_per_window = FALSE) {
  method <- match.arg(method)

  # Coerce to numeric matrix
  if (is.data.frame(data)) data <- as.matrix(data)
  if (!is.matrix(data) || !is.numeric(data)) {
    stop("`data` must be a numeric matrix or data.frame (rows = time, cols = participants).")
  }

  n_timepoints <- nrow(data)
  n_participants <- ncol(data)

  if (n_participants <= 1) {
    stop("There must be more than one participant to calculate ISC.")
  }
  if (!is.finite(window_size) || window_size < 2 || window_size > n_timepoints) {
    stop("`window_size` must be an integer between 2 and the number of time points.")
  }
  if (!is.finite(step) || step < 1) stop("`step` must be a positive integer.")
  if (!is.finite(min_overlap) || min_overlap < 2) {
    stop("`min_overlap` must be an integer >= 2.")
  }

  # Precompute window starts/ends
  starts <- seq(1, n_timepoints - window_size + 1, by = step)
  ends <- starts + window_size - 1L
  n_windows <- length(starts)
  if (n_windows == 0) {
    stop("No windows to compute. Reduce `window_size` or increase data length.")
  }

  # Storage
  p_names <- .participant_names(data)
  isc_values <- rep(NA_real_, n_participants)
  names(isc_values) <- p_names

  # Pre-allocate per-window storage
  if (return_per_window) {
    pw_window_start <- integer(n_participants * n_windows)
    pw_window_end   <- integer(n_participants * n_windows)
    pw_subject      <- character(n_participants * n_windows)
    pw_z            <- numeric(n_participants * n_windows)
    pw_idx <- 0L
  }

  # Loop over subjects
  for (subject in seq_len(n_participants)) {
    # Pre-allocate z_vals for this subject
    z_vals <- numeric(n_windows)
    z_count <- 0L

    for (w in seq_len(n_windows)) {
      i1 <- starts[w]
      i2 <- ends[w]
      window_data <- data[i1:i2, , drop = FALSE]

      # Mean of other subjects, row-wise, ignoring NA
      if (n_participants == 2) {
        other_subjects_mean <- window_data[, -subject, drop = TRUE]
      } else {
        other_subjects_mean <- rowMeans(window_data[, -subject, drop = FALSE], na.rm = TRUE)
      }

      x <- window_data[, subject]
      y <- other_subjects_mean

      # Keep only indices where both x and y are non-NA
      ok <- is.finite(x) & is.finite(y)
      if (sum(ok) < min_overlap) next

      x_ok <- x[ok]
      y_ok <- y[ok]

      # Check variance; cor() will return NA if either is constant
      if (sd(x_ok) == 0 || sd(y_ok) == 0) next

      r <- suppressWarnings(cor(x_ok, y_ok, method = method))
      if (!is.na(r) && is.finite(r)) {
        z <- atanh(.clamp_r(r))
        z_count <- z_count + 1L
        z_vals[z_count] <- z

        if (return_per_window) {
          pw_idx <- pw_idx + 1L
          pw_window_start[pw_idx] <- i1
          pw_window_end[pw_idx]   <- i2
          pw_subject[pw_idx]      <- p_names[subject]
          pw_z[pw_idx]            <- z
        }
      }
    }

    if (z_count > 0) {
      isc_values[subject] <- tanh(mean(z_vals[seq_len(z_count)]))
    }
  }

  if (return_per_window) {
    per_window_df <- data.frame(
      window_start = pw_window_start[seq_len(pw_idx)],
      window_end   = pw_window_end[seq_len(pw_idx)],
      subject      = pw_subject[seq_len(pw_idx)],
      z            = pw_z[seq_len(pw_idx)],
      stringsAsFactors = FALSE
    )
    return(list(isc = isc_values, per_window = per_window_df))
  } else {
    return(isc_values)
  }
}
