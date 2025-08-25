#' Time-windowed Intersubject Correlation (ISC) per subject
#'
#' Computes ISC for each subject by correlating that subject's time series
#' against the mean of all *other* subjects within sliding windows, then
#' Fisher z-transforms correlations, averages them per subject, and converts
#' back to r.
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
  isc_values <- rep(NA_real_, n_participants)
  names(isc_values) <- colnames(data) %||% paste0("S", seq_len(n_participants))

  if (return_per_window) {
    per_win_list <- vector("list", n_participants)
  }

  # Loop over subjects
  for (subject in seq_len(n_participants)) {
    z_vals <- numeric(0)

    # Optional per-window capture
    if (return_per_window) {
      per_sub_win <- data.frame(
        window_start = integer(0),
        window_end = integer(0),
        subject = character(0),
        z = numeric(0),
        stringsAsFactors = FALSE
      )
    }

    for (w in seq_len(n_windows)) {
      i1 <- starts[w]
      i2 <- ends[w]
      window_data <- data[i1:i2, , drop = FALSE]

      # Mean of other subjects, row-wise, ignoring NA
      if (n_participants == 2) {
        # Fast path: "others" is just the other single subject
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
      if (!is.na(r) && is.finite(r) && r > -1 && r < 1) {
        z <- atanh(r) # Fisher z
        z_vals <- c(z_vals, z)

        if (return_per_window) {
          per_sub_win <- rbind(per_sub_win, data.frame(
            window_start = i1,
            window_end = i2,
            subject = names(isc_values)[subject],
            z = z,
            stringsAsFactors = FALSE
          ))
        }
      }
    }

    if (length(z_vals) > 0) {
      isc_values[subject] <- tanh(mean(z_vals))
    } else {
      isc_values[subject] <- NA_real_
    }

    if (return_per_window) {
      per_win_list[[subject]] <- per_sub_win
    }
  }

  if (return_per_window) {
    per_window_df <- do.call(rbind, per_win_list)
    return(list(isc = isc_values, per_window = per_window_df))
  } else {
    return(isc_values)
  }
}
