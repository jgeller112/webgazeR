#' @keywords internal
"_PACKAGE"

#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr unnest fill
#' @importFrom purrr map map_lgl walk imap list_rbind
#' @importFrom readr read_csv read_tsv write_csv
#' @importFrom stringr str_replace_all str_trim
#' @importFrom tibble as_tibble tibble
#' @importFrom rlang .data sym %||% :=
#' @importFrom janitor clean_names make_clean_names
#' @importFrom readxl read_excel
#' @importFrom zoo na.approx rollapply
#' @importFrom jsonlite fromJSON stream_in
#' @importFrom ggokabeito scale_color_okabe_ito
#' @importFrom stats cor sd median
#' @importFrom utils head install.packages
NULL

utils::globalVariables(c(
  "IA_label", "Looks", "SR_subject", "SR_trial",
  "centroid_x", "centroid_y", "dispersion", "eyedata",
  "gaze_val_before", "height_normalized",
  "is_bad_row", "is_bad_subject", "is_bad_trial",
  "max_time", "n_times", "outside_count", "smoothed",
  "subject", "subject_missing_percentage",
  "time", "total_points", "total_trials", "trial",
  "width_normalized", "x", "x_normalized",
  "x_outside_count", "x_outside_percentage",
  "y", "y_normalized", "y_outside_count", "y_outside_percentage",
  "zone_height_normalised", "zone_name",
  "zone_width_normalised", "zone_x_normalised", "zone_y_normalised"
))
