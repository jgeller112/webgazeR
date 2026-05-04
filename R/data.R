#' Example webcam eye-tracking data
#'
#' A sample of webcam-based eye-tracking gaze samples exported from a Gorilla
#' visual-world-paradigm experiment. Used to demonstrate the preprocessing
#' pipeline in the package vignette.
#'
#' @format A data frame with 104,823 rows and 24 columns. Key columns include:
#' \describe{
#'   \item{subject}{Participant identifier.}
#'   \item{trial}{Trial number within a participant.}
#'   \item{time}{Sample time within a trial (ms).}
#'   \item{time_stamp}{Raw sample timestamp.}
#'   \item{type}{Sample type (e.g., prediction, calibration).}
#'   \item{screen_index}{Screen index within the trial.}
#'   \item{x_pred, y_pred}{Predicted gaze coordinates in screen pixels.}
#'   \item{x_pred_normalised, y_pred_normalised}{Predicted gaze coordinates
#'     normalised to [0, 1].}
#'   \item{convergence}{WebGazer convergence value for the prediction.}
#'   \item{face_conf}{Face-detection confidence for the sample.}
#'   \item{zone_name}{Name of the interest area (zone) the sample belongs to.}
#'   \item{zone_x, zone_y, zone_width, zone_height}{Zone position and size in
#'     pixels.}
#'   \item{zone_x_normalised, zone_y_normalised, zone_width_normalised,
#'     zone_height_normalised}{Normalised zone position and size.}
#'   \item{filename}{Source data file the sample came from.}
#' }
#' @source Gorilla Experiment Builder (\url{https://gorilla.sc/}).
"eyedata"

#' Example behavioural data from a Gorilla experiment
#'
#' Trial-level behavioural output from the same Gorilla experiment as
#' \code{\link{eyedata}}. Used in the vignette to illustrate merging behavioural
#' responses with gaze samples.
#'
#' @format A data frame with 66,350 rows and 78 columns. Notable columns
#'   include participant and session identifiers (e.g.,
#'   \code{Participant.Public.ID}, \code{Participant.Private.ID}), trial and
#'   spreadsheet metadata (\code{Trial.Number}, \code{Spreadsheet.Row},
#'   \code{Zone.Name}), response data (\code{Reaction.Time}, \code{Response},
#'   \code{Correct}), trial design columns (\code{ANSWER}, \code{targetword},
#'   \code{soundfile}, \code{trialtype}, the \code{tl*}/\code{tr*}/\code{bl*}/
#'   \code{br*} picture and code columns), and a \code{subject} identifier
#'   matched to \code{eyedata}.
#' @source Gorilla Experiment Builder (\url{https://gorilla.sc/}).
"behav_data"

#' Visual-world-paradigm fixation counts
#'
#' Aggregated fixation counts by participant, condition, and time bin for a
#' visual-world-paradigm analysis. Suitable for demonstrating empirical-logit
#' or growth-curve modelling of gaze proportions.
#'
#' @format A data frame with 814 rows and 6 columns:
#' \describe{
#'   \item{subject}{Participant identifier.}
#'   \item{condition_num}{Numeric coding of condition (e.g., \code{-0.5} /
#'     \code{0.5}) suitable as a contrast.}
#'   \item{time_bin}{Time bin relative to target onset (ms).}
#'   \item{condition}{Condition label (e.g., \code{"unrelated"}).}
#'   \item{fix}{Number of samples in the bin that fell on the target interest
#'     area.}
#'   \item{total_fix}{Total number of samples in the bin across all interest
#'     areas.}
#' }
#' @source Aggregated from the example webcam eye-tracking data shipped with
#'   this package.
"vwp_counts"
