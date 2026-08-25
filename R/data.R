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

#' Example Labvanced webcam eye-tracking data
#'
#' The output of \code{merge_webcam_files(kind = "labvanced")} run on two
#' participants' Labvanced timeseries, trials, and sessions exports. Used in
#' the \code{Labvanced} vignette to demonstrate the merging pipeline without
#' requiring the raw export files (which aren't available in the browser-based
#' interactive code chunks the package website runs).
#'
#' @format A data frame with 75,958 rows and 27 columns, in the standardized
#'   long format \code{merge_webcam_files()} returns
#'   (\code{subject}/\code{trial}/\code{time}/\code{x}/\code{y}), plus
#'   \code{confidence}, trial-level metadata from the trials export (e.g.
#'   \code{task_name}, \code{condition_id}), and screen/window size from the
#'   sessions export.
#' @source Labvanced (\url{https://www.labvanced.com/}).
"labvanced_eyedata"

#' Example Labvanced webcam eye-tracking data, scoped to one task
#'
#' The same data as \code{\link{labvanced_eyedata}}, but merged with
#' \code{merge_webcam_files(kind = "labvanced", task = "sentence")} instead of
#' the default call -- so \code{trial} is a plain numeral (e.g. \code{"1"},
#' \code{"2"}, ...) rather than the safe-but-verbose default composite id.
#' Used in the \code{Labvanced} vignette wherever a numeral trial id is
#' convenient (e.g. filtering/sorting, or the gaze-trace plot).
#'
#' @format A data frame with 55,114 rows and 27 columns, in the same
#'   standardized long format as \code{\link{labvanced_eyedata}}.
#' @source Labvanced (\url{https://www.labvanced.com/}).
"labvanced_eyedata_sentence"

#' Example jsPsych webcam eye-tracking data (free-sort experiment)
#'
#' The output of \code{merge_webcam_files(kind = "jspsych")} run on a jsPsych
#' + webgazer extension export from an eye-tracking free-sort experiment in
#' Serrano-Carot, Angele, Xu, & Vasilev (2025). Used in the \code{jsPsych}
#' vignette to demonstrate the merging pipeline, including flattening a
#' free-sort-plugin-style \code{webgazer_targets} field, without requiring the
#' raw JSON export (which isn't available in the browser-based interactive
#' code chunks the package website runs).
#'
#' @format A data frame with 9,279 rows and 47 columns, in the standardized
#'   long format \code{merge_webcam_files()} returns
#'   (\code{subject}/\code{trial}/\code{time}/\code{x}/\code{y}), plus
#'   trial-level fields (e.g. \code{trial_type}, \code{rt}) and the flattened
#'   \code{target_0}-\code{target_3} boundary columns.
#' @source James, Ryskin, Hartshorne, et al. (2025). \emph{What Paradigms Can
#'   Webcam Eye-Tracking Be Used For? Attempted Replications of Five Cognitive
#'   Science Experiments}. Collabra: Psychology, 11(1), 140755.
#'   \url{https://doi.org/10.1525/collabra.140755}
"jspsych_eyedata"

#' Example jsPsych webcam eye-tracking data (webgazer extension `targets`)
#'
#' The output of \code{merge_webcam_files(kind = "jspsych")} run on a second,
#' separate jsPsych + webgazer extension export from Serrano-Carot, Angele,
#' Xu, & Vasilev (2025), which records \code{webgazer_targets} in the
#' extension's own keyed-by-selector shape (\code{"#scenes"}) rather than the
#' free-sort plugin shape in \code{\link{jspsych_eyedata}}. Kept as a separate
#' dataset -- not merged with \code{jspsych_eyedata} -- since it's a distinct
#' experiment with its own subject and design.
#'
#' @format A data frame with 3,686 rows and 36 columns, in the standardized
#'   long format \code{merge_webcam_files()} returns
#'   (\code{subject}/\code{trial}/\code{time}/\code{x}/\code{y}), plus
#'   trial-level fields and the flattened \code{scene_roi_*} boundary columns.
#' @source James, Ryskin, Hartshorne, et al. (2025). \emph{What Paradigms Can
#'   Webcam Eye-Tracking Be Used For? Attempted Replications of Five Cognitive
#'   Science Experiments}. Collabra: Psychology, 11(1), 140755.
#'   \url{https://doi.org/10.1525/collabra.140755}
"jspsych_eyedata2"

#' Example PsychoPy webcam eye-tracking data
#'
#' The output of \code{merge_webcam_files(kind = "psychopy")} run on a
#' PsychoPy export with per-trial gaze arrays. Used in the \code{PsychoPy}
#' vignette to demonstrate the merging pipeline, including the
#' seconds-to-milliseconds \code{time} conversion, without requiring the raw
#' CSV export (which isn't available in the browser-based interactive code
#' chunks the package website runs).
#'
#' @format A data frame with 13,006 rows and 19 columns, in the standardized
#'   long format \code{merge_webcam_files()} returns
#'   (\code{subject}/\code{trial}/\code{time}/\code{x}/\code{y}), plus
#'   trial-level fields (e.g. \code{task_correct}, \code{hoo_id},
#'   \code{win_width}, \code{win_height}).
#' @source PsychoPy (\url{https://www.psychopy.org/}).
"psychopy_eyedata"
