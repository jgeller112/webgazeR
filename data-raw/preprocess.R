# Load raw data from .csv file
eyedata <- read.csv("data-raw/combined_eye_data_mono.csv")
behav_data <- read.csv("data-raw/data_exp_189729-v5_task-yxtu.csv")
# Apply preprocessing...
# Save the cleaned data in the required R package location
usethis::use_data(eyedata)
usethis::use_data(behav_data)

# Labvanced/jsPsych/PsychoPy example datasets, used by their respective
# vignettes. These are bundled (rather than the vignettes reading the raw
# exports directly) because the package website re-executes vignette code
# live in the browser via quarto-webr, which has no access to this repo's
# files -- only to installed packages and their bundled data.
labvanced_files <- list.files(
  "data-raw/labvanced",
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)
labvanced_eyedata <- merge_webcam_files(file_paths = labvanced_files, kind = "labvanced")

# Scoped to one task, so `trial` is a plain numeral rather than the default
# composite id -- convenient for filtering/sorting demos in the vignette.
labvanced_eyedata_sentence <- merge_webcam_files(
  file_paths = labvanced_files,
  kind = "labvanced",
  task = "sentence"
)

jspsych_eyedata <- merge_webcam_files(
  file_paths = "data-raw/jspsych/subject-6085a5370e7aa.json",
  kind = "jspsych",
  col_map = list(subject = "subject", trial = "trial_index"),
  array_col = "webgazer_data",
  array_key = TRUE,
  webgazer_target_names = c(
    "#jspsych-free-sort-draggable-0" = "target_0",
    "#jspsych-free-sort-draggable-1" = "target_1",
    "#jspsych-free-sort-draggable-2" = "target_2",
    "#jspsych-free-sort-draggable-3" = "target_3"
  )
)

# A separate jsPsych experiment (kept separate, not merged with
# jspsych_eyedata, since they're different studies) that uses the webgazer
# extension's own targets shape rather than jspsych-free-sort's.
jspsych_eyedata2 <- merge_webcam_files(
  file_paths = "data-raw/jspsych/subject-6085bd39a5358.json",
  kind = "jspsych",
  col_map = list(subject = "subject_id", trial = "trial_index"),
  array_col = "webgazer_data",
  array_key = TRUE,
  webgazer_target_names = c("#scenes" = "scene_roi")
)

psychopy_eyedata <- merge_webcam_files(
  file_paths = "data-raw/psychopy/STU172607h07554f66.csv",
  kind = "psychopy",
  col_map = list(subject = "pid", trial = "Problem_id"),
  array_col = "TaskGazeArray",
  array_key = FALSE
)

usethis::use_data(labvanced_eyedata, overwrite = TRUE)
usethis::use_data(labvanced_eyedata_sentence, overwrite = TRUE)
usethis::use_data(jspsych_eyedata, overwrite = TRUE)
usethis::use_data(jspsych_eyedata2, overwrite = TRUE)
usethis::use_data(psychopy_eyedata, overwrite = TRUE)
