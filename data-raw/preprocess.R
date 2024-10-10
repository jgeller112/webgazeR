# Load raw data from .csv file
eyedata <- read.csv("data-raw/combined_eye_data_mono.csv")
behav_data <- read.csv("data-raw/data_exp_189729-v5_task-yxtu.csv")
# Apply preprocessing...
# Save the cleaned data in the required R package location
usethis::use_data(eyedata)
usethis::use_data(behav_data)
