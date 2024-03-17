wd <- "/Users/nickmurray/Desktop/Klein_Lab_Projects/gaze_ilm_analyses"

library(tidyverse)
library(gridExtra)
library(afex)
library(ez)

# Read data

data_path <- "/Users/nickmurray/Desktop/Klein_Lab_Projects/gaze_ilm_analyses/raw_data"

participant_filenames <- list.files(path = data_path)

df <- NULL

setwd(data_path)

for (file in participant_filenames) {
  
  participant_data <- read.csv(file)
  
  df <- rbind(df, participant_data)
  
}

setwd(wd)

################################################################################
# DATA CLEANING
################################################################################

# Detection task

detection_task <- subset(
  df, 
  df$task_requirement == "detection"
)

  # Coding cue validity
  detection_task$cue_validity <- ifelse(
    detection_task$cue_location == detection_task$target_location, 
    "valid", 
    ifelse(
      detection_task$cue_location == "neutral", "neutral", 
      "invalid"
    )
  )
  
  # Coding accuracy
  detection_task$accuracy <- ifelse(
    detection_task$target_location == detection_task$response, 
    1, 0
  )
  
# Line motion rating task
line_task <- subset(
  df,
  df$task_requirement != "detection"
)

  # Recode line motion ratings from -50 to +50
  line_task$response <- as.numeric(line_task$response)
  line_task$response <- .5 - line_task$response
  line_task$response <- -1 * line_task$response

################################################################################
# SUMMARY DATAFRAMES
################################################################################
detection_task_summary <- summary_dataframe(detection_task, FALSE)
line_task_summary <- summary_dataframe(line_task, TRUE)
