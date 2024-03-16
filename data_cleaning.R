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

  
  
