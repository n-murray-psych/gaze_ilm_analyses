wd <- "/Users/nickmurray/Desktop/Klein_Lab_Projects/gaze_ilm_analyses"
source("convenience_functions.r")

library(tidyverse)

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
detection_summary <- summary_dataframe(detection_task, FALSE)
line_summary <- summary_dataframe(line_task, TRUE)
line_summary$combined_cue_type_location <- paste0(
  line_summary$cue_type, 
  " ",
  line_summary$cue_location
)
line_task_summary_left_motion <- subset(line_summary, 
                                        task_requirement == "leftward real line motion rating"
                                          )
line_task_summary_right_motion <- subset(line_summary, 
                                        task_requirement == "rightward real line motion rating"
                                          )

line_task_summary_illusory_motion <- subset(line_summary, 
                                        task_requirement == "illusory line motion rating"
                                          )



