# Calculate conditional means based on cue type and cue validity for a given
  # task dataframe and outcome variable.
conditional_mean <- function(task_frame, cue, validity, outcome, line_rating = FALSE) {
  
  task <- task_frame
  
  return(mean(
    task[task$cue_type == cue & task$cue_validity == validity,
         outcome
         ]
  ))
}

conditional_sd <- function(task_frame, cue, validity, outcome) {
  
  task <- task_frame
  
  return(sd(
    task[task$cue_type == cue & task$cue_validity == validity,
         outcome
         ]
  ))
  
}

conditional_median <- function(task_frame, cue, validity, outcome) {
  
  task <- task_frame
  
  return(median(
    task[task$cue_type == cue & task$cue_validity == validity,
         outcome
         ]
  ))
  
}

# Create dataframe with summary statistics (means; SD; medians) of reaction time and accuracy
  # for a respective task.
summary_dataframe <- function(task_frame) {
  
  task <- task_frame
  
  df <- data.frame(
    cue_type = c("exogenous", "gaze", 
                 "exogenous", "gaze", 
                 "exogenous", "gaze"
                 ),
    cue_validity = c("valid", "neutral", "invalid",
                     "valid", "neutral", "invalid"
                     )
  )
  
  df$mean_rt <- NA
  df$sd_rt <- NA
  df$median_rt <- NA
  df$mean_accuracy <- NA
  df$sd_accuracy <- NA

  for (row in 1:nrow(df)) {
    
    df[row, "mean_rt"] <- conditional_mean(task,
                                           df[row, "cue_type"],
                                           df[row, "cue_validity"],
                                           "reaction_time"
                                           )
    
    df[row, "sd_rt"] <- conditional_sd(task,
                                           df[row, "cue_type"],
                                           df[row, "cue_validity"],
                                           "reaction_time"
                                           )
    
    df[row, "median_rt"] <- conditional_median(task,
                                           df[row, "cue_type"],
                                           df[row, "cue_validity"],
                                           "reaction_time"
                                           )
    
    df[row, "mean_accuracy"] <- conditional_mean(task,
                                           df[row, "cue_type"],
                                           df[row, "cue_validity"],
                                           "accuracy"
                                           )
    
    df[row, "sd_accuracy"] <- conditional_sd(task,
                                           df[row, "cue_type"],
                                           df[row, "cue_validity"],
                                           "accuracy"
                                           )
    
  }
  
  return(df)
  
}
