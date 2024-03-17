# Calculate conditional means based on cue type and cue validity for a given
  # task dataframe and outcome variable.
conditional_mean <- function(task_frame, line_task = FALSE, cue, validity = NA,
                             requirement = NA, 
                             location = NA,
                             outcome
                             ) {
  
  task <- task_frame
  
  if (isTRUE(line_task)) {
    return(mean(
      task[task$cue_type == cue & task$task_requirement == requirement & 
             task$cue_location == location, 
           outcome
             ]
  ))
  } else {
    return(mean(
    task[task$cue_type == cue & task$cue_validity == validity,
         outcome
         ]
    ))
  }
}

conditional_sd <- function(task_frame, line_task = FALSE, cue, validity = NA,
                             requirement = NA, 
                             location = NA,
                             outcome
                             ) {
  
  task <- task_frame
  
  if (isTRUE(line_task)) {
    return(sd(
      task[task$cue_type == cue & task$task_requirement == requirement & 
             task$cue_location == location, 
           outcome
             ]
  ))
  } else {
    return(sd(
    task[task$cue_type == cue & task$cue_validity == validity,
         outcome
         ]
    ))
  }
}

conditional_median <- function(task_frame, line_task = FALSE, cue, validity = NA,
                             requirement = NA, 
                             location = NA,
                             outcome
                             ) {
  
  task <- task_frame
  
  if (isTRUE(line_task)) {
    return(median(
      task[task$cue_type == cue & task$task_requirement == requirement & 
             task$cue_location == location, 
           outcome
             ]
  ))
  } else {
    return(median(
    task[task$cue_type == cue & task$cue_validity == validity,
         outcome
         ]
    ))
  }
}


# Create dataframe with summary statistics (means; SD; medians) of reaction time and accuracy
  # for a respective task.summary_dataframe <- function(task_frame, line_task = FALSE) {
 
summary_dataframe <- function(task_frame, line_task = FALSE) {
  
  cue_type <- unique(task_frame$cue_type)
  cue_validity <- unique(task_frame$cue_validity)
  cue_location <- unique(task_frame$cue_location)
  task_requirement <- unique(task_frame$task_requirement)
  
  if (as.character(substitute(task_frame)) == "line_task") {
    
    df <- expand.grid(
    cue_type = cue_type, 
    cue_validity = NA,
    cue_location = cue_location,
    task_requirement = task_requirement
    
  )
  
  df <- as.data.frame(df)
  } else {
    
    df <- expand.grid(
    cue_type = cue_type, 
    cue_validity = cue_validity,
    cue_location = NA,
    task_requirement = NA
    
  )
    
  }
  
  df$mean_rt <- NA
  df$sd_rt <- NA
  df$median_rt <- NA
  df$mean_accuracy <- NA
  df$sd_accuracy <- NA
  df$mean_line_rating <- NA
  df$sd_line_rating <- NA
  
  for (row in 1:nrow(df)) {
    
    df[row, "mean_rt"] <- conditional_mean(task_frame,  
                                            line_task, 
                                           df[row, "cue_type"], 
                                           df[row, "cue_validity"],
                                           df[row, "task_requirement"],
                                           df[row, "cue_location"],
                                           "reaction_time"
                                           )
    
    df[row, "sd_rt"] <- conditional_sd(task_frame,  
                                            line_task, 
                                           df[row, "cue_type"], 
                                           df[row, "cue_validity"],
                                           df[row, "task_requirement"],
                                           df[row, "cue_location"],
                                           "reaction_time"
                                           )
    
    df[row, "median_rt"] <- conditional_median(task_frame,  
                                            line_task, 
                                           df[row, "cue_type"], 
                                           df[row, "cue_validity"],
                                           df[row, "task_requirement"],
                                           df[row, "cue_location"],
                                           "reaction_time"
                                           )
    
    df[row, "mean_accuracy"] <- conditional_mean(task_frame, 
                                            line_task,  
                                           df[row, "cue_type"], 
                                           df[row, "cue_validity"],
                                           df[row, "task_requirement"],
                                           df[row, "cue_location"],
                                           "accuracy"
                                           )
    
    df[row, "sd_accuracy"] <- conditional_sd(task_frame,  
                                            line_task, 
                                           df[row, "cue_type"], 
                                           df[row, "cue_validity"],
                                           df[row, "task_requirement"],
                                           df[row, "cue_location"],
                                           "accuracy"
                                           )
    
    df[row, "mean_line_rating"] <- conditional_mean(task_frame, 
                                            line_task, 
                                           df[row, "cue_type"], 
                                           df[row, "cue_validity"],
                                           df[row, "task_requirement"],
                                           df[row, "cue_location"],
                                           "response"
                                           )
    
    df[row, "sd_line_rating"] <- conditional_sd(task_frame,  
                                            line_task, 
                                           df[row, "cue_type"], 
                                           df[row, "cue_validity"],
                                           df[row, "task_requirement"],
                                           df[row, "cue_location"],
                                           "response"
                                           )
    
}
  
  return(df)
  
}


