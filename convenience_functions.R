conditional_mean <- function(task, cue, validity, outcome) {
  
  task <- task
  
  return(mean(
    task[task$cue_type == cue & task$cue_validity == validity,
         outcome
         ]
  ))
}

summary_dataframe <- function() {
  
  df <- data.frame(
    
    task_type = c("exogenous", "gaze"),
    cue_validity = c("valid", "neutral", "invalid"),
    mean_rt = c(
      mean(
        
      )
    )
    
  )
  
}