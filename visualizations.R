library(tidyverse)
library(gridExtra)

source("data_cleaning.R")

detection_rt_graph <- ggplot(data = detection_summary,
       aes(x = cue_validity, y = mean_rt, group = cue_type, colour = cue_type)
       ) + 
  geom_point() + 
  geom_smooth(stat = "identity") + 
  xlab("Cue Validity") + 
  ylab("Mean Reaction Time (ms)") + 
  labs(title = "Cuing Effect Reaction Times")

detection_accuracy_graph <- ggplot(data = detection_summary,
       aes(x = cue_validity, y = mean_accuracy, group = cue_type, colour = cue_type)
       ) + 
  geom_point() + 
  geom_smooth(stat = "identity") + 
  xlab("Cue Validity") + 
  ylab("Mean Accuracy (%)") + 
  labs(title = "Cuing Effect Accuracies")


motion_graph <- ggplot(
  data = line_task_summary,
  aes(
    x = mean_line_rating, 
    y = combined_cue_type_location,
    colour = task_type
    )
) + 
  geom_point() + 
  geom_errorbar(
    aes(
      xmin = mean_line_rating - sd_line_rating,
        xmax = mean_line_rating + sd_line_rating
    )
  )


left_motion_graph <- ggplot(
  data = line_task_summary_left_motion,
  aes(
    x = mean_line_rating, 
    y = combined_cue_type_location
  )
) + 
  geom_point() + 
  geom_errorbar(
    aes(
      xmin = mean_line_rating - sd_line_rating,
        xmax = mean_line_rating + sd_line_rating
    )
  ) + 
  labs(title = "Leftward Motion"
       ) + 
  xlab("Line Motion Rating (Left to Right") + 
  ylab("Cue Type") + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  xlim(-.5, .5)

right_motion_graph <- ggplot(
  data = line_task_summary_right_motion,
  aes(
    x = mean_line_rating, 
    y = combined_cue_type_location
  )
) + 
  geom_point() + 
  geom_errorbar(
    aes(
      xmin = mean_line_rating - sd_line_rating,
        xmax = mean_line_rating + sd_line_rating
    )
  ) + 
  labs(title = "Rightward Motion"
       ) + 
  xlab("Line Motion Rating (Left to Right") + 
  ylab("Cue Type") + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  xlim(-.5, .5)

illusory_motion_graph <- ggplot(
  data = line_task_summary_illusory_motion,
  aes(
    x = mean_line_rating, 
    y = combined_cue_type_location
  )
) + 
  geom_point() + 
  geom_errorbar(
    aes(
      xmin = mean_line_rating - sd_line_rating,
        xmax = mean_line_rating + sd_line_rating
    )
  ) + 
  labs(title = "Illusory Motion"
       ) + 
  xlab("Line Motion Rating (Left to Right") + 
  ylab("Cue Type") + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  xlim(-.5, .5)
