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
  labs(title = "Cuing Effect") + 
  theme_classic() + 
  labs(colour = "Cue Type")

detection_accuracy_graph <- ggplot(data = detection_summary,
       aes(x = cue_validity, y = mean_accuracy, group = cue_type, colour = cue_type)
       ) + 
  geom_point() + 
  geom_smooth(stat = "identity") + 
  xlab("Cue Validity") + 
  ylab("Mean Accuracy (%)") + 
  labs(title = "Cuing Effect \n Accuracies") + 
  theme_classic()

grid.arrange(detection_rt_graph, detection_accuracy_graph, nrow = 1)


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
  xlim(-.5, .5) + 
  theme_classic()

ggsave("ilm_plot.pdf", illusory_motion_graph, width = 5, height = 2, dpi = 300)
ggsave("cuing_effect_rt_plot.pdf", detection_rt_graph, width = 5, height = 2, dpi = 300)

grid.arrange(
  detection_rt_graph, 
  illusory_motion_graph,
  nrow = 2
) # Just save this with 5 (w) x 6 (h) dimensions 

# TESTING VISUALIZATIONS WITH AFEX




