# Load data and convenience functions
setwd("/Users/nickmurray/Desktop/Klein_Lab_Projects/gaze_ilm_analyses")
source("convenience_functions.R")
source("data_cleaning.R")

# Import packages
library(gridExtra)
library(afex)
library(ez)
library(emmeans)

################################################################################
# DETECTION TASK ANOVAS
################################################################################
rt_anova_detection <- ezANOVA(data = detection_task,
                              dv = reaction_time,
                              wid = participant,
                              within = .(cue_type, cue_validity),
                              return_aov = TRUE,
                              detailed = FALSE, 
                              )

acc_anova_detection <- ezANOVA(data = detection_task,
                              dv = accuracy,
                              wid = participant,
                              within = .(cue_type, cue_validity),
                              return_aov = TRUE,
                              detailed = FALSE
                              )


# Detection ANOVAs and contrasts.
detection_rt_anova <- aov_ez(
  id = "participant", 
  dv = "reaction_time", 
  data = detection_task, 
  within = c("cue_type", "cue_validity")
)

rt_ls <- lsmeans(detection_rt_anova, c("cue_validity"))
update(pairs(rt_ls), by = NULL, adjust = "holm")

rt_ls_interaction <- lsmeans(detection_rt_anova, c("cue_validity", "cue_type"))
update(pairs(rt_ls_interaction), by = NULL, adjust = "holm")

detection_accuracy_anova <- aov_ez(
  id = "participant", 
  dv = "accuracy", 
  data = detection_task, 
  within = c("cue_type", "cue_validity")
)

accuracy_ls <- lsmeans(detection_accuracy_anova, c("cue_validity"))
update(pairs(accuracy_ls), by = NULL, adjust = "holm")

accuracy_ls_interaction <- lsmeans(detection_accuracy_anova, c("cue_validity", "cue_type"))
update(pairs(accuracy_ls_interaction), by = NULL, adjust = "holm")

# Line rating ANOVAs and contrasts.
line_task_anova <- aov_ez(
  id = "participant", 
  dv = "response", 
  data = line_task, 
  within = c("cue_location", "cue_type", "task_requirement")
)
line_task_ls_cue_location <- lsmeans(line_task_anova, c("cue_location"))
update(pairs(line_task_ls_cue_location), by = NULL, adjust = "holm")

line_task_ls_cue_type <- lsmeans(line_task_anova, c("cue_type"))
update(pairs(line_task_ls_cue_type), by = NULL, adjust = "holm")

line_task_ls_line_type <- lsmeans(line_task_anova, c("task_requirement"))
update(pairs(line_task_ls_line_type), by = NULL, adjust = "holm")

line_task_ls_location_type_interaction <- lsmeans(line_task_anova, c("cue_location", "cue_type"))
update(pairs(line_task_ls_location_type_interaction), by = NULL, adjust = "holm")

line_task_ls_location_type_interaction <- lsmeans(line_task_anova, c("cue_location", "task_requirement"))
update(pairs(line_task_ls_location_type_interaction), by = NULL, adjust = "holm")

line_task_ls_interaction <- lsmeans(line_task_anova, c("cue_type", "task_requirement", "cue_location"))
update(pairs(line_task_ls_interaction), by = NULL, adjust = "holm")

# TESTING VISUALIZATIONS WITH AFEX
afex_plot(line_task_anova, c("cue_location", "cue_type"), error = "within", return = "data")
afex_plot(detection_rt_anova, c("cue_type", "cue_validity"), error = "within")


illusory_motion <- data.frame(
  cue_type = c(
    "Gaze Right", "Gaze Neutral", "Gaze Left", 
    "Exogenous Right", "Exogenous Neutral", "Exogenous Left"
  ),
  estimate = c(
    0.094, 0.076, 0.097, 
    -0.133, 0.023, 0.230
  ),
  lowci = c(
    0.012, 0.012, 0.045, 
    -0.196, -0.007, 0.133
  ), 
  highci = c(
    0.175, 0.140, 0.147, 
    -0.070, 0.054, 0.325
  )
)

rightward_motion <- illusory_motion <- data.frame(
  cue_type = c(
    "Gaze Right", "Gaze Neutral", "Gaze Left", 
    "Exogenous Right", "Exogenous Neutral", "Exogenous Left"
  ),
  estimate = c(
    0.202, 0.213, 0.230, 
    0.173, 0.10, 0.226
  ),
  lowci = c(
    0.111, 0.154, 0.153, 
    0.089, 0.044, 0.162 
  ), 
  highci = c(
    0.293, 0.272, 0.306,
    0.257, 0.153, 0.289
  )
)

leftward_motion <- illusory_motion <- data.frame(
  cue_type = c(
    "Gaze Right", "Gaze Neutral", "Gaze Left", 
    "Exogenous Right", "Exogenous Neutral", "Exogenous Left"
  ),
  estimate = c(
    -0.190, -0.140, -0.156, 
    -0.178, -0.025, -0.060
  ),
  lowci = c(
    -0.276, -0.220, -0.232, 
    -0.252, -0.115, -0.203
  ), 
  highci = c(
    -0.10, -0.06, -0.08, 
    -0.10, 0.070, 0.09
  )
)

ilm_graph <- function(data, title) {
  
    ggplot(
    data = data,
    aes(x = estimate, y = cue_type)
  ) + 
    geom_point() + 
    geom_errorbar(aes(xmin = lowci, xmax = highci)) + 
    labs(title = title) + 
    xlab("Line Motion Rating (Left to Right") + 
    ylab("Cue Type") + 
    geom_vline(xintercept = 0, linetype = "dashed") +
    xlim(-.5, .5) + 
    theme_classic()
  
}

static_line_plot <- ilm_graph(illusory_motion, "Illusory Motion")
right_line_plot <- ilm_graph(rightward_motion, "Rightward Real Line Motion")
left_line_plot <- ilm_graph(leftward_motion, "Leftward Real Line Motion")

rt_anova_plot <- afex_plot(detection_rt_anova, "cue_validity", error = "within", 
          trace = "cue_type",
          data_plot = FALSE,
          legend_title = "Cue Type"
          ) + 
  theme_classic() + 
  labs(x = "Cue Validity", y = "Reaction Time (ms)")
  
accuracy_anova_plot <- afex_plot(detection_accuracy_anova, "cue_validity", error = "within", 
          trace = "cue_type",
          data_plot = FALSE,
          legend_title = "Cue Type"
          ) + 
  theme_classic() + 
  labs(x = "Cue Validity", y = "Reaction Time (ms)")

ggsave("rt_anova_plot.pdf", rt_anova_plot, width = 4, height = 4, dpi = 300)
ggsave("accuracy_anova_plot.pdf", accuracy_anova_plot, width = 4, height = 4, dpi = 300)
ggsave("static_line_plot.pdf", static_line_plot, width = 10, height = 4, dpi = 300)
ggsave("right_line_plot.pdf", right_line_plot, width = 10, height = 4, dpi = 300)
ggsave("left_line_plot.pdf", left_line_plot, width = 10, height = 4, dpi = 300)



afex_plot(line_task_anova, c("cue_location", "cue_type", "task_requirement"), error = "within", return = "data")
afex_plot(line_task_anova, c("cue_location", "task_requirement"), error = "within", return = "data")

illusory_motion_no_split
