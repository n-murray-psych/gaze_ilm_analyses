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

line_task_ls_interaction <- lsmeans(line_task_anova, c("cue_type", "task_requirement"))
update(pairs(line_task_ls_interaction), by = NULL, adjust = "holm")
