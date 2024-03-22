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

contrast_matrix <- matrix(c(
  1, -1, 0, # valid vs invalid, neutral as reference
  1, 0, -1, # valid vs neutral, invalid as reference
  0, 1, -1 # invalid vs neutral, valid as reference
), ncol = 3, byrow = TRUE)

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

emmeans(rt_anova_detection$aov, ~ cue_type * cue_validity)

aov_ez(data = detection_task,
                              dv = reaction_time,
                              wid = participant,
                              within = .(cue_type, cue_validity),
                              return_aov = TRUE,
                              detailed = FALSE
                              )


# Using afex's aov_ez so I can do planned contrasts with aov_ez
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

