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
                              detailed = FALSE
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


