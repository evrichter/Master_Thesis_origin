### LMER Analysis ###

# load packages
library(lme4) 
library(lmerTest) #for printing p-values (results are the same like when using lme4, but causes error message that model failed to converge)
library(dplyr)
library(ggplot2)

setwd("~/Downloads/Master_Thesis/3_SPR_Study_2/Results_SPR2_Plaus_avg/")

GP6 <- read.csv("GP6SPR_processed.csv") #read file after removing outliers

# create empty tables to add later residuals, estimated logRTs, p-values and coefficients
residuals <- data.frame(
  Region = character(0),
  Condition = character(0),
  Residual = numeric(0),
  SE_Residual = numeric(0)
)

logRT_estimated <- data.frame(
  Region = character(0),
  Condition = character(0),
  Estimated_logRT = numeric(0),
  SE_Estimated = numeric(0)
)

p_values <- data.frame(
  Region = character(0),
  p_value_plausibility_target = numeric(0),
  p_value_surprisal_distractor = numeric(0)
)

SPR_coefficients <- data.frame(
  Region = character(0),
  Estimate = character(0),
  Estimate_value = numeric(0),
  Estimate_error = numeric(0),
  Z_value = numeric(0)
)


# define regions and conditions
regions <- c("Pre-critical", "Critical", "Spillover", "Post-spillover") 
conditions <- c("A", "B", "C") 


### loop through each of the 4 regions ###
for (region in regions) 
{
  # take subset for each region (precritical, critical, spillover, postspillover)
  region_subset <- subset(GP6, Region == region)
  
  # standardise predictors (target plausibility (per-trial) and distractor surprisal (pre-test))
  region_subset$scaled_Plaus_per_region <- scale(region_subset$Plaus_target_avg)
  region_subset$scaled_Surprisaldist_per_region <- scale(region_subset$Surprisal_distractor)
  
  # invert predictor target plausibility
  region_subset$inverted_scaled_Plaus_per_region <- (region_subset$scaled_Plaus_per_region) * (-1)
  
  #log transform reading times
  region_subset$logRT_per_region <- log(region_subset$ReadingTime)
  
  
  # fit linear mixed-effects regression model for each region
  simple_model <- lmerTest::lmer(logRT_per_region ~ inverted_scaled_Plaus_per_region + scaled_Surprisaldist_per_region + 
                                       (1 + inverted_scaled_Plaus_per_region + scaled_Surprisaldist_per_region | Subject) + 
                                       (1 + inverted_scaled_Plaus_per_region + scaled_Surprisaldist_per_region | Item), data = region_subset)

  # standardise predictors (use average plausibility ratings per item and condition from plausibility norming study here)
  region_subset$scaled_Plaus_per_region_plaus <- scale(region_subset$Plaus_target_avg) #avg plaus ratings from pretest
  region_subset$scaled_Plaus_per_region_spr <- scale(region_subset$SPR_Plaus_Rating) #single plaus ratings from SPR study
  region_subset$scaled_Surprisaldist_per_region <- scale(region_subset$Surprisal_distractor) #surprisal predictor
  # invert predictors plausibility
  region_subset$inverted_scaled_Plaus_per_region_plaus <- (region_subset$scaled_Plaus_per_region_plaus) * (-1)
  region_subset$inverted_scaled_Plaus_per_region_spr <- (region_subset$scaled_Plaus_per_region_spr) * (-1)
  #log transform reading times
  region_subset$logRT_per_region <- log(region_subset$ReadingTime)
  
  
  # define and run the linear mixed-effects regression model
  complex_model <- lmer(logRT_per_region ~ inverted_scaled_Plaus_per_region_plaus + inverted_scaled_Plaus_per_region_spr + scaled_Surprisaldist_per_region + 
                          (1 + inverted_scaled_Plaus_per_region_plaus + inverted_scaled_Plaus_per_region_spr + scaled_Surprisaldist_per_region | Subject) + 
                          (1 + inverted_scaled_Plaus_per_region_plaus + inverted_scaled_Plaus_per_region_spr + scaled_Surprisaldist_per_region | Item), REML = FALSE, data = region_subset)
  
  
  # use REML = FALSE when comparing a simple to a complex (nested) model
  
  lrt <- anova(simple_model, complex_model)
  print(lrt) 
}