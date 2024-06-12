### LMER Analysis ###

# load packages
library(lme4) 
library(lmerTest) #for printing p-values (results are the same like when using lme4, but causes error message that model failed to converge)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)


setwd("~/Downloads/Master_Thesis/3_SPR_Study/Results_SPR_Plaus_single/")

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
  region_subset$scaled_Plaus_per_region <- scale(region_subset$SPR_Plaus_Rating)
  region_subset$scaled_Surprisaldist_per_region <- scale(region_subset$Surprisal_distractor)
  
  # invert predictor target plausibility
  region_subset$inverted_scaled_Plaus_per_region <- (region_subset$scaled_Plaus_per_region) * (-1)
  
  #log transform reading times
  region_subset$logRT_per_region <- log(region_subset$ReadingTime)
  
  
  # fit linear mixed-effects regression model for each region
  model_per_region <- lmerTest::lmer(logRT_per_region ~ inverted_scaled_Plaus_per_region + scaled_Surprisaldist_per_region + 
                                       (1 + inverted_scaled_Plaus_per_region + scaled_Surprisaldist_per_region | Subject) + 
                                       (1 + inverted_scaled_Plaus_per_region + scaled_Surprisaldist_per_region | Item), data = region_subset)
  
  # print summary of the model
  summary_per_region <- summary(model_per_region)
  print(summary_per_region)
  
  # calculate p-values
  p_values_per_region <- summary_per_region$coefficients[, "Pr(>|t|)"]
  new_row_p_value <- data.frame(Region = region, 
                                p_value_plausibility_target = p_values_per_region[2], 
                                p_value_surprisal_distractor = p_values_per_region[3])
  p_values <- rbind(p_values, new_row_p_value) # add to p-values table
  
  # extract intercept and coefficients (target plausibility and distractor surprisal) added to intercept
  coefficients_per_region <- summary_per_region$coefficients
  intercept <- coefficients_per_region["(Intercept)", 1]
  plaus_target_coeff <- coefficients_per_region["(Intercept)", 1] + coefficients_per_region["inverted_scaled_Plaus_per_region", 1]
  surprisal_distractor_coeff <- coefficients_per_region["(Intercept)", 1] + coefficients_per_region["scaled_Surprisaldist_per_region", 1]
  
  # add error bars for intercept and coefficients (target plausibility and distractor surprisal) 
  new_row_coefficient <- data.frame(Region = region, 
                                    Estimate = "Intercept", 
                                    Estimate_value = intercept, 
                                    Estimate_error = coefficients_per_region["(Intercept)", 2],
                                    Z_value = 0)
  SPR_coefficients <- rbind(SPR_coefficients, new_row_coefficient) # add to coefficients table
  
  new_row_coefficient <- data.frame(Region = region,
                                    Estimate = "Plausability",
                                    Estimate_value = plaus_target_coeff,
                                    Estimate_error = coefficients_per_region["inverted_scaled_Plaus_per_region", 2],
                                    Z_value = coefficients_per_region["inverted_scaled_Plaus_per_region", 1] / coefficients_per_region["inverted_scaled_Plaus_per_region", 2])
  SPR_coefficients <- rbind(SPR_coefficients, new_row_coefficient) # add to coefficients table
  
  new_row_coefficient <- data.frame(Region = region,
                                    Estimate = "Surprisal", 
                                    Estimate_value = surprisal_distractor_coeff, 
                                    Estimate_error = coefficients_per_region["scaled_Surprisaldist_per_region", 2],
                                    Z_value = coefficients_per_region["scaled_Surprisaldist_per_region", 1] / coefficients_per_region["scaled_Surprisaldist_per_region", 2])
  SPR_coefficients <- rbind(SPR_coefficients, new_row_coefficient) # add to coefficients table
  
  
  
  ### loop through each of the 3 conditions ###
  for (condition in conditions)
  {
    # extract subset for each condition
    region_per_condition <- subset(region_subset, Condition == condition)
    # predict logRTs for each condition separately, within each of the 4 regions
    region_per_condition$region_per_condition_Predicted <- predict(model_per_region, newdata = region_per_condition,  type = "response")
    
    # print observed mean logRTs
    region_per_condition_logRT_observed <- mean(region_per_condition$logRT_per_region)
    
    # print estimated mean logRTs
    region_per_condition_logRT_estimated <- mean(region_per_condition$region_per_condition_Predicted, na.rm = TRUE)
    
    # calculate residuals (observed logRTs - estimated logRTs)
    Residual_region_per_condition <- mean(region_per_condition$logRT_per_region) - mean(region_per_condition$region_per_condition_Predicted, na.rm = TRUE)
    
    # calculate standard error for residuals
    SE_residuals_region_per_condition <- sqrt(sd(region_per_condition$logRT, na.rm = TRUE)^2/length(region_per_condition$logRT) + sd(region_per_condition$region_per_condition_Predicted, na.rm = TRUE)^2/length(region_per_condition$region_per_condition_Predicted))
    
    # add to residuals table
    new_row_residuals <- data.frame(Region = region, Condition = condition, Residual = Residual_region_per_condition, SE_Residual = SE_residuals_region_per_condition)
    residuals <- rbind(residuals, new_row_residuals)
    
    # calculate standard error for estimated logRTs
    SE_estimated_region_per_condition <- sd(region_per_condition$region_per_condition_Predicted, na.rm = TRUE) / sqrt(length(region_per_condition$region_per_condition_Predicted)) 
    
    # add to estimated logRTs table
    new_row_logRT_estimated <- data.frame(Region = region, Condition = condition, Estimated_logRT = region_per_condition_logRT_estimated, SE_Estimated = SE_estimated_region_per_condition)
    logRT_estimated <- rbind(logRT_estimated, new_row_logRT_estimated)
  }
}

# plot effect sizes (z-vaues)
Effect_sizes <- subset(SPR_coefficients, Estimate != 'Intercept')

p1 <- ggplot(Effect_sizes, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                               y = Z_value, color = Estimate, group = Estimate)) + geom_point(shape = 4, size = 3.5, stroke = 0.8) + geom_line(linewidth=0.5) + ylim (-5.2, 5)
p1 <- p1 + geom_hline(yintercept=0, linetype=2)
p1 <- p1 + theme_minimal()
p1 <- p1 + scale_color_manual(name="Coefficients", labels=c( "Target Plausibility", "Distractor Surprisal"), values=c("#FF00FF", "#00FFFF"))
p1 <- p1 + labs(x="Region", y="Z-values", title = "Single Online Plausibility + GPT-2 Surprisal") 
p1 <- p1 + theme(legend.position="none", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 7)) + theme(plot.title = element_text(size=8)) + theme(plot.margin = margin(t = 0.5, r = 0.2, b = 0, l = 0.2, unit = "cm"))
p1 <- p1 + geom_point(data = Effect_sizes, aes(3, -5.2, group = Estimate), size = 2, shape = 19, color = "#FF00FF")
p1 <- p1 + geom_point(data = Effect_sizes, aes(4, -5.2, group = Estimate), size = 2, shape = 19, color = "#FF00FF")
p1 

setwd("~/Downloads/Master_Thesis/3_SPR_Study/Results_SPR_Plaus_Leo_LM/")

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
  region_subset$scaled_Plaus_per_region <- scale(region_subset$SPR_Plaus_Rating)
  region_subset$scaled_Surprisaldist_per_region <- scale(region_subset$LeoLM_dist)
  
  # invert predictor target plausibility
  region_subset$inverted_scaled_Plaus_per_region <- (region_subset$scaled_Plaus_per_region) * (-1)
  
  #log transform reading times
  region_subset$logRT_per_region <- log(region_subset$ReadingTime)
  
  
  # fit linear mixed-effects regression model for each region
  model_per_region <- lmerTest::lmer(logRT_per_region ~ inverted_scaled_Plaus_per_region + scaled_Surprisaldist_per_region + 
                                       (1 + inverted_scaled_Plaus_per_region + scaled_Surprisaldist_per_region | Subject) + 
                                       (1 + inverted_scaled_Plaus_per_region + scaled_Surprisaldist_per_region | Item), data = region_subset)
  
  # print summary of the model
  summary_per_region <- summary(model_per_region)
  print(summary_per_region)
  
  # calculate p-values
  p_values_per_region <- summary_per_region$coefficients[, "Pr(>|t|)"]
  new_row_p_value <- data.frame(Region = region, 
                                p_value_plausibility_target = p_values_per_region[2], 
                                p_value_surprisal_distractor = p_values_per_region[3])
  p_values <- rbind(p_values, new_row_p_value) # add to p-values table
  
  # extract intercept and coefficients (target plausibility and distractor surprisal) added to intercept
  coefficients_per_region <- summary_per_region$coefficients
  intercept <- coefficients_per_region["(Intercept)", 1]
  plaus_target_coeff <- coefficients_per_region["(Intercept)", 1] + coefficients_per_region["inverted_scaled_Plaus_per_region", 1]
  surprisal_distractor_coeff <- coefficients_per_region["(Intercept)", 1] + coefficients_per_region["scaled_Surprisaldist_per_region", 1]
  
  # add error bars for intercept and coefficients (target plausibility and distractor surprisal) 
  new_row_coefficient <- data.frame(Region = region, 
                                    Estimate = "Intercept", 
                                    Estimate_value = intercept, 
                                    Estimate_error = coefficients_per_region["(Intercept)", 2],
                                    Z_value = 0)
  SPR_coefficients <- rbind(SPR_coefficients, new_row_coefficient) # add to coefficients table
  
  new_row_coefficient <- data.frame(Region = region,
                                    Estimate = "Plausability",
                                    Estimate_value = plaus_target_coeff,
                                    Estimate_error = coefficients_per_region["inverted_scaled_Plaus_per_region", 2],
                                    Z_value = coefficients_per_region["inverted_scaled_Plaus_per_region", 1] / coefficients_per_region["inverted_scaled_Plaus_per_region", 2])
  SPR_coefficients <- rbind(SPR_coefficients, new_row_coefficient) # add to coefficients table
  
  new_row_coefficient <- data.frame(Region = region,
                                    Estimate = "Surprisal", 
                                    Estimate_value = surprisal_distractor_coeff, 
                                    Estimate_error = coefficients_per_region["scaled_Surprisaldist_per_region", 2],
                                    Z_value = coefficients_per_region["scaled_Surprisaldist_per_region", 1] / coefficients_per_region["scaled_Surprisaldist_per_region", 2])
  SPR_coefficients <- rbind(SPR_coefficients, new_row_coefficient) # add to coefficients table
  
  
  
  ### loop through each of the 3 conditions ###
  for (condition in conditions)
  {
    # extract subset for each condition
    region_per_condition <- subset(region_subset, Condition == condition)
    # predict logRTs for each condition separately, within each of the 4 regions
    region_per_condition$region_per_condition_Predicted <- predict(model_per_region, newdata = region_per_condition,  type = "response")
    
    # print observed mean logRTs
    region_per_condition_logRT_observed <- mean(region_per_condition$logRT_per_region)
    
    # print estimated mean logRTs
    region_per_condition_logRT_estimated <- mean(region_per_condition$region_per_condition_Predicted, na.rm = TRUE)
    
    # calculate residuals (observed logRTs - estimated logRTs)
    Residual_region_per_condition <- mean(region_per_condition$logRT_per_region) - mean(region_per_condition$region_per_condition_Predicted, na.rm = TRUE)
    
    # calculate standard error for residuals
    SE_residuals_region_per_condition <- sqrt(sd(region_per_condition$logRT, na.rm = TRUE)^2/length(region_per_condition$logRT) + sd(region_per_condition$region_per_condition_Predicted, na.rm = TRUE)^2/length(region_per_condition$region_per_condition_Predicted))
    
    # add to residuals table
    new_row_residuals <- data.frame(Region = region, Condition = condition, Residual = Residual_region_per_condition, SE_Residual = SE_residuals_region_per_condition)
    residuals <- rbind(residuals, new_row_residuals)
    
    # calculate standard error for estimated logRTs
    SE_estimated_region_per_condition <- sd(region_per_condition$region_per_condition_Predicted, na.rm = TRUE) / sqrt(length(region_per_condition$region_per_condition_Predicted)) 
    
    # add to estimated logRTs table
    new_row_logRT_estimated <- data.frame(Region = region, Condition = condition, Estimated_logRT = region_per_condition_logRT_estimated, SE_Estimated = SE_estimated_region_per_condition)
    logRT_estimated <- rbind(logRT_estimated, new_row_logRT_estimated)
  }
}

# plot effect sizes (z-vaues)
Effect_sizes <- subset(SPR_coefficients, Estimate != 'Intercept')

p2 <- ggplot(Effect_sizes, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                               y = Z_value, color = Estimate, group = Estimate)) + geom_point(shape = 4, size = 3.5, stroke = 0.8) + geom_line(linewidth=0.5) + ylim (-5.2, 5)
p2 <- p2 + geom_hline(yintercept=0, linetype=2)
p2 <- p2 + theme_minimal()
p2 <- p2 + scale_color_manual(name="Coefficients", labels=c( "Target Plausibility", "Distractor Surprisal"), values=c("#FF00FF", "#00FFFF"))
p2 <- p2 + labs(x="Region", y="Z-values", title = "Single Online Plausibility + LeoLM Surprisal") 
p2 <- p2 + theme(legend.position="none", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 7)) + theme(plot.title = element_text(size=8)) + theme(plot.margin = margin(t = 0.5, r = 0.2, b = 0, l = 0.2, unit = "cm"))
p2 <- p2 + geom_point(data = Effect_sizes, aes(3, -4.6, group = Estimate), size = 2, shape = 19, color = "#FF00FF")
p2 <- p2 + geom_point(data = Effect_sizes, aes(2, -5.2, group = Estimate), size = 2, shape = 19, color = "#00FFFF")
p2 <- p2 + geom_point(data = Effect_sizes, aes(3, -5.2, group = Estimate), size = 2, shape = 19, color = "#00FFFF")
p2 <- p2 + geom_point(data = Effect_sizes, aes(4, -5.2, group = Estimate), size = 2, shape = 19, color = "#00FFFF")
p2 



setwd("~/Downloads/Master_Thesis/3_SPR_Study/Results_SPR_Plaus_avg/")
GP6 <- read.csv("GP6SPR_processed.csv")

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


regions <- c("Pre-critical", "Critical", "Spillover", "Post-spillover") 
conditions <- c("A", "B", "C") 

for (region in regions) 
{
  # Precritical region
  region_subset <- subset(GP6, Region == region)
  
  # standardise predictors
  region_subset$scaled_Plaus_per_region <- scale(region_subset$SPR_Plaus_avg)
  region_subset$scaled_Surprisaldist_per_region <- scale(region_subset$Surprisal_distractor)
  # invert predictor plausibility
  region_subset$inverted_scaled_Plaus_per_region <- (region_subset$scaled_Plaus_per_region) * (-1)
  #log transform reading times
  region_subset$logRT_per_region <- log(region_subset$ReadingTime)
  
  
  # define and run the linear mixed-effects regression model for the precritical region 
  model_per_region <- lmerTest::lmer(logRT_per_region ~ inverted_scaled_Plaus_per_region + scaled_Surprisaldist_per_region + 
                                       (1 + inverted_scaled_Plaus_per_region + scaled_Surprisaldist_per_region | Subject) + 
                                       (1 + inverted_scaled_Plaus_per_region + scaled_Surprisaldist_per_region | Item), data = region_subset)
  
  # print the summary of the model
  summary_per_region <- summary(model_per_region)
  print(summary_per_region)
  
  # calculate p-values
  p_values_per_region <- summary_per_region$coefficients[, "Pr(>|t|)"]
  new_row_p_value <- data.frame(Region = region, 
                                p_value_plausibility_target = p_values_per_region[2], 
                                p_value_surprisal_distractor = p_values_per_region[3])
  p_values <- rbind(p_values, new_row_p_value)
  
  # extract intercept and coefficients added to intercept
  coefficients_per_region <- summary_per_region$coefficients
  intercept <- coefficients_per_region["(Intercept)", 1]
  plaus_target_coeff <- coefficients_per_region["(Intercept)", 1] + coefficients_per_region["inverted_scaled_Plaus_per_region", 1]
  surprisal_distractor_coeff <- coefficients_per_region["(Intercept)", 1] + coefficients_per_region["scaled_Surprisaldist_per_region", 1]
  
  # add error bars for coefficients and intercept
  new_row_coefficient <- data.frame(Region = region, 
                                    Estimate = "Intercept", 
                                    Estimate_value = intercept, 
                                    Estimate_error = coefficients_per_region["(Intercept)", 2],
                                    Z_value = 0)
  SPR_coefficients <- rbind(SPR_coefficients, new_row_coefficient)
  
  new_row_coefficient <- data.frame(Region = region,
                                    Estimate = "Plausability",
                                    Estimate_value = plaus_target_coeff,
                                    Estimate_error = coefficients_per_region["inverted_scaled_Plaus_per_region", 2],
                                    Z_value = coefficients_per_region["inverted_scaled_Plaus_per_region", 1] / coefficients_per_region["inverted_scaled_Plaus_per_region", 2])
  SPR_coefficients <- rbind(SPR_coefficients, new_row_coefficient)
  
  new_row_coefficient <- data.frame(Region = region,
                                    Estimate = "Surprisal", 
                                    Estimate_value = surprisal_distractor_coeff, 
                                    Estimate_error = coefficients_per_region["scaled_Surprisaldist_per_region", 2],
                                    Z_value = coefficients_per_region["scaled_Surprisaldist_per_region", 1] / coefficients_per_region["scaled_Surprisaldist_per_region", 2])
  SPR_coefficients <- rbind(SPR_coefficients, new_row_coefficient)
  
  for (condition in conditions)
  {
    #####predict condition A, precritical#####
    region_per_condition <- subset(region_subset, Condition == condition)
    region_per_condition$region_per_condition_Predicted <- predict(model_per_region, newdata = region_per_condition,  type = "response")
    
    # calculate residuals
    Residual_region_per_condition <- mean(region_per_condition$logRT_per_region) - mean(region_per_condition$region_per_condition_Predicted)
    Residual_region_per_condition
    # observed RT for condition A precritical
    region_per_condition_logRT_observed <- mean(region_per_condition$logRT_per_region)
    # if (condition == "C") 
    #  {
    #  print(region_per_condition_logRT_observed)
    # }
    # estimated RT for condition A precritical
    region_per_condition_logRT_estimated <- mean(region_per_condition$region_per_condition_Predicted)
    region_per_condition_logRT_estimated
    
    # calculate standard error for residuals
    SE_residuals_region_per_condition <- sqrt(sd(region_per_condition$logRT_per_region, na.rm = TRUE)^2/length(region_per_condition$logRT_per_region) + sd(region_per_condition$region_per_condition_Predicted, na.rm = TRUE)^2/length(region_per_condition$region_per_condition_Predicted))
    
    new_row_residuals <- data.frame(Region = region, Condition = condition, Residual = Residual_region_per_condition, SE_Residual = SE_residuals_region_per_condition)
    residuals <- rbind(residuals, new_row_residuals)
    
    # calculate standard error for logRT estimated
    ##
    SE_estimated_region_per_condition <- sd(region_per_condition$region_per_condition_Predicted, na.rm = TRUE) / sqrt(length(region_per_condition$region_per_condition_Predicted)) 
    
    new_row_logRT_estimated <- data.frame(Region = region, Condition = condition, Estimated_logRT = region_per_condition_logRT_estimated, SE_Estimated = SE_estimated_region_per_condition)
    logRT_estimated <- rbind(logRT_estimated, new_row_logRT_estimated)
  }
}

# plot effect sizes (z-vaues)
Effect_sizes <- subset(SPR_coefficients, Estimate != 'Intercept')

p3 <- ggplot(Effect_sizes, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                              y = Z_value, color = Estimate, group = Estimate)) + geom_point(shape = 4, size = 3.5, stroke = 0.8) + geom_line(linewidth=0.5) + ylim (-5.2, 5)
p3 <- p3 + geom_hline(yintercept=0, linetype=2)
p3 <- p3 + theme_minimal()
p3 <- p3 + scale_color_manual(name="Coefficients", labels=c( "Target Plausibility", "Distractor Surprisal"), values=c("#FF00FF", "#00FFFF"))
p3 <- p3 + labs(x="Region", y="Z-values", title = "Average Online Plausibility + GPT-2 Surprisal") 
p3 <- p3 + theme(legend.position="none", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 7)) + theme(plot.title = element_text(size=8)) + theme(plot.margin = margin(t = 0.5, r = 0.2, b = 0, l = 0.2, unit = "cm"))
p3 <- p3 + geom_point(data = Effect_sizes, aes(2, -5.2, group = Estimate), size = 2, shape = 19, color = "#FF00FF")
p3 <- p3 + geom_point(data = Effect_sizes, aes(3, -5.2, group = Estimate), size = 2, shape = 19, color = "#FF00FF")
p3 <- p3 + geom_point(data = Effect_sizes, aes(4, -5.2, group = Estimate), size = 2, shape = 19, color = "#FF00FF")
p3 


setwd("~/Downloads/Master_Thesis/3_SPR_Study/Results_SPR_Plaus_Leo_LM/")

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
  region_subset$scaled_Plaus_per_region <- scale(region_subset$SPR_Plaus_avg)
  region_subset$scaled_Surprisaldist_per_region <- scale(region_subset$LeoLM_dist)
  
  # invert predictor target plausibility
  region_subset$inverted_scaled_Plaus_per_region <- (region_subset$scaled_Plaus_per_region) * (-1)
  
  #log transform reading times
  region_subset$logRT_per_region <- log(region_subset$ReadingTime)
  
  
  # fit linear mixed-effects regression model for each region
  model_per_region <- lmerTest::lmer(logRT_per_region ~ inverted_scaled_Plaus_per_region + scaled_Surprisaldist_per_region + 
                                       (1 + inverted_scaled_Plaus_per_region + scaled_Surprisaldist_per_region | Subject) + 
                                       (1 + inverted_scaled_Plaus_per_region + scaled_Surprisaldist_per_region | Item), data = region_subset)
  
  # print summary of the model
  summary_per_region <- summary(model_per_region)
  print(summary_per_region)
  
  # calculate p-values
  p_values_per_region <- summary_per_region$coefficients[, "Pr(>|t|)"]
  new_row_p_value <- data.frame(Region = region, 
                                p_value_plausibility_target = p_values_per_region[2], 
                                p_value_surprisal_distractor = p_values_per_region[3])
  p_values <- rbind(p_values, new_row_p_value) # add to p-values table
  
  # extract intercept and coefficients (target plausibility and distractor surprisal) added to intercept
  coefficients_per_region <- summary_per_region$coefficients
  intercept <- coefficients_per_region["(Intercept)", 1]
  plaus_target_coeff <- coefficients_per_region["(Intercept)", 1] + coefficients_per_region["inverted_scaled_Plaus_per_region", 1]
  surprisal_distractor_coeff <- coefficients_per_region["(Intercept)", 1] + coefficients_per_region["scaled_Surprisaldist_per_region", 1]
  
  # add error bars for intercept and coefficients (target plausibility and distractor surprisal) 
  new_row_coefficient <- data.frame(Region = region, 
                                    Estimate = "Intercept", 
                                    Estimate_value = intercept, 
                                    Estimate_error = coefficients_per_region["(Intercept)", 2],
                                    Z_value = 0)
  SPR_coefficients <- rbind(SPR_coefficients, new_row_coefficient) # add to coefficients table
  
  new_row_coefficient <- data.frame(Region = region,
                                    Estimate = "Plausability",
                                    Estimate_value = plaus_target_coeff,
                                    Estimate_error = coefficients_per_region["inverted_scaled_Plaus_per_region", 2],
                                    Z_value = coefficients_per_region["inverted_scaled_Plaus_per_region", 1] / coefficients_per_region["inverted_scaled_Plaus_per_region", 2])
  SPR_coefficients <- rbind(SPR_coefficients, new_row_coefficient) # add to coefficients table
  
  new_row_coefficient <- data.frame(Region = region,
                                    Estimate = "Surprisal", 
                                    Estimate_value = surprisal_distractor_coeff, 
                                    Estimate_error = coefficients_per_region["scaled_Surprisaldist_per_region", 2],
                                    Z_value = coefficients_per_region["scaled_Surprisaldist_per_region", 1] / coefficients_per_region["scaled_Surprisaldist_per_region", 2])
  SPR_coefficients <- rbind(SPR_coefficients, new_row_coefficient) # add to coefficients table
  
  
  
  ### loop through each of the 3 conditions ###
  for (condition in conditions)
  {
    # extract subset for each condition
    region_per_condition <- subset(region_subset, Condition == condition)
    # predict logRTs for each condition separately, within each of the 4 regions
    region_per_condition$region_per_condition_Predicted <- predict(model_per_region, newdata = region_per_condition,  type = "response")
    
    # print observed mean logRTs
    region_per_condition_logRT_observed <- mean(region_per_condition$logRT_per_region)
    
    # print estimated mean logRTs
    region_per_condition_logRT_estimated <- mean(region_per_condition$region_per_condition_Predicted, na.rm = TRUE)
    
    # calculate residuals (observed logRTs - estimated logRTs)
    Residual_region_per_condition <- mean(region_per_condition$logRT_per_region) - mean(region_per_condition$region_per_condition_Predicted, na.rm = TRUE)
    
    # calculate standard error for residuals
    SE_residuals_region_per_condition <- sqrt(sd(region_per_condition$logRT, na.rm = TRUE)^2/length(region_per_condition$logRT) + sd(region_per_condition$region_per_condition_Predicted, na.rm = TRUE)^2/length(region_per_condition$region_per_condition_Predicted))
    
    # add to residuals table
    new_row_residuals <- data.frame(Region = region, Condition = condition, Residual = Residual_region_per_condition, SE_Residual = SE_residuals_region_per_condition)
    residuals <- rbind(residuals, new_row_residuals)
    
    # calculate standard error for estimated logRTs
    SE_estimated_region_per_condition <- sd(region_per_condition$region_per_condition_Predicted, na.rm = TRUE) / sqrt(length(region_per_condition$region_per_condition_Predicted)) 
    
    # add to estimated logRTs table
    new_row_logRT_estimated <- data.frame(Region = region, Condition = condition, Estimated_logRT = region_per_condition_logRT_estimated, SE_Estimated = SE_estimated_region_per_condition)
    logRT_estimated <- rbind(logRT_estimated, new_row_logRT_estimated)
  }
}

# plot effect sizes (z-vaues)
Effect_sizes <- subset(SPR_coefficients, Estimate != 'Intercept')

p4 <- ggplot(Effect_sizes, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                              y = Z_value, color = Estimate, group = Estimate)) + geom_point(shape = 4, size = 3.5, stroke = 0.8) + geom_line(linewidth=0.5) + ylim (-5.2, 5)
p4 <- p4 + geom_hline(yintercept=0, linetype=2)
p4 <- p4 + theme_minimal()
p4 <- p4 + scale_color_manual(name="Coefficients", labels=c( "Target Plausibility", "Distractor Surprisal"), values=c("#FF00FF", "#00FFFF"))
p4 <- p4 + labs(x="Region", y="Z-values", title = "Average Online Plausibility + LeoLM Surprisal") 
p4 <- p4 + theme(legend.position="none", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 7)) + theme(plot.title = element_text(size=8)) + theme(plot.margin = margin(t = 0.5, r = 0.2, b = 0, l = 0.2, unit = "cm"))
p4 <- p4 + geom_point(data = Effect_sizes, aes(2, -5.2, group = Estimate), size = 2, shape = 19, color = "#00FFFF")
p4 <- p4 + geom_point(data = Effect_sizes, aes(3, -5.2, group = Estimate), size = 2, shape = 19, color = "#00FFFF")
p4 <- p4 + geom_point(data = Effect_sizes, aes(4, -5.2, group = Estimate), size = 2, shape = 19, color = "#00FFFF")
p4 <- p4 + geom_point(data = Effect_sizes, aes(3, -4.6, group = Estimate), size = 2, shape = 19, color = "#FF00FF")
p4 <- p4 + geom_point(data = Effect_sizes, aes(4, -4.6, group = Estimate), size = 2, shape = 19, color = "#FF00FF")
p4


setwd("~/Downloads/Master_Thesis/3_SPR_Study/Results_Plaus_Plaus_avg/")
GP6 <- read.csv("GP6SPR_processed.csv")

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


regions <- c("Pre-critical", "Critical", "Spillover", "Post-spillover") 
conditions <- c("A", "B", "C") 

for (region in regions) 
{
  # Precritical region
  region_subset <- subset(GP6, Region == region)
  
  # standardise predictors (use average plausibility ratings per item and condition from plausibility norming study here)
  region_subset$scaled_Plaus_per_region <- scale(region_subset$Plaus_target_avg)
  region_subset$scaled_Surprisaldist_per_region <- scale(region_subset$Surprisal_distractor)
  # invert predictor plausibility
  region_subset$inverted_scaled_Plaus_per_region <- (region_subset$scaled_Plaus_per_region) * (-1)
  #log transform reading times
  region_subset$logRT_per_region <- log(region_subset$ReadingTime)
  
  
  # define and run the linear mixed-effects regression model for the precritical region 
  model_per_region <- lmerTest::lmer(logRT_per_region ~ inverted_scaled_Plaus_per_region + scaled_Surprisaldist_per_region + 
                                       (1 + inverted_scaled_Plaus_per_region + scaled_Surprisaldist_per_region | Subject) + 
                                       (1 + inverted_scaled_Plaus_per_region + scaled_Surprisaldist_per_region | Item), data = region_subset)
  
  # print the summary of the model
  summary_per_region <- summary(model_per_region)
  print(summary_per_region)
  
  # calculate p-values
  p_values_per_region <- summary_per_region$coefficients[, "Pr(>|t|)"]
  new_row_p_value <- data.frame(Region = region, 
                                p_value_plausibility_target = p_values_per_region[2], 
                                p_value_surprisal_distractor = p_values_per_region[3])
  p_values <- rbind(p_values, new_row_p_value)
  
  # extract intercept and coefficients added to intercept
  coefficients_per_region <- summary_per_region$coefficients
  intercept <- coefficients_per_region["(Intercept)", 1]
  plaus_target_coeff <- coefficients_per_region["(Intercept)", 1] + coefficients_per_region["inverted_scaled_Plaus_per_region", 1]
  surprisal_distractor_coeff <- coefficients_per_region["(Intercept)", 1] + coefficients_per_region["scaled_Surprisaldist_per_region", 1]
  
  # add error bars for coefficients and intercept
  new_row_coefficient <- data.frame(Region = region, 
                                    Estimate = "Intercept", 
                                    Estimate_value = intercept, 
                                    Estimate_error = coefficients_per_region["(Intercept)", 2],
                                    Z_value = 0)
  SPR_coefficients <- rbind(SPR_coefficients, new_row_coefficient)
  
  new_row_coefficient <- data.frame(Region = region,
                                    Estimate = "Plausability",
                                    Estimate_value = plaus_target_coeff,
                                    Estimate_error = coefficients_per_region["inverted_scaled_Plaus_per_region", 2],
                                    Z_value = coefficients_per_region["inverted_scaled_Plaus_per_region", 1] / coefficients_per_region["inverted_scaled_Plaus_per_region", 2])
  SPR_coefficients <- rbind(SPR_coefficients, new_row_coefficient)
  
  new_row_coefficient <- data.frame(Region = region,
                                    Estimate = "Surprisal", 
                                    Estimate_value = surprisal_distractor_coeff, 
                                    Estimate_error = coefficients_per_region["scaled_Surprisaldist_per_region", 2],
                                    Z_value = coefficients_per_region["scaled_Surprisaldist_per_region", 1] / coefficients_per_region["scaled_Surprisaldist_per_region", 2])
  SPR_coefficients <- rbind(SPR_coefficients, new_row_coefficient)
  
  for (condition in conditions)
  {
    #####predict condition A, precritical#####
    region_per_condition <- subset(region_subset, Condition == condition)
    region_per_condition$region_per_condition_Predicted <- predict(model_per_region, newdata = region_per_condition,  type = "response")
    
    # calculate residuals
    Residual_region_per_condition <- mean(region_per_condition$logRT_per_region) - mean(region_per_condition$region_per_condition_Predicted)
    Residual_region_per_condition
    # observed RT for condition A precritical
    region_per_condition_logRT_observed <- mean(region_per_condition$logRT_per_region)
    # if (condition == "C") 
    #  {
    #  print(region_per_condition_logRT_observed)
    # }
    # estimated RT for condition A precritical
    region_per_condition_logRT_estimated <- mean(region_per_condition$region_per_condition_Predicted)
    region_per_condition_logRT_estimated
    
    # calculate standard error for residuals
    SE_residuals_region_per_condition <- sqrt(sd(region_per_condition$logRT_per_region, na.rm = TRUE)^2/length(region_per_condition$logRT_per_region) + sd(region_per_condition$region_per_condition_Predicted, na.rm = TRUE)^2/length(region_per_condition$region_per_condition_Predicted))
    
    new_row_residuals <- data.frame(Region = region, Condition = condition, Residual = Residual_region_per_condition, SE_Residual = SE_residuals_region_per_condition)
    residuals <- rbind(residuals, new_row_residuals)
    
    # calculate standard error for logRT estimated
    ##
    SE_estimated_region_per_condition <- sd(region_per_condition$region_per_condition_Predicted, na.rm = TRUE) / sqrt(length(region_per_condition$region_per_condition_Predicted)) 
    
    new_row_logRT_estimated <- data.frame(Region = region, Condition = condition, Estimated_logRT = region_per_condition_logRT_estimated, SE_Estimated = SE_estimated_region_per_condition)
    logRT_estimated <- rbind(logRT_estimated, new_row_logRT_estimated)
  }
}

# plot effect sizes (z-vaues)
Effect_sizes <- subset(SPR_coefficients, Estimate != 'Intercept')

p5 <- ggplot(Effect_sizes, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                               y = Z_value, color = Estimate, group = Estimate)) + geom_point(shape = 4, size = 3.5, stroke = 0.8) + geom_line(linewidth=0.5) + ylim (-5.2, 5)
p5 <- p5 + geom_hline(yintercept=0, linetype=2)
p5 <- p5 + theme_minimal()
p5 <- p5 + scale_color_manual(name="Coefficients", labels=c("Target Plausibility", "Distractor Surprisal"), values=c("#FF00FF", "#00FFFF"))
p5 <- p5 + labs(x="Region", y="Z-values", title = "Average Offline Plausibility + GPT-2 Surprisal") 
p5 <- p5 + theme(legend.position="none", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 7)) + theme(plot.title = element_text(size=8)) + theme(plot.margin = margin(t = 0.5, r = 0.2, b = 0, l = 0.2, unit = "cm"))
p5 <- p5 + geom_point(data = Effect_sizes, aes(2, -5.2, group = Estimate), size = 2, shape = 19, color = "#FF00FF")
p5 <- p5 + geom_point(data = Effect_sizes, aes(3, -5.2, group = Estimate), size = 2, shape = 19, color = "#FF00FF")
p5 <- p5 + geom_point(data = Effect_sizes, aes(4, -5.2, group = Estimate), size = 2, shape = 19, color = "#FF00FF")
p5 
p5 



setwd("~/Downloads/Master_Thesis/3_SPR_Study/Results_SPR_Plaus_Leo_LM/")

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
  region_subset$scaled_Surprisaldist_per_region <- scale(region_subset$LeoLM_dist)
  
  # invert predictor target plausibility
  region_subset$inverted_scaled_Plaus_per_region <- (region_subset$scaled_Plaus_per_region) * (-1)
  
  #log transform reading times
  region_subset$logRT_per_region <- log(region_subset$ReadingTime)
  
  
  # fit linear mixed-effects regression model for each region
  model_per_region <- lmerTest::lmer(logRT_per_region ~ inverted_scaled_Plaus_per_region + scaled_Surprisaldist_per_region + 
                                       (1 + inverted_scaled_Plaus_per_region + scaled_Surprisaldist_per_region | Subject) + 
                                       (1 + inverted_scaled_Plaus_per_region + scaled_Surprisaldist_per_region | Item), data = region_subset)
  
  # print summary of the model
  summary_per_region <- summary(model_per_region)
  print(summary_per_region)
  
  # calculate p-values
  p_values_per_region <- summary_per_region$coefficients[, "Pr(>|t|)"]
  new_row_p_value <- data.frame(Region = region, 
                                p_value_plausibility_target = p_values_per_region[2], 
                                p_value_surprisal_distractor = p_values_per_region[3])
  p_values <- rbind(p_values, new_row_p_value) # add to p-values table
  
  # extract intercept and coefficients (target plausibility and distractor surprisal) added to intercept
  coefficients_per_region <- summary_per_region$coefficients
  intercept <- coefficients_per_region["(Intercept)", 1]
  plaus_target_coeff <- coefficients_per_region["(Intercept)", 1] + coefficients_per_region["inverted_scaled_Plaus_per_region", 1]
  surprisal_distractor_coeff <- coefficients_per_region["(Intercept)", 1] + coefficients_per_region["scaled_Surprisaldist_per_region", 1]
  
  # add error bars for intercept and coefficients (target plausibility and distractor surprisal) 
  new_row_coefficient <- data.frame(Region = region, 
                                    Estimate = "Intercept", 
                                    Estimate_value = intercept, 
                                    Estimate_error = coefficients_per_region["(Intercept)", 2],
                                    Z_value = 0)
  SPR_coefficients <- rbind(SPR_coefficients, new_row_coefficient) # add to coefficients table
  
  new_row_coefficient <- data.frame(Region = region,
                                    Estimate = "Plausability",
                                    Estimate_value = plaus_target_coeff,
                                    Estimate_error = coefficients_per_region["inverted_scaled_Plaus_per_region", 2],
                                    Z_value = coefficients_per_region["inverted_scaled_Plaus_per_region", 1] / coefficients_per_region["inverted_scaled_Plaus_per_region", 2])
  SPR_coefficients <- rbind(SPR_coefficients, new_row_coefficient) # add to coefficients table
  
  new_row_coefficient <- data.frame(Region = region,
                                    Estimate = "Surprisal", 
                                    Estimate_value = surprisal_distractor_coeff, 
                                    Estimate_error = coefficients_per_region["scaled_Surprisaldist_per_region", 2],
                                    Z_value = coefficients_per_region["scaled_Surprisaldist_per_region", 1] / coefficients_per_region["scaled_Surprisaldist_per_region", 2])
  SPR_coefficients <- rbind(SPR_coefficients, new_row_coefficient) # add to coefficients table
  
  
  
  ### loop through each of the 3 conditions ###
  for (condition in conditions)
  {
    # extract subset for each condition
    region_per_condition <- subset(region_subset, Condition == condition)
    # predict logRTs for each condition separately, within each of the 4 regions
    region_per_condition$region_per_condition_Predicted <- predict(model_per_region, newdata = region_per_condition,  type = "response")
    
    # print observed mean logRTs
    region_per_condition_logRT_observed <- mean(region_per_condition$logRT_per_region)
    
    # print estimated mean logRTs
    region_per_condition_logRT_estimated <- mean(region_per_condition$region_per_condition_Predicted, na.rm = TRUE)
    
    # calculate residuals (observed logRTs - estimated logRTs)
    Residual_region_per_condition <- mean(region_per_condition$logRT_per_region) - mean(region_per_condition$region_per_condition_Predicted, na.rm = TRUE)
    
    # calculate standard error for residuals
    SE_residuals_region_per_condition <- sqrt(sd(region_per_condition$logRT, na.rm = TRUE)^2/length(region_per_condition$logRT) + sd(region_per_condition$region_per_condition_Predicted, na.rm = TRUE)^2/length(region_per_condition$region_per_condition_Predicted))
    
    # add to residuals table
    new_row_residuals <- data.frame(Region = region, Condition = condition, Residual = Residual_region_per_condition, SE_Residual = SE_residuals_region_per_condition)
    residuals <- rbind(residuals, new_row_residuals)
    
    # calculate standard error for estimated logRTs
    SE_estimated_region_per_condition <- sd(region_per_condition$region_per_condition_Predicted, na.rm = TRUE) / sqrt(length(region_per_condition$region_per_condition_Predicted)) 
    
    # add to estimated logRTs table
    new_row_logRT_estimated <- data.frame(Region = region, Condition = condition, Estimated_logRT = region_per_condition_logRT_estimated, SE_Estimated = SE_estimated_region_per_condition)
    logRT_estimated <- rbind(logRT_estimated, new_row_logRT_estimated)
  }
}

# plot effect sizes (z-vaues)
Effect_sizes <- subset(SPR_coefficients, Estimate != 'Intercept')

p6 <- ggplot(Effect_sizes, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                               y = Z_value, color = Estimate, group = Estimate)) + geom_point(shape = 4, size = 3.5, stroke = 0.8) + geom_line(linewidth=0.5) + ylim (-5.2, 5)
p6 <- p6 + geom_hline(yintercept=0, linetype=2)
p6 <- p6 + theme_minimal()
p6 <- p6 + scale_color_manual(name="Z-values", labels=c("Target Plausibility", "Distractor Surprisal"), values=c("#FF00FF", "#00FFFF"))
p6 <- p6 + labs(x="Region", y="Z-values", title = "Average Offline Plausibility + LeoLM Surprisal") 
p6 <- p6 + theme(legend.position="none", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 7)) + theme(plot.title = element_text(size=8)) + theme(plot.margin = margin(t = 0.5, r = 0.2, b = 0, l = 0.2, unit = "cm"))
p6 <- p6 + geom_point(data = Effect_sizes, aes(2, -4.6, group = Estimate), size = 2, shape = 19, color = "#FF00FF")
p6 <- p6 + geom_point(data = Effect_sizes, aes(3, -4.6, group = Estimate), size = 2, shape = 19, color = "#FF00FF")
p6 <- p6 + geom_point(data = Effect_sizes, aes(4, -4.6, group = Estimate), size = 2, shape = 19, color = "#FF00FF")
p6 <- p6 + geom_point(data = Effect_sizes, aes(3, -5.2, group = Estimate), size = 2, shape = 19, color = "#00FFFF")
p6 


# Create a line plot for estimated logRTs
Effect_sizes <- subset(SPR_coefficients, Estimate != 'Intercept')

p_legend <- ggplot(Effect_sizes, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                                     y = Z_value, color = Estimate, group = Estimate)) + geom_point(shape = 4, size = 3.5, stroke = 0.4) + geom_line(linewidth=0.5) + ylim (-5, 5)
p_legend <- p_legend + geom_hline(yintercept=0, linetype=2)
p_legend <- p_legend + theme_minimal()
p_legend <- p_legend + scale_color_manual(name="Z-values", labels=c("Target Plausibility", "Distractor Surprisal"), values=c("#FF00FF", "#00FFFF"))
p_legend <- p_legend + labs(x="Region", y="Z-values", title = "Inferential Statistics") 
p_legend <- p_legend + theme(legend.position="bottom", legend.text=element_text(size=10), legend.title=element_text(size=10), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) 
p_legend
#ggsave("Estimated_RTs_Plot.pdf", p, width=4, height=4)

# Create a line plot for estimated logRTs
Effect_sizes <- subset(SPR_coefficients, Estimate != 'Intercept')

p_legend2 <- ggplot(Effect_sizes, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                                     y = Z_value, color = Estimate, group = Estimate)) + geom_point(shape = 4, size = 3.5, stroke = 0.4) 
p_legend2 <- p_legend2 + geom_hline(yintercept=0, linetype=2)
p_legend2 <- p_legend2 + theme_minimal()
p_legend2 <- p_legend2 + scale_color_manual(name="Coefficients", labels=c("Target Plausibility", "Distractor Surprisal"), values=c("#FF00FF", "#00FFFF"))
p_legend2 <- p_legend2 + labs(x="Region", y="Z-values", title = "Inferential Statistics") 
p_legend2 <- p_legend2 + theme(legend.position="bottom", legend.text=element_text(size=10), legend.title=element_text(size=10), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) 
p_legend2 <- p_legend2 + scale_color_manual(name="P-values", 
                                            labels=c("Significant", "Not significant"), 
                                            values=c("black", "white")) +
  guides(color = guide_legend(override.aes = list(shape = c(20, 20))))
p_legend2 


# plot1 with legend
combined_plot <- grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)
caption <- "Inferential Statistics"

# function to extract legend from plot 
get_only_legend <- function(plot) { 
  plot_table <- ggplot_gtable(ggplot_build(plot)) 
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box") 
  legend <- plot_table$grobs[[legend_plot]] 
  return(legend) 
} 

empty_row <- grid.rect(gp = gpar(fill = "white", col = "white"))

# extract legend from plot1 using above function 
legend <- get_only_legend(p_legend) 
combined_plot_with_legend <- grid.arrange(empty_row, combined_plot, legend, legend2, nrow = 4, heights = c(0.2, 9,0.3, 0.3))
combined_plot_with_legend <- grid.text(caption, x = 0.211, y = 0.98, just = "center", gp = gpar(fontsize = 15))
combined_plot_with_legend

setwd("~/Downloads/Master_Thesis/Plots_SPR1/")
# Save the combined plot
ggsave("Combined_Plot_InferentialStastics.pdf", combined_plot_with_legend, device = "pdf")


