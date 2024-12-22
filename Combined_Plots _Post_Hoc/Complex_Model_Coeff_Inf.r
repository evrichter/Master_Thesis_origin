### LMER ###
# load lme4 package
library(lme4)
library(lmerTest)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

setwd("~/Downloads/Master_Thesis/3_SPR_Study/Results_Plaus_avg_and_SPR_single/")
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
  p_value_plausibility_target_plaus = numeric(0),
  p_value_plausibility_target_spr = numeric(0),
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
  region_subset$scaled_Plaus_per_region_plaus <- scale(region_subset$Plaus_target_avg) #avg plaus ratings from pretest
  region_subset$scaled_Plaus_per_region_spr <- scale(region_subset$SPR_Plaus_Rating) #single plaus ratings from SPR study
  region_subset$scaled_Surprisaldist_per_region <- scale(region_subset$Surprisal_distractor) #surprisal predictor
  # invert predictors plausibility
  region_subset$inverted_scaled_Plaus_per_region_plaus <- (region_subset$scaled_Plaus_per_region_plaus) * (-1)
  region_subset$inverted_scaled_Plaus_per_region_spr <- (region_subset$scaled_Plaus_per_region_spr) * (-1)
  #log transform reading times
  region_subset$logRT_per_region <- log(region_subset$ReadingTime)
  
  
  # define and run the linear mixed-effects regression model
  model_per_region <- lmer(logRT_per_region ~ inverted_scaled_Plaus_per_region_plaus + inverted_scaled_Plaus_per_region_spr + scaled_Surprisaldist_per_region + 
                             (1 + inverted_scaled_Plaus_per_region_plaus + inverted_scaled_Plaus_per_region_spr + scaled_Surprisaldist_per_region | Subject) + 
                             (1 + inverted_scaled_Plaus_per_region_plaus + inverted_scaled_Plaus_per_region_spr + scaled_Surprisaldist_per_region | Item), REML = FALSE, data = region_subset)
  # use REML = FALSE when comparing a simple to a complex (nested) model
  # print the summary of the model
  summary_per_region <- summary(model_per_region)
  print(summary_per_region)
  
  # calculate p-values
  # p_values_per_region <- summary_per_region$coefficients[, "Pr(>|t|)"]
  #new_row_p_value <- data.frame(Region = region, 
  #                             p_value_plausibility_target_plaus = p_values_per_region[2], 
  #                            p_value_plausibility_target_spr = p_values_per_region[3],
  #                           p_value_surprisal_distractor = p_values_per_region[4])
  #p_values <- rbind(p_values, new_row_p_value)
  
  # extract intercept and coefficients added to intercept
  coefficients_per_region <- summary_per_region$coefficients
  intercept <- coefficients_per_region["(Intercept)", 1]
  plaus_target_coeff <- coefficients_per_region["(Intercept)", 1] + coefficients_per_region["inverted_scaled_Plaus_per_region_plaus", 1]
  spr_target_coeff <- coefficients_per_region["(Intercept)", 1] + coefficients_per_region["inverted_scaled_Plaus_per_region_spr", 1]
  surprisal_distractor_coeff <- coefficients_per_region["(Intercept)", 1] + coefficients_per_region["scaled_Surprisaldist_per_region", 1]
  
  # add error bars for coefficients and intercept
  new_row_coefficient <- data.frame(Region = region, 
                                    Estimate = "Intercept", 
                                    Estimate_value = intercept, 
                                    Estimate_error = coefficients_per_region["(Intercept)", 2],
                                    Z_value = 0)
  SPR_coefficients <- rbind(SPR_coefficients, new_row_coefficient)
  
  new_row_coefficient <- data.frame(Region = region,
                                    Estimate = "Plausability_Plaus_Avg",
                                    Estimate_value = plaus_target_coeff,
                                    Estimate_error = coefficients_per_region["inverted_scaled_Plaus_per_region_plaus", 2],
                                    Z_value = coefficients_per_region["inverted_scaled_Plaus_per_region_plaus", 1] / coefficients_per_region["inverted_scaled_Plaus_per_region_plaus", 2])
  SPR_coefficients <- rbind(SPR_coefficients, new_row_coefficient)
  
  new_row_coefficient <- data.frame(Region = region,
                                    Estimate = "Surprisal", 
                                    Estimate_value = surprisal_distractor_coeff, 
                                    Estimate_error = coefficients_per_region["scaled_Surprisaldist_per_region", 2],
                                    Z_value = coefficients_per_region["scaled_Surprisaldist_per_region", 1] / coefficients_per_region["scaled_Surprisaldist_per_region", 2])
  SPR_coefficients <- rbind(SPR_coefficients, new_row_coefficient)
  
  new_row_coefficient <- data.frame(Region = region,
                                    Estimate = "Plausibility_SPR_Single", 
                                    Estimate_value = spr_target_coeff, 
                                    Estimate_error = coefficients_per_region["inverted_scaled_Plaus_per_region_spr", 2],
                                    Z_value = coefficients_per_region["inverted_scaled_Plaus_per_region_spr", 1] / coefficients_per_region["inverted_scaled_Plaus_per_region_spr", 2])
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


# plot intercept and coefficients added to intercept
p1 <- ggplot(SPR_coefficients, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                                  y = Estimate_value, color = Estimate, group = Estimate)) + geom_point(shape = 4, size = 3.5, stroke = 0.8) + geom_line(linewidth=0.5) + ylim (5.5, 5.65)
p1 <- p1 + theme_minimal() + geom_errorbar(aes(ymin=Estimate_value-Estimate_error, ymax=Estimate_value+Estimate_error), width=.1, size=0.3) 
p1 <- p1 + scale_color_manual(name="Coefficients", labels=c("Intercept", "Target Plausibility (Average)", "Target Plausibility (Single)", "Distractor Surprisal"), values=c("#000000", "#FF00FF", "#00CC33", "#00FFFF"))
p1 <- p1 + labs(x="Region", y="SPR Coefficients", title = "Coefficients (Plausibility + GPT-2 Surprisal)") 
p1 <- p1 + theme(legend.position="none", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 8)) + theme(plot.title = element_text(size=9)) + theme(plot.margin = margin(t = 0.5, r = 0.2, b = 0, l = 0.2, unit = "cm"))
p1 

# plot effect sizes (z-vaues)
Effect_sizes <- subset(SPR_coefficients, Estimate != 'Intercept')

p2 <- ggplot(Effect_sizes, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                              y = Z_value, color = Estimate, group = Estimate)) + geom_point(shape = 4, size = 3.5, stroke = 0.8) + geom_line(linewidth=0.5) + ylim (-5.2, 5)
p2 <- p2 + geom_hline(yintercept=0, linetype=2)
p2 <- p2 + theme_minimal()
p2 <- p2 + scale_color_manual(name="Coefficients", labels=c( "Plausibility (Average)", "Plausibility (Single)", "GPT-2 Surprisal"), values=c("#FF00FF", "#00CC33", "#00FFFF"))
p2 <- p2 + labs(x="Region", y="Z-values", title = "Inferential Statistics (Plausibility + GPT-2 Surprisal)") 
p2 <- p2 + theme(legend.position="none", legend.text=element_text(size=8), legend.title=element_text(size=8), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 8)) + theme(plot.title = element_text(size=9)) + theme(plot.margin = margin(t = 0.5, r = 0.2, b = 0, l = 0.2, unit = "cm"))
p2 <- p2 + geom_point(data = Effect_sizes, aes(3, -5.2, group = Estimate), size = 2, shape = 19, color = "#FF00FF")
p2 <- p2 + geom_point(data = Effect_sizes, aes(4, -5.2, group = Estimate), size = 2, shape = 19, color = "#FF00FF")
p2 <- p2 + geom_point(data = Effect_sizes, aes(2, -5.2, group = Estimate), size = 2, shape = 19, color = "#00FFFF")
p2 
##################################################################
### LMER ###
# load lme4 package
library(lme4)
library(lmerTest)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

setwd("~/Downloads/Master_Thesis/3_SPR_Study/Results_Plaus_avg_and_SPR_single/")
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
  p_value_plausibility_target_plaus = numeric(0),
  p_value_plausibility_target_spr = numeric(0),
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
  region_subset$scaled_Plaus_per_region_plaus <- scale(region_subset$Plaus_target_avg) #avg plaus ratings from pretest
  region_subset$scaled_Plaus_per_region_spr <- scale(region_subset$SPR_Plaus_Rating) #single plaus ratings from SPR study
  region_subset$scaled_Surprisaldist_per_region <- scale(region_subset$LeoLM_dist) #surprisal predictor
  # invert predictors plausibility
  region_subset$inverted_scaled_Plaus_per_region_plaus <- (region_subset$scaled_Plaus_per_region_plaus) * (-1)
  region_subset$inverted_scaled_Plaus_per_region_spr <- (region_subset$scaled_Plaus_per_region_spr) * (-1)
  #log transform reading times
  region_subset$logRT_per_region <- log(region_subset$ReadingTime)
  
  
  # define and run the linear mixed-effects regression model
  model_per_region <- lmer(logRT_per_region ~ inverted_scaled_Plaus_per_region_plaus + inverted_scaled_Plaus_per_region_spr + scaled_Surprisaldist_per_region + 
                             (1 + inverted_scaled_Plaus_per_region_plaus + inverted_scaled_Plaus_per_region_spr + scaled_Surprisaldist_per_region | Subject) + 
                             (1 + inverted_scaled_Plaus_per_region_plaus + inverted_scaled_Plaus_per_region_spr + scaled_Surprisaldist_per_region | Item), REML = FALSE, data = region_subset)
  # use REML = FALSE when comparing a simple to a complex (nested) model
  # print the summary of the model
  summary_per_region <- summary(model_per_region)
  print(summary_per_region)
  
  # calculate p-values
  # p_values_per_region <- summary_per_region$coefficients[, "Pr(>|t|)"]
  #new_row_p_value <- data.frame(Region = region, 
  #                             p_value_plausibility_target_plaus = p_values_per_region[2], 
  #                            p_value_plausibility_target_spr = p_values_per_region[3],
  #                           p_value_surprisal_distractor = p_values_per_region[4])
  #p_values <- rbind(p_values, new_row_p_value)
  
  # extract intercept and coefficients added to intercept
  coefficients_per_region <- summary_per_region$coefficients
  intercept <- coefficients_per_region["(Intercept)", 1]
  plaus_target_coeff <- coefficients_per_region["(Intercept)", 1] + coefficients_per_region["inverted_scaled_Plaus_per_region_plaus", 1]
  spr_target_coeff <- coefficients_per_region["(Intercept)", 1] + coefficients_per_region["inverted_scaled_Plaus_per_region_spr", 1]
  surprisal_distractor_coeff <- coefficients_per_region["(Intercept)", 1] + coefficients_per_region["scaled_Surprisaldist_per_region", 1]
  
  # add error bars for coefficients and intercept
  new_row_coefficient <- data.frame(Region = region, 
                                    Estimate = "Intercept", 
                                    Estimate_value = intercept, 
                                    Estimate_error = coefficients_per_region["(Intercept)", 2],
                                    Z_value = 0)
  SPR_coefficients <- rbind(SPR_coefficients, new_row_coefficient)
  
  new_row_coefficient <- data.frame(Region = region,
                                    Estimate = "Plausability_Plaus_Avg",
                                    Estimate_value = plaus_target_coeff,
                                    Estimate_error = coefficients_per_region["inverted_scaled_Plaus_per_region_plaus", 2],
                                    Z_value = coefficients_per_region["inverted_scaled_Plaus_per_region_plaus", 1] / coefficients_per_region["inverted_scaled_Plaus_per_region_plaus", 2])
  SPR_coefficients <- rbind(SPR_coefficients, new_row_coefficient)
  
  new_row_coefficient <- data.frame(Region = region,
                                    Estimate = "Surprisal", 
                                    Estimate_value = surprisal_distractor_coeff, 
                                    Estimate_error = coefficients_per_region["scaled_Surprisaldist_per_region", 2],
                                    Z_value = coefficients_per_region["scaled_Surprisaldist_per_region", 1] / coefficients_per_region["scaled_Surprisaldist_per_region", 2])
  SPR_coefficients <- rbind(SPR_coefficients, new_row_coefficient)
  
  new_row_coefficient <- data.frame(Region = region,
                                    Estimate = "Plausibility_SPR_Single", 
                                    Estimate_value = spr_target_coeff, 
                                    Estimate_error = coefficients_per_region["inverted_scaled_Plaus_per_region_spr", 2],
                                    Z_value = coefficients_per_region["inverted_scaled_Plaus_per_region_spr", 1] / coefficients_per_region["inverted_scaled_Plaus_per_region_spr", 2])
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


# plot intercept and coefficients added to intercept
p3 <- ggplot(SPR_coefficients, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                                   y = Estimate_value, color = Estimate, group = Estimate)) + geom_point(shape = 4, size = 3.5, stroke = 0.8) + geom_line(linewidth=0.5) + ylim (5.5, 5.65)
p3 <- p3 + theme_minimal() + geom_errorbar(aes(ymin=Estimate_value-Estimate_error, ymax=Estimate_value+Estimate_error), width=.1, size=0.3) 
p3 <- p3 + scale_color_manual(name="Coefficients", labels=c("Intercept", "Target Plausibility (Average)", "Target Plausibility (Single)", "Distractor Surprisal"), values=c("#000000", "#FF00FF", "#00CC33", "#00FFFF"))
p3 <- p3 + labs(x="Region", y="SPR Coefficients", title = "Coefficients (Plausibility + LeoLM Surprisal)") 
p3 <- p3 + theme(legend.position="none", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 8)) + theme(plot.title = element_text(size=9)) + theme(plot.margin = margin(t = 0.5, r = 0.2, b = 0, l = 0.2, unit = "cm"))
p3 

# plot effect sizes (z-vaues)
Effect_sizes <- subset(SPR_coefficients, Estimate != 'Intercept')

p4 <- ggplot(Effect_sizes, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                               y = Z_value, color = Estimate, group = Estimate)) + geom_point(shape = 4, size = 3.5, stroke = 0.8) + geom_line(linewidth=0.5) + ylim (-5.2, 5)
p4 <- p4 + geom_hline(yintercept=0, linetype=2)
p4 <- p4 + theme_minimal()
p4 <- p4 + scale_color_manual(name="Coefficients", labels=c( "Plausibility (Average)", "Plausibility (Single)", "GPT-2 Surprisal"), values=c("#FF00FF", "#00CC33", "#00FFFF"))
p4 <- p4 + labs(x="Region", y="Z-values", title = "Inferential Statistics (Plausibility + LeoLM Surprisal)") 
p4 <- p4 + theme(legend.position="none", legend.text=element_text(size=8), legend.title=element_text(size=8), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 8)) + theme(plot.title = element_text(size=9)) + theme(plot.margin = margin(t = 0.5, r = 0.2, b = 0, l = 0.2, unit = "cm"))
p4 <- p4 + geom_point(data = Effect_sizes, aes(3, -5.2, group = Estimate), size = 2, shape = 19, color = "#00FFFF")
p4 


#################################################################
#p_legend1 <- ggplot(residuals, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
#                            y = Residual, color = Condition, group = Condition)) + geom_point(shape = 4, size = 3.5, stroke = 0.8) + geom_line(linewidth=0.5) + ylim (0.10, -0.10)
#p_legend1 <- p_legend1 + theme_minimal() + geom_errorbar(aes(ymin=Residual-SE_Residual, ymax=Residual+SE_Residual), width=.1, size=0.3) 
#p_legend1 <- p_legend1 + scale_color_manual(name="Condition", labels=c("A: Plausible", "B: Medium Plausible", "C: Implausible"), values=c("#000000", "#FF0000", "#0000FF"))
#p_legend1 <- p_legend1 + labs(x="Region", y="logRT", title = "Residuals: Avg + Single PlausT + SurprisalD") 
#p_legend1 <- p_legend1 + theme(legend.position="bottom", legend.text=element_text(size=8), legend.title=element_text(size=8), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 8)) + theme(plot.title = element_text(size=11)) + theme(plot.margin = margin(t = 0.5, r = 0.2, b = 0, l = 0.2, unit = "cm"))
#p_legend1 


p_legend <- ggplot(SPR_coefficients, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                                   y = Estimate_value, color = Estimate, group = Estimate)) + geom_point(shape = 4, size = 3.5, stroke = 0.4) + geom_line(linewidth=0.5) + ylim (5.5, 5.7)
p_legend <- p_legend + theme_minimal() + geom_errorbar(aes(ymin=Estimate_value-Estimate_error, ymax=Estimate_value+Estimate_error), width=.1, size=0.3) 
p_legend <- p_legend + scale_color_manual(name="Coefficients", labels=c("Intercept", "Target Plausibility (Average)", "Target Plausibility (Single)", "Distractor Surprisal"), values=c("#000000", "#FF00FF", "#00CC33", "#00FFFF"))
p_legend <- p_legend + labs(x="Region", y="SPR Coefficients", title = "Coefficients") 
p_legend <- p_legend + theme(legend.position="bottom", legend.text=element_text(size=8), legend.title=element_text(size=8), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 8)) + theme(plot.title = element_text(size=11)) + theme(plot.margin = margin(t = 0.5, r = 0.2, b = 0, l = 0.2, unit = "cm"))
p_legend 


p_legend2 <- ggplot(SPR_coefficients, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                                         y = Estimate_value, color = Estimate, group = Estimate)) + geom_point(shape = 4, size = 3.5, stroke = 0.4) 
p_legend2 <- p_legend2 + theme_minimal()
p_legend2 <- p_legend2 + scale_color_manual(name="Coefficients", labels=c("Intercept", "Target Plausibility (Average)", "Target Plausibility (Single)", "Distractor Surprisal"), values=c("#000000", "#FF00FF", "#00CC33", "#00FFFF"))
p_legend2 <- p_legend2 + labs(x="Region", y="SPR Coefficients", title = "Coefficients") 
p_legend2 <- p_legend2 + theme(legend.position="bottom", legend.text=element_text(size=8), legend.title=element_text(size=8), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 8)) + theme(plot.title = element_text(size=11)) + theme(plot.margin = margin(t = 0.5, r = 0.2, b = 0, l = 0.2, unit = "cm"))
p_legend2 <- p_legend2 + scale_color_manual(name="P-values", 
                                            labels=c("Significant", "Not significant", "", ""), 
                                            values=c("black", "white", "white", "white")) +
  guides(color = guide_legend(override.aes = list(shape = c(20, 20, 20, 20))))
p_legend2 

# plot1 with legend
combined_plot1 <- grid.arrange(p1, p2, p3, p4, ncol = 2)
#combined_plot2 <- grid.arrange(p3, p4, ncol = 2)
caption <- "Average Plausibility + Single Plausibility + Surprisal"

empty_row <- grid.rect(gp = gpar(fill = "white", col = "white"))

# function to extract legend from plot 
get_only_legend <- function(plot) { 
  plot_table <- ggplot_gtable(ggplot_build(plot)) 
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box") 
  legend <- plot_table$grobs[[legend_plot]] 
  return(legend) 
} 

# extract legend from plot1 using above function 
#legend1 <- get_only_legend(p_legend1)
#legend2 <- get_only_legend(p_legend2) 
#combined_plot_with_legend <- grid.arrange(empty_row, combined_plot1, empty_row, legend1, nrow = 5, heights = c(0.2,4.5,0.20, 0.3,4.5,0.20,0.3))
#combined_plot_with_legend <- grid.text(caption, x = 0.463, y = 0.98, gp = gpar(fontsize = 13))
#combined_plot_with_legend

legend <- get_only_legend(p_legend) 
legend2 <- get_only_legend(p_legend2) 
combined_plot_with_legend <- grid.arrange(empty_row, combined_plot1, legend, legend2, nrow = 4, heights = c(0.2,9,0.4,0.4))
combined_plot_with_legend <- grid.text(caption, x = 0.385, y = 0.98, gp = gpar(fontsize = 13))
combined_plot_with_legend

setwd("~/Downloads/Master_Thesis/Plots_Post_Hoc/")
# Save the combined plot
ggsave("Combined_Plot.pdf", combined_plot_with_legend, device = "pdf")

