### LMER ###
# load lme4 package
library(lme4)
#library(lmerTest)
library(dplyr)
library(ggplot2)

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

# plot residuals
# Create a line plot 
p <- ggplot(residuals, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                           y = Residual, color = Condition, group = Condition)) + geom_point(shape = 4, size = 3.5, stroke = 0.4) + geom_line(linewidth=0.5) + ylim (0.10, -0.10)
p <- p + theme_minimal() + geom_errorbar(aes(ymin=Residual-SE_Residual, ymax=Residual+SE_Residual), width=.1, size=0.3) 
p <- p + scale_color_manual(name="Condition", labels=c("A: Plausible", "B: Medium Plausible", "C: Implausible"), values=c("#000000", "#FF0000", "#0000FF"))
p <- p + labs(x="Region", y="logRT", title = "Residuals: Avg + Single PlausT + SurprisalD") 
p <- p + theme(legend.position="bottom", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) 
p 
ggsave("Residuals_Plot.pdf", p, width=4, height=4)


# plot estimated logRTs
# Create a line plot 
p <- ggplot(logRT_estimated, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                                 y = Estimated_logRT, color = Condition, group = Condition)) + geom_point(shape = 4, size = 3.5, stroke = 0.4) + geom_line(linewidth=0.5) + ylim (5.5, 5.7)
p <- p + theme_minimal() + geom_errorbar(aes(ymin=Estimated_logRT-SE_Estimated, ymax=Estimated_logRT+SE_Estimated), width=.1, size=0.3) 
p <- p + scale_color_manual(name="Condition", labels=c("A: Plausible", "B: Medium Plausible", "C: Implausible"), values=c("#000000", "#FF0000", "#0000FF"))
p <- p + labs(x="Region", y="logRT", title = "Estimated RTs") 
p <- p + theme(legend.position="bottom", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) 
p 
ggsave("Estimated_RTs_Plot.pdf", p, width=4, height=4)

# plot intercept and coefficients added to intercept
p <- ggplot(SPR_coefficients, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                                  y = Estimate_value, color = Estimate, group = Estimate)) + geom_point(shape = 4, size = 3.5, stroke = 0.4) + geom_line(linewidth=0.5) + ylim (5.5, 5.7)
p <- p + theme_minimal() + geom_errorbar(aes(ymin=Estimate_value-Estimate_error, ymax=Estimate_value+Estimate_error), width=.1, size=0.3) 
p <- p + scale_color_manual(name="Coefficients", labels=c("Intercept", "Target Plausibility (Average)", "Target Plausibility (Single)", "Distractor Surprisal"), values=c("#000000", "#FF00FF", "#00CC33", "#00FFFF"))
p <- p + labs(x="Region", y="SPR Coefficients", title = "Coefficients") 
p <- p + theme(legend.position="bottom", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) 
p 
ggsave("Intercept_Coefficients_Plot.pdf", p, width=4, height=4)


# plot effect sizes (z-vaues)
Effect_sizes <- subset(SPR_coefficients, Estimate != 'Intercept')

p <- ggplot(Effect_sizes, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                              y = Z_value, color = Estimate, group = Estimate)) + geom_point(shape = 4, size = 3.5, stroke = 0.4) + geom_line(linewidth=0.5) + ylim (-5, 5)
p <- p + geom_hline(yintercept=0, linetype=2)
p <- p + theme_minimal()
p <- p + scale_color_manual(name="Coefficients", labels=c( "Target Plausibility (Average)", "Target Plausibility (single)", "Distractor Surprisal"), values=c("#FF00FF", "#00CC33", "#00FFFF"))
p <- p + labs(x="Region", y="Z-values", title = "Inferential Statistics") 
p <- p + theme(legend.position="bottom", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) 
p 
ggsave("Z_values_Plot.pdf", p, width=4, height=4)

