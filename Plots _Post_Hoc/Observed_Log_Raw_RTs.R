### SPR PARSING AND DATA VIZ 
## CHRISTOPH AURNHAMMER, 2021
## Adapted from comp_ibex.r

### PACKAGES
library(ggplot2)
library(data.table)
library(gridExtra)
library(dplyr)
library(gridExtra)
library(grid)

setwd("~/Downloads/Master_Thesis/3_SPR_Study/Results_SPR_Plaus_single/")
source("ibex_fns.r")


#### DATA FORMATTING
# Get DEMOG CONSENT SURVEY data
cn <- get_consent("consent.txt")
dm <- get_demog("demog.txt")
sv <- get_survey("survey.txt")

survey <- merge(dm, sv, by=c("IPhash"), all=TRUE)
survey$Age <- as.numeric(survey$Age)

round(mean(survey$Age),2)
round(sd(survey$Age),2)
range(survey$Age)
table(survey$gender)
nrow(survey[!(Native_language %in% c("Deutsch", "deutsch")),])
table(survey$Task_difficulty)/length(survey$Task_difficulty)
table(survey$Experiment_length)/length(survey$Experiment_length)
table(survey$handedness)


# Get REACTION TIMES & READING TIMES
rc <- get_reacts("task.txt")
pr <- get_plausibility_rating("task.txt")
rd <- get_reads("reading.txt")

df <- merge(rd, rc[,c("ReactionTime", "Accuracy", "IPhash", "Item")], by=c("IPhash", "Item"), all=TRUE)
df <- merge(df, pr[,c("IPhash", "Item", "Condition", "SPR_Plaus_Rating", "SPR_Plaus_avg")], by=c("IPhash", "Item", "Condition"), all=TRUE)

# Change IPhashes to subject numbers
colnames(df)[1] <- "Subject"
df[, Subject := .GRP, by = .(Subject)]
df$Subject <- as.character(df$Subject)

# Done below after obtaining GP6_processed.csv file
# Check accuracies / reaction times per participant
#agg_df <- df[, lapply(.SD, mean, na.rm = TRUE), by=Subject, .SDcols=c("ReactionTime", "Accuracy", "ReadingTime")]
#agg_df$Subject <- as.factor(agg_df$Subject)
#agg_df[order(agg_df$ReactionTime),]
#agg_df[order(agg_df$Accuracy),]

# calculate mean accuracy and mean reaction time
#mean_accuracy <- mean(GP6$Accuracy, na.rm = TRUE)
#cat("Mean Accuracy:", as.numeric(mean_accuracy))
#mean_RT <- mean(GP6$ReactionTime, na.rm = TRUE)
#cat("Mean Reaction Time:", as.numeric(mean_RT))

# merge df with assoc plausibility and surprisal values in pretests
pretests <- fread("GradedP6_FollowUpStudy_Pretests.csv")
df <- merge(df, pretests[,c("Item", "Condition", "Verb", "Target", "Distractor", "Last_Mentioned", "Plaus_target_avg", "Plaus_dist_avg", "Surprisal_target", "Surprisal_distractor", "LeoLM_tar", "LeoLM_dist")], by=c("Item", "Condition"))

# add precritRT as predictor
df$precritRT <- rep(df[Region=="Pre-critical",]$ReadingTime, each=5)
# change column order
df <- df %>% relocate(precritRT, .before= ReadingTime)

# save merged files in new csv file
fwrite(df, "GP6SPR.csv")

# Verb length per cond
# df$ncharverb <- nchar(df$Verb)
# aggregate(ncharverb ~ Condition, df, mean)


### REMOVING OUTLIERS IN READING TIME AND REACTION TIME DATA
# Remove too high or low reading times and reaction times
df <- remove_outliers(df)

fwrite(df, "GP6SPR_processed.csv")

##### CHECK ACCURACIES AND READING TIMES #####
# saved df into GP6 csv file because when reading GP6 from the scratch it cannot be processed by the following functions 
# GP6 <- read.csv("GP6SPR_processed.csv")
GP6 <- df


# Check mean accuracies / mean reaction times PER PARTICIPANT
###calculate mean Reaction Time and Accuracy per subject ### just for info, is not included in thesis
# removed rows where reaction time was NA, because otherwise the lapply method includes them in the mean calculation which makes the mean lower
GP6 <- GP6[!(ReactionTime %in% c(NA)),] #remove rows
GP6 <- GP6[, lapply(.SD, mean, na.rm = TRUE), by=Subject, .SDcols=c("ReactionTime", "Accuracy")] #mean of accuracy and reaction time per subject
GP6$Subject <- as.factor(GP6$Subject)
GP6[order(GP6$ReactionTime),] #grouped  by reaction time
GP6[order(GP6$Accuracy),] # grouped by accuracy


# calculate mean accuracy and mean reaction time OF ALL PARTICIPANTS (based on rows containing reaction times and accuracy)
mean_accuracy <- mean(GP6$Accuracy, na.rm = TRUE)
cat("Mean Accuracy:", as.numeric(mean_accuracy))
mean_RT <- mean(GP6$ReactionTime, na.rm = TRUE)
cat("Mean Reaction Time:", as.numeric(mean_RT))

# calculate sd of accuracy and sd of reaction time of all participants
sd_accuracy <- sd(GP6$Accuracy, na.rm = TRUE)
cat("SD:", as.numeric(sd_accuracy))
sd_RT <- sd(GP6$ReactionTime, na.rm = TRUE)
cat("SD:", as.numeric(sd_RT))

# calculate range of accuracy and range of reaction time of all participants
range_accuracy <- range(GP6$Accuracy, na.rm = TRUE)
cat("Minimum value:", as.numeric(range_accuracy[1]), "\n")
cat("Maximum value:", as.numeric(range_accuracy[2]), "\n")
range_RT <- range(GP6$ReactionTime, na.rm = TRUE)
cat("Minimum value:", as.numeric(range_RT[1]), "\n")
cat("Maximum value:", as.numeric(range_RT[2]), "\n")


# calculate mean accuracies and mean reaction times PER PARTICIPANT AND CONDITION
#again rename df into GP6 because loading the GP6 csv file causes an error
#GP6 <- read.csv("GP6SPR_processed.csv")
GP6 <- df
GP6 <- GP6[!(ReactionTime %in% c(NA)),] #remove rows containing NA for reaction time and accuracy

conditions <- c("A", "B", "C") 
for (condition in conditions) 
{
  GP6_per_condition <- GP6[(Condition %in% c(condition)),] #subset condition
  GP6_per_condition <- GP6_per_condition[, lapply(.SD, mean, na.rm = TRUE), by=Subject, .SDcols=c("ReactionTime", "Accuracy")]
  GP6_per_condition[order(GP6_per_condition$ReactionTime),] #grouped  by reaction time
  GP6_per_condition[order(GP6_per_condition$Accuracy),] # grouped by accuracy
  
  #caclulate mean reaction time and accuracy per condition
  mean_accuracy <- mean(GP6_per_condition$Accuracy, na.rm = TRUE)
  cat("Mean Accuracy:", as.numeric(mean_accuracy), "\n")
  mean_RT <- mean(GP6_per_condition$ReactionTime, na.rm = TRUE)
  cat("Mean Reaction Time:", as.numeric(mean_RT), "\n")
  
  #calculate sd of accuracy and sd of reaction time of all participants
  sd_accuracy <- sd(GP6_per_condition$Accuracy, na.rm = TRUE)
  cat("SD:", as.numeric(sd_accuracy), "\n")
  sd_RT <- sd(GP6_per_condition$ReactionTime, na.rm = TRUE)
  cat("SD:", as.numeric(sd_RT), "\n")
  
  # calculate range of accuracy and range of reaction time of all participants
  range_accuracy <- range(GP6_per_condition$Accuracy, na.rm = TRUE)
  cat("Minimum value:", as.numeric(range_accuracy[1]), "\n")
  cat("Maximum value:", as.numeric(range_accuracy[2]), "\n")
  range_RT <- range(GP6_per_condition$ReactionTime, na.rm = TRUE)
  cat("Minimum value:", as.numeric(range_RT[1]), "\n")
  cat("Maximum value:", as.numeric(range_RT[2]), "\n")
}


# calculate MEAN PLAUSIBILITY RATINGS per condition [after removing outliers]
GP6 <- df

plaus_averages_by_condition <- aggregate(SPR_Plaus_avg ~ Condition, GP6, FUN = mean)
plaus_averages_by_condition
plaus_sd_by_condition <- aggregate(SPR_Plaus_avg ~ Condition, GP6, FUN = sd)
plaus_sd_by_condition
plaus_range_by_condition <- aggregate(SPR_Plaus_avg ~ Condition, GP6, FUN = range)
plaus_range_by_condition

#log transform reading times and add them as new column to GP6
GP6$logRT <- log(GP6$ReadingTime)
### Calculate average reading times per region and per condition ###

regions <- c("Pre-critical", "Critical", "Spillover", "Post-spillover") 
conditions <- c("A", "B", "C") 

averages <- GP6 %>%
  group_by(Region, Condition) %>%
  summarise(
    MeanReadingTime = mean(logRT, na.rm = TRUE), 
    SE = sd(logRT, na.rm = TRUE) / sqrt(n()))
# Print the resulting averages
print(averages)

# Exclude Pre-critical_2
averages <- averages %>% 
  filter(Region != "Pre-critical_2")
print(averages)

# Create a line plot with average log-transformed reading times
p1 <- ggplot(averages, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                          y = MeanReadingTime, color = Condition, group = Condition)) + geom_point(shape = 4, size = 3.5, stroke = 0.8) + geom_line(linewidth=0.5) + ylim (5.5, 5.65)
p1 <- p1 + theme_minimal() + geom_errorbar(aes(ymin= MeanReadingTime-SE, ymax=MeanReadingTime+SE), width=.1, size=0.5) 
p1 <- p1 + scale_color_manual(name="Condition", labels=c("A: Plausible", "B: Medium Plausible", "C: Implausible"), values=c("#000000", "#FF0000", "#0000FF"))
p1 <- p1 + theme(legend.position="none", legend.text=element_text(size=6), legend.title=element_text(size=6), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) 
p1 <- p1 + labs(x="Region", y="logRT", title = "Log RTs") 
p1 <- p1 + theme(legend.position="none", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 8))
p1 <- p1 + theme(plot.title = element_text(size=10)) + theme(plot.margin = margin(t = 0.5, r = 0.2, b = 0, l = 0.2, unit = "cm"))
p1 

#################################################################


setwd("~/Downloads/Master_Thesis/3_SPR_Study/not_log_transformed/")
source("ibex_fns.r")


#### DATA FORMATTING
# Get DEMOG CONSENT SURVEY data
cn <- get_consent("consent.txt")
dm <- get_demog("demog.txt")
sv <- get_survey("survey.txt")

survey <- merge(dm, sv, by=c("IPhash"), all=TRUE)
survey$Age <- as.numeric(survey$Age)

round(mean(survey$Age),2)
round(sd(survey$Age),2)
range(survey$Age)
table(survey$gender)
nrow(survey[!(Native_language %in% c("Deutsch", "deutsch")),])
table(survey$Task_difficulty)/length(survey$Task_difficulty)
table(survey$Experiment_length)/length(survey$Experiment_length)
table(survey$handedness)


# Get REACTION TIMES & READING TIMES
rc <- get_reacts("task.txt")
pr <- get_plausibility_rating("task.txt")
rd <- get_reads("reading.txt")

df <- merge(rd, rc[,c("ReactionTime", "Accuracy", "IPhash", "Item")], by=c("IPhash", "Item"), all=TRUE)
df <- merge(df, pr[,c("IPhash", "Item", "Condition", "SPR_Plaus_Rating", "SPR_Plaus_avg")], by=c("IPhash", "Item", "Condition"), all=TRUE)

# Change IPhashes to subject numbers
colnames(df)[1] <- "Subject"
df[, Subject := .GRP, by = .(Subject)]
df$Subject <- as.character(df$Subject)

# Done below after obtaining GP6_processed.csv file
# Check accuracies / reaction times per participant
#agg_df <- df[, lapply(.SD, mean, na.rm = TRUE), by=Subject, .SDcols=c("ReactionTime", "Accuracy", "ReadingTime")]
#agg_df$Subject <- as.factor(agg_df$Subject)
#agg_df[order(agg_df$ReactionTime),]
#agg_df[order(agg_df$Accuracy),]

# calculate mean accuracy and mean reaction time
#mean_accuracy <- mean(GP6$Accuracy, na.rm = TRUE)
#cat("Mean Accuracy:", as.numeric(mean_accuracy))
#mean_RT <- mean(GP6$ReactionTime, na.rm = TRUE)
#cat("Mean Reaction Time:", as.numeric(mean_RT))

# merge df with assoc plausibility and surprisal values in pretests
pretests <- fread("GradedP6_FollowUpStudy_Pretests.csv")
df <- merge(df, pretests[,c("Item", "Condition", "Verb", "Target", "Distractor", "Last_Mentioned", "Plaus_target_avg", "Plaus_dist_avg", "Surprisal_target", "Surprisal_distractor")], by=c("Item", "Condition"))

# add precritRT as predictor
df$precritRT <- rep(df[Region=="Pre-critical",]$ReadingTime, each=5)
# change column order
df <- df %>% relocate(precritRT, .before= ReadingTime)

# save merged files in new csv file
fwrite(df, "GP6SPR.csv")

# Verb length per cond
# df$ncharverb <- nchar(df$Verb)
# aggregate(ncharverb ~ Condition, df, mean)


### REMOVING OUTLIERS IN READING TIME AND REACTION TIME DATA
# Remove too high or low reading times and reaction times
df <- remove_outliers(df)

fwrite(df, "GP6SPR_processed.csv")

##### CHECK ACCURACIES AND READING TIMES #####
# saved df into GP6 csv file because when reading GP6 from the scratch it cannot be processed by the following functions 
# GP6 <- read.csv("GP6SPR_processed.csv")
GP6 <- df

# Check mean accuracies / mean reaction times PER PARTICIPANT
###calculate mean Reaction Time and Accuracy per subject ### just for info, is not included in thesis
# removed rows where reaction time was NA, because otherwise the lapply method includes them in the mean calculation which makes the mean lower
GP6 <- GP6[!(ReactionTime %in% c(NA)),] #remove rows
GP6 <- GP6[, lapply(.SD, mean, na.rm = TRUE), by=Subject, .SDcols=c("ReactionTime", "Accuracy")] #mean of accuracy and reaction time per subject
GP6$Subject <- as.factor(GP6$Subject)
GP6[order(GP6$ReactionTime),] #grouped  by reaction time
GP6[order(GP6$Accuracy),] # grouped by accuracy


# calculate mean accuracy and mean reaction time OF ALL PARTICIPANTS (based on rows containing reaction times and accuracy)
mean_accuracy <- mean(GP6$Accuracy, na.rm = TRUE)
cat("Mean Accuracy:", as.numeric(mean_accuracy))
mean_RT <- mean(GP6$ReactionTime, na.rm = TRUE)
cat("Mean Reaction Time:", as.numeric(mean_RT))

# calculate sd of accuracy and sd of reaction time of all participants
sd_accuracy <- sd(GP6$Accuracy, na.rm = TRUE)
cat("SD:", as.numeric(sd_accuracy))
sd_RT <- sd(GP6$ReactionTime, na.rm = TRUE)
cat("SD:", as.numeric(sd_RT))

# calculate range of accuracy and range of reaction time of all participants
range_accuracy <- range(GP6$Accuracy, na.rm = TRUE)
cat("Minimum value:", as.numeric(range_accuracy[1]), "\n")
cat("Maximum value:", as.numeric(range_accuracy[2]), "\n")
range_RT <- range(GP6$ReactionTime, na.rm = TRUE)
cat("Minimum value:", as.numeric(range_RT[1]), "\n")
cat("Maximum value:", as.numeric(range_RT[2]), "\n")


# calculate mean accuracies and mean reaction times PER PARTICIPANT AND CONDITION
#again rename df into GP6 because loading the GP6 csv file causes an error
#GP6 <- read.csv("GP6SPR_processed.csv")
GP6 <- df
GP6 <- GP6[!(ReactionTime %in% c(NA)),] #remove rows containing NA for reaction time and accuracy

conditions <- c("A", "B", "C") 
for (condition in conditions) 
{
  GP6_per_condition <- GP6[(Condition %in% c(condition)),] #subset condition
  GP6_per_condition <- GP6_per_condition[, lapply(.SD, mean, na.rm = TRUE), by=Subject, .SDcols=c("ReactionTime", "Accuracy")]
  GP6_per_condition[order(GP6_per_condition$ReactionTime),] #grouped  by reaction time
  GP6_per_condition[order(GP6_per_condition$Accuracy),] # grouped by accuracy
  
  #caclulate mean reaction time and accuracy per condition
  mean_accuracy <- mean(GP6_per_condition$Accuracy, na.rm = TRUE)
  cat("Mean Accuracy:", as.numeric(mean_accuracy), "\n")
  mean_RT <- mean(GP6_per_condition$ReactionTime, na.rm = TRUE)
  cat("Mean Reaction Time:", as.numeric(mean_RT), "\n")
  
  #calculate sd of accuracy and sd of reaction time of all participants
  sd_accuracy <- sd(GP6_per_condition$Accuracy, na.rm = TRUE)
  cat("SD:", as.numeric(sd_accuracy), "\n")
  sd_RT <- sd(GP6_per_condition$ReactionTime, na.rm = TRUE)
  cat("SD:", as.numeric(sd_RT), "\n")
  
  # calculate range of accuracy and range of reaction time of all participants
  range_accuracy <- range(GP6_per_condition$Accuracy, na.rm = TRUE)
  cat("Minimum value:", as.numeric(range_accuracy[1]), "\n")
  cat("Maximum value:", as.numeric(range_accuracy[2]), "\n")
  range_RT <- range(GP6_per_condition$ReactionTime, na.rm = TRUE)
  cat("Minimum value:", as.numeric(range_RT[1]), "\n")
  cat("Maximum value:", as.numeric(range_RT[2]), "\n")
}


# calculate MEAN PLAUSIBILITY RATINGS per condition [after removing outliers]
GP6 <- df

plaus_averages_by_condition <- aggregate(SPR_Plaus_avg ~ Condition, GP6, FUN = mean)
plaus_averages_by_condition
plaus_sd_by_condition <- aggregate(SPR_Plaus_avg ~ Condition, GP6, FUN = sd)
plaus_sd_by_condition
plaus_range_by_condition <- aggregate(SPR_Plaus_avg ~ Condition, GP6, FUN = range)
plaus_range_by_condition

#log transform reading times and add them as new column to GP6
#GP6$logRT <- log(GP6$ReadingTime)
### Calculate average reading times per region and per condition ###

avg_logRT_by_regions_and_conditions <- data.frame(
  Region = character(0),
  Condition = character(0),
  MeanReadingTime = numeric(0),
  SE_MeanReadingTime = numeric(0)
)

regions <- c("Pre-critical", "Critical", "Spillover", "Post-spillover") 
conditions <- c("A", "B", "C") 

averages <- GP6 %>%
  group_by(Region, Condition) %>%
  summarise(
    MeanReadingTime = mean(ReadingTime, na.rm = TRUE), 
    SE = sd(ReadingTime, na.rm = TRUE) / sqrt(n()))
# Print the resulting averages
print(averages)

# Exclude Pre-critical_2
averages <- averages %>% 
  filter(Region != "Pre-critical_2")
print(averages)

# Create a line plot with average raw reading times
p2 <- ggplot(averages, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                          y = MeanReadingTime, color = Condition, group = Condition)) + geom_point(shape = 4, size = 3.5, stroke = 0.8) + geom_line(linewidth=0.5) + ylim (260, 310)
p2 <- p2 + theme_minimal() + geom_errorbar(aes(ymin= MeanReadingTime-SE, ymax=MeanReadingTime+SE), width=.1, size=0.5) 
p2 <- p2 + scale_color_manual(name="Condition", labels=c("A: Plausible", "B: Medium Plausible", "C: Implausible"), values=c("#000000", "#FF0000", "#0000FF"))
p2 <- p2 + theme(legend.position="none", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) 
p2 <- p2 + labs(x="Region", y="RT", title = "Raw RTs") 
p2 <- p2 + theme(legend.position="none", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 8))
p2 <- p2 + theme(plot.title = element_text(size=10)) + theme(plot.margin = margin(t = 0.5, r = 0.2, b = 0, l = 0.2, unit = "cm"))
p2 

########################################################################################################

# load packages
library(lme4) 
library(lmerTest) #for printing p-values (results are the same like when using lme4, but causes error message that model failed to converge)
library(dplyr)
library(ggplot2)

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

# Create a line plot for estimated logRTs
p3 <- ggplot(logRT_estimated, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                                 y = Estimated_logRT, color = Condition, group = Condition)) + geom_point(shape = 4, size = 3.5, stroke = 0.4) + geom_line(linewidth=0.5) + ylim (5.5, 5.65)
p3 <- p3 + theme_minimal() + geom_errorbar(aes(ymin=Estimated_logRT-SE_Estimated, ymax=Estimated_logRT+SE_Estimated), width=.1, size=0.3) 
p3 <- p3 + scale_color_manual(name="Condition", labels=c("A: Plausible", "B: Medium Plausible", "C: Implausible"), values=c("#000000", "#FF0000", "#0000FF"))
p3 <- p3 + labs(x="Region", y="logRT", title = "Estimated Log RTs") 
p3 <- p3 + theme(legend.position="none", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 8))
p3 <- p3 + theme(plot.title = element_text(size=10)) + theme(plot.margin = margin(t = 0.5, r = 0.2, b = 0, l = 0.2, unit = "cm"))
p3 

###############################################################################

setwd("~/Downloads/Master_Thesis/3_SPR_Study/not_log_transformed/")
GP6 <- read.csv("GP6SPR_processed.csv")

residuals <- data.frame(
  Region = character(0),
  Condition = character(0),
  Residual = numeric(0),
  SE_Residual = numeric(0)
)

RT_estimated <- data.frame(
  Region = character(0),
  Condition = character(0),
  Estimated_RT = numeric(0),
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
  region_subset$scaled_Plaus_per_region <- scale(region_subset$SPR_Plaus_Rating)
  region_subset$scaled_Surprisaldist_per_region <- scale(region_subset$Surprisal_distractor)
  # invert predictor plausibility
  region_subset$inverted_scaled_Plaus_per_region <- (region_subset$scaled_Plaus_per_region) * (-1)
  #log transform reading times
  region_subset$RT_per_region <- region_subset$ReadingTime
  
  
  # define and run the linear mixed-effects regression model for the precritical region 
  model_per_region <- lmerTest::lmer(RT_per_region ~ inverted_scaled_Plaus_per_region + scaled_Surprisaldist_per_region + 
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
    Residual_region_per_condition <- mean(region_per_condition$RT_per_region) - mean(region_per_condition$region_per_condition_Predicted)
    
    # observed RT for condition A precritical
    region_per_condition_RT_observed <- mean(region_per_condition$RT_per_region)
    
    # estimated RT for condition A precritical
    region_per_condition_RT_estimated <- mean(region_per_condition$region_per_condition_Predicted)
    
    # calculate standard error for residuals
    SE_residuals_region_per_condition <- sqrt(sd(region_per_condition$RT_per_region, na.rm = TRUE)^2/length(region_per_condition$RT_per_region) + sd(region_per_condition$region_per_condition_Predicted, na.rm = TRUE)^2/length(region_per_condition$region_per_condition_Predicted))
    
    new_row_residuals <- data.frame(Region = region, Condition = condition, Residual = Residual_region_per_condition, SE_Residual = SE_residuals_region_per_condition)
    residuals <- rbind(residuals, new_row_residuals)
    
    # calculate standard error for RT estimated
    SE_estimated_region_per_condition <- sd(region_per_condition$region_per_condition_Predicted, na.rm = TRUE) / sqrt(length(region_per_condition$region_per_condition_Predicted)) 
    
    new_row_RT_estimated <- data.frame(Region = region, Condition = condition, Estimated_RT = region_per_condition_RT_estimated, SE_Estimated = SE_estimated_region_per_condition)
    RT_estimated <- rbind(RT_estimated, new_row_RT_estimated)
  }
}

# plot estimated RTs
# Create a line plot 
p4 <- ggplot(RT_estimated, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                              y = Estimated_RT, color = Condition, group = Condition)) + geom_point(shape = 4, size = 3.5, stroke = 0.4) + geom_line(linewidth=0.5) + ylim (260, 310)
p4 <- p4 + theme_minimal() + geom_errorbar(aes(ymin=Estimated_RT-SE_Estimated, ymax=Estimated_RT+SE_Estimated), width=.1, size=0.3) 
p4 <- p4 + scale_color_manual(name="Condition", labels=c("A: Plausible", "B: Medium Plausible", "C: Implausible"), values=c("#000000", "#FF0000", "#0000FF"))
p4 <- p4 + labs(x="Region", y="RT", title = "Estimated Raw RTs") 
p4 <- p4 + theme(legend.position="none", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 8))
p4 <- p4 + theme(plot.title = element_text(size=10)) + theme(plot.margin = margin(t = 0.5, r = 0.2, b = 0, l = 0.2, unit = "cm"))
p4 
############################################################################

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


# Create a line plot for residuals
p5 <- ggplot(residuals, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                           y = Residual, color = Condition, group = Condition)) + geom_point(shape = 4, size = 3.5, stroke = 0.4) + geom_line(linewidth=0.5) + ylim (0.10, -0.10)
p5 <- p5 + theme_minimal() + geom_errorbar(aes(ymin=Residual-SE_Residual, ymax=Residual+SE_Residual), width=.1, size=0.3) 
p5 <- p5 + scale_color_manual(name="Condition", labels=c("A: Plausible", "B: Medium Plausible", "C: Implausible"), values=c("#000000", "#FF0000", "#0000FF"))
p5 <- p5 + labs(x="Region", y="logRT", title = "Residuals (Log RTs)") 
p5 <- p5 + theme(legend.position="none", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 8))
p5 <- p5 + theme(plot.title = element_text(size=10)) + theme(plot.margin = margin(t = 0.5, r = 0.2, b = 0, l = 0.2, unit = "cm"))
p5  

##################################################################

setwd("~/Downloads/Master_Thesis/3_SPR_Study/not_log_transformed/")
GP6 <- read.csv("GP6SPR_processed.csv")

residuals <- data.frame(
  Region = character(0),
  Condition = character(0),
  Residual = numeric(0),
  SE_Residual = numeric(0)
)

RT_estimated <- data.frame(
  Region = character(0),
  Condition = character(0),
  Estimated_RT = numeric(0),
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
  region_subset$scaled_Plaus_per_region <- scale(region_subset$SPR_Plaus_Rating)
  region_subset$scaled_Surprisaldist_per_region <- scale(region_subset$Surprisal_distractor)
  # invert predictor plausibility
  region_subset$inverted_scaled_Plaus_per_region <- (region_subset$scaled_Plaus_per_region) * (-1)
  #log transform reading times
  region_subset$RT_per_region <- region_subset$ReadingTime
  
  
  # define and run the linear mixed-effects regression model for the precritical region 
  model_per_region <- lmerTest::lmer(RT_per_region ~ inverted_scaled_Plaus_per_region + scaled_Surprisaldist_per_region + 
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
    Residual_region_per_condition <- mean(region_per_condition$RT_per_region) - mean(region_per_condition$region_per_condition_Predicted)
    
    # observed RT for condition A precritical
    region_per_condition_RT_observed <- mean(region_per_condition$RT_per_region)
    
    # estimated RT for condition A precritical
    region_per_condition_RT_estimated <- mean(region_per_condition$region_per_condition_Predicted)
    
    # calculate standard error for residuals
    SE_residuals_region_per_condition <- sqrt(sd(region_per_condition$RT_per_region, na.rm = TRUE)^2/length(region_per_condition$RT_per_region) + sd(region_per_condition$region_per_condition_Predicted, na.rm = TRUE)^2/length(region_per_condition$region_per_condition_Predicted))
    
    new_row_residuals <- data.frame(Region = region, Condition = condition, Residual = Residual_region_per_condition, SE_Residual = SE_residuals_region_per_condition)
    residuals <- rbind(residuals, new_row_residuals)
    
    # calculate standard error for RT estimated
    SE_estimated_region_per_condition <- sd(region_per_condition$region_per_condition_Predicted, na.rm = TRUE) / sqrt(length(region_per_condition$region_per_condition_Predicted)) 
    
    new_row_RT_estimated <- data.frame(Region = region, Condition = condition, Estimated_RT = region_per_condition_RT_estimated, SE_Estimated = SE_estimated_region_per_condition)
    RT_estimated <- rbind(RT_estimated, new_row_RT_estimated)
  }
}

# plot residuals
# Create a line plot 
p6 <- ggplot(residuals, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                           y = Residual, color = Condition, group = Condition)) + geom_point(shape = 4, size = 3.5, stroke = 0.4) + geom_line(linewidth=0.5) + ylim (20, -20)
p6 <- p6 + theme_minimal() + geom_errorbar(aes(ymin=Residual-SE_Residual, ymax=Residual+SE_Residual), width=.1, size=0.3) 
p6 <- p6 + scale_color_manual(name="Condition", labels=c("A: Plausible", "B: Medium Plausible", "C: Implausible"), values=c("#000000", "#FF0000", "#0000FF"))
p6 <- p6 + labs(x="Region", y="RT", title = "Residuals (Raw RTs)") 
p6 <- p6 + theme(legend.position="none", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 8))
p6 <- p6 + theme(plot.title = element_text(size=10)) + theme(plot.margin = margin(t = 0.5, r = 0.2, b = 0, l = 0.2, unit = "cm"))
p6 

#############################################################################


# Create a line plot with average log-transformed reading times
p_legend <- ggplot(averages, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                          y = MeanReadingTime, color = Condition, group = Condition)) + geom_point(shape = 4, size = 3.5, stroke = 0.8) + geom_line(linewidth=0.5) + ylim (260, 310)
p_legend <- p_legend + theme_minimal() + geom_errorbar(aes(ymin= MeanReadingTime-SE, ymax=MeanReadingTime+SE), width=.1, size=0.5) 
p_legend <- p_legend + scale_color_manual(name="Condition", labels=c("A: Plausible", "B: Medium Plausible", "C: Implausible"), values=c("#000000", "#FF0000", "#0000FF"))
p_legend <- p_legend + theme(legend.position="bottom", legend.text=element_text(size=10), legend.title=element_text(size=10), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) 
p_legend <- p_legend + labs(x="Region", y="logRT", title = "Observed RTs") 
p_legend 


# plot1 with legend
combined_plot <- grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)
caption <- "Log and Raw Reading Times (SPR1)"

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
combined_plot_with_legend <- grid.arrange(empty_row, combined_plot, legend, nrow = 3, heights = c(0.2,9,0.8))
combined_plot_with_legend <- grid.text(caption, x = 0.35, y = 0.98, just = "center", gp = gpar(fontsize = 15))
combined_plot_with_legend

setwd("~/Downloads/Master_Thesis/Plots_Post_Hoc/")
# Save the combined plot
ggsave("Combined_Plot.pdf", combined_plot_with_legend, device = "pdf")
