### PACKAGES
library(ggplot2)
library(data.table)
library(gridExtra)
library(dplyr)
library(gridExtra)
library(grid)

setwd("~/Downloads/Master_Thesis/3_SPR_Study/Precritical2_RTs/")
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
df$precritRT <- rep(df[Region=="Precrit",]$ReadingTime, each=7)
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

avg_logRT_by_regions_and_conditions <- data.frame(
  Region = character(0),
  Condition = character(0),
  MeanReadingTime = numeric(0),
  SE_MeanReadingTime = numeric(0)
)

regions <- c("Main_Verb", "Precrit_3", "Precrit_2", "Precrit", "Critical", "Spillover", "Post-spillover") 
conditions <- c("A", "B", "C") 

# for log RTs
averages <- GP6 %>%
  group_by(Region, Condition) %>%
  summarise(
    MeanReadingTime = mean(logRT, na.rm = TRUE), #first log transform each value in ReadingTime column and then take the mean per region and condition
    SE = sd(logRT, na.rm = TRUE) / sqrt(n()))
# Print the resulting averages
print(averages)

# for not log RT
#averages <- GP6 %>%
#  group_by(Region, Condition) %>%
#  summarise(
#    MeanReadingTime = mean(ReadingTime, na.rm = TRUE), #first log transform each value in ReadingTime column and then take the mean per region and condition
#    SE = sd(logRT, na.rm = TRUE) / sqrt(n()))
# Print the resulting averages
#print(averages)

# Exclude Pre-critical_2
#averages <- averages %>% 
#  filter(Region != "Pre-critical_2")
#print(averages)

# Create a line plot with average log-transformed reading times
p1 <- ggplot(averages, aes(x = factor(Region, levels = c("Main_Verb", "Precrit_3", "Precrit_2", "Precrit", "Critical", "Spillover", "Post-spillover")), 
                          y = MeanReadingTime, color = Condition, group = Condition)) + geom_point(shape = 4, size = 3.5, stroke = 0.8) + geom_line(linewidth=0.5) + ylim (5.48, 5.68)
p1 <- p1 + theme_minimal() + geom_errorbar(aes(ymin= MeanReadingTime-SE, ymax=MeanReadingTime+SE), width=.1, size=0.5) 
p1 <- p1 + scale_color_manual(name="Condition", labels=c("A: Plausible", "B: Medium Plausible", "C: Implausible"), values=c("#000000", "#FF0000", "#0000FF"))
p1 <- p1 + theme(legend.position="none", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) 
p1 <- p1 + labs(x="Region", y="logRT", title = "Log RTs (SPR1)") 
p1 <- p1 + theme(legend.position="none", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 8))
p1 <- p1 + theme(plot.title = element_text(size=10)) + theme(plot.margin = margin(t = 0.5, r = 0.2, b = 0, l = 0.2, unit = "cm"))
p1 

#############################################################

setwd("~/Downloads/Master_Thesis/3_SPR_Study_2/Precritical2_RTs/")
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
#pr <- get_plausibility_rating("task.txt")
rd <- get_reads("reading.txt")

df <- merge(rd, rc[,c("ReactionTime", "Accuracy", "IPhash", "Item")], by=c("IPhash", "Item"), all=TRUE)
#df <- merge(df, pr[,c("IPhash", "Item", "Condition", "SPR_Plaus_Rating", "SPR_Plaus_avg")], by=c("IPhash", "Item", "Condition"), all=TRUE)

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
df$precritRT <- rep(df[Region=="Precrit",]$ReadingTime, each=7)
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
#GP6 <- df

#plaus_averages_by_condition <- aggregate(SPR_Plaus_avg ~ Condition, GP6, FUN = mean)
#plaus_averages_by_condition
#plaus_sd_by_condition <- aggregate(SPR_Plaus_avg ~ Condition, GP6, FUN = sd)
#plaus_sd_by_condition
#plaus_range_by_condition <- aggregate(SPR_Plaus_avg ~ Condition, GP6, FUN = range)
#plaus_range_by_condition

#log transform reading times and add them as new column to GP6
GP6$logRT <- log(GP6$ReadingTime)
### Calculate average reading times per region and per condition ###

avg_logRT_by_regions_and_conditions <- data.frame(
  Region = character(0),
  Condition = character(0),
  MeanReadingTime = numeric(0),
  SE_MeanReadingTime = numeric(0)
)

regions <- c("Main_Verb", "Precrit_3", "Precrit_2", "Precrit", "Critical", "Spillover", "Post-spillover") 
conditions <- c("A", "B", "C") 

# for log RTs
averages <- GP6 %>%
  group_by(Region, Condition) %>%
  summarise(
    MeanReadingTime = mean(logRT, na.rm = TRUE), #first log transform each value in ReadingTime column and then take the mean per region and condition
    SE = sd(logRT, na.rm = TRUE) / sqrt(n()))
# Print the resulting averages
print(averages)

# for not log RT
#averages <- GP6 %>%
#  group_by(Region, Condition) %>%
#  summarise(
#    MeanReadingTime = mean(ReadingTime, na.rm = TRUE), #first log transform each value in ReadingTime column and then take the mean per region and condition
#    SE = sd(logRT, na.rm = TRUE) / sqrt(n()))
# Print the resulting averages
#print(averages)

# Exclude Pre-critical_2
#averages <- averages %>% 
#  filter(Region != "Pre-critical_2")
#print(averages)

# Create a line plot with average log-transformed reading times
p2 <- ggplot(averages, aes(x = factor(Region, levels = c("Main_Verb", "Precrit_3", "Precrit_2", "Precrit", "Critical", "Spillover", "Post-spillover")), 
                          y = MeanReadingTime, color = Condition, group = Condition)) + geom_point(shape = 4, size = 3.5, stroke = 0.8) + geom_line(linewidth=0.5) + ylim (5.48, 5.68)
p2 <- p2 + theme_minimal() + geom_errorbar(aes(ymin= MeanReadingTime-SE, ymax=MeanReadingTime+SE), width=.1, size=0.5) 
p2 <- p2 + scale_color_manual(name="Condition", labels=c("A: Plausible", "B: Medium Plausible", "C: Implausible"), values=c("#000000", "#FF0000", "#0000FF"))
p2 <- p2 + theme(legend.position="none", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) 
p2 <- p2 + labs(x="Region", y="logRT", title = "Log RTs (SPR2)") 
p2 <- p2 + theme(legend.position="none", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 8))
p2 <- p2 + theme(plot.title = element_text(size=10)) + theme(plot.margin = margin(t = 0.5, r = 0.2, b = 0, l = 0.2, unit = "cm"))
p2 
##############################################

# Create a line plot with average log-transformed reading times
p_legend <- ggplot(averages, aes(x = factor(Region, levels = c("Main_Verb", "Precrit_3", "Precrit_2", "Precrit", "Critical", "Spillover", "Post-spillover")), 
                           y = MeanReadingTime, color = Condition, group = Condition)) + geom_point(shape = 4, size = 3.5, stroke = 0.8) + geom_line(linewidth=0.5) + ylim (5.4, 5.7)
p_legend <- p_legend + theme_minimal() + geom_errorbar(aes(ymin= MeanReadingTime-SE, ymax=MeanReadingTime+SE), width=.1, size=0.5) 
p_legend <- p_legend + scale_color_manual(name="Condition", labels=c("A: Plausible", "B: Medium Plausible", "C: Implausible"), values=c("#000000", "#FF0000", "#0000FF"))
p_legend <- p_legend + theme(legend.position="bottom", legend.text=element_text(size=10), legend.title=element_text(size=10), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) 
p_legend <- p_legend + labs(x="Region", y="logRT", title = "Observed RTs") 
p_legend 


# plot1 with legend
combined_plot <- grid.arrange(p1, p2, ncol = 1)
caption <- "Log Reading Times including Pre-critical Regions"

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
combined_plot_with_legend <- grid.text(caption, x = 0.467, y = 0.98, just = "center", gp = gpar(fontsize = 12))
combined_plot_with_legend

setwd("~/Downloads/Master_Thesis/Plots_Post_Hoc/")
# Save the combined plot
ggsave("Combined_Plot.pdf", combined_plot_with_legend, device = "pdf")
