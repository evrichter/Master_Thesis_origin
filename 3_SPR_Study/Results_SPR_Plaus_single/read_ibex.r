### SPR PARSING AND DATA VIZ 
## CHRISTOPH AURNHAMMER, 2021
## Adapted from comp_ibex.r

### PACKAGES
library(ggplot2)
library(data.table)
library(gridExtra)
library(dplyr)

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
GP6$logRT <- log(GP6$ReadingTime)
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
    MeanReadingTime = mean(logRT, na.rm = TRUE), 
    SE = sd(logRT, na.rm = TRUE) / sqrt(n()))
# Print the resulting averages
print(averages)

# Exclude Pre-critical_2
averages <- averages %>% 
  filter(Region != "Pre-critical_2")
print(averages)

# Create a line plot with average log-transformed reading times
p <- ggplot(averages, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                     y = MeanReadingTime, color = Condition, group = Condition)) + geom_point(shape = 4, size = 3.5, stroke = 0.8) + geom_line(linewidth=0.5) + ylim (5.5, 5.7)
p <- p + theme_minimal() + geom_errorbar(aes(ymin= MeanReadingTime-SE, ymax=MeanReadingTime+SE), width=.1, size=0.5) 
p <- p + scale_color_manual(name="Condition", labels=c("A: Plausible", "B: Medium Plausible", "C: Implausible"), values=c("#000000", "#FF0000", "#0000FF"))
p <- p + theme(legend.position="bottom", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) 
p <- p + labs(x="Region", y="logRT", title = "Observed RTs") 
p 
ggsave("Observed_RTs_Plot.pdf", p, width=4, height=4)

# CALCULARE CORRELATIONS
# correlation avg Plaus per item and condition & suprisal from Plaus study
GP6 <- read.csv("GP6SPR.csv")  #use unprocessed file bc in prestudies no outliers based on RTs were removed yet
correlation_matrix <- cor(GP6[, c("Plaus_target_avg", "Plaus_dist_avg", "Surprisal_target", "Surprisal_distractor")])
correlation_matrix

# correlation svg Plaus per item and condition & surprisal from SPR study
# no plausibility distractor column, bc no distractor values were calculated for plausibility in the SPR study
correlation_matrix <- cor(GP6[, c("SPR_Plaus_Rating", "Surprisal_target", "Surprisal_distractor")])
correlation_matrix

# calculate correlation coefficient between SPR_Plaus_avg (avg Plausratings from SPR study) and Plaus_target_avg (avg Plausratings from Plaus study)
correlation <- cor(GP6$SPR_Plaus_avg, GP6$Plaus_target_avg)
cat("Correlation between SPR_Plaus_avg and Plaus_target_avg:", correlation)


########## READ PROCESSED DATA
#setwd("~/Downloads/Master_Thesis/3_SPR_Study/Results/")
#df_bal <- fread("GP6SPR_processed.csv")
#df_bal <- df
#df_bal$Subject <- as.factor(df$Subject)

# # data exclusion per region
#cutoff_in_sd = 4
# # df_first <- exclude(df_bal[ReadingTime > 100 & ReadingTime < 2500 & Region == "First",], sd_cutoff=cutoff_in_sd)
# # df_second <- exclude(df_bal[ReadingTime > 100 & ReadingTime < 2500 & Region == "Second",], sd_cutoff=cutoff_in_sd)
# # df_third <- exclude(df_bal[ReadingTime > 100 & ReadingTime < 2500 & Region == "Third",], sd_cutoff=cutoff_in_sd)
# # df_fourth <- exclude(df_bal[ReadingTime > 100 & ReadingTime < 2500 & Region == "Fourth",], sd_cutoff=cutoff_in_sd)
# # df_fifth <- exclude(df_bal[ReadingTime > 100 & ReadingTime < 2500 & Region == "Fifth",], sd_cutoff=cutoff_in_sd)
# # df_sixth <- exclude(df_bal[ReadingTime > 100 & ReadingTime < 2500 & Region == "Sixth",], sd_cutoff=cutoff_in_sd)
#df_precrit_2 <- exclude(df_bal[ReadingTime > 100 & ReadingTime < 2500 & Region == "Pre-critical_2",], sd_cutoff=cutoff_in_sd)
#df_precrit <- exclude(df_bal[ReadingTime > 100 & ReadingTime < 2500 & Region == "Pre-critical",], sd_cutoff=cutoff_in_sd)
#df_crit <- exclude(df_bal[ReadingTime > 100 & ReadingTime < 2500 & Region == "Critical",], sd_cutoff=cutoff_in_sd)
#df_spill <- exclude(df_bal[ReadingTime > 100 & ReadingTime < 2500 & Region == "Spillover",], sd_cutoff=cutoff_in_sd)
#df_postspill <- exclude(df_bal[ReadingTime > 100 & ReadingTime < 2500 & Region == "Post-spillover",], sd_cutoff=cutoff_in_sd)



# #### DATA VISUALISATION
# Data Viz for avg Plausratings from SPR Study
library(ggplot2)

setwd("~/Downloads/Master_Thesis/3_SPR_Study/Results_SPR_Plaus_single/")
dt <- fread("GP6SPR_processed.csv") #plots plausratings after removing outliers


means <- aggregate(SPR_Plaus_avg ~ Condition, dt, FUN=mean)
means$Plaus_SE <- aggregate(SPR_Plaus_avg ~ Condition, dt, FUN=se)$SPR_Plaus_avg
dt_items_abc <- dt[, lapply(.SD, mean), by=list(Item, Condition), .SDcols=c("SPR_Plaus_avg")]

# density plot
p <- ggplot(dt_items_abc, aes(x=SPR_Plaus_avg, color=Condition, fill=Condition)) + geom_density(alpha=0.4) + theme_minimal() + xlim(1,7) + ylim(0, 1.5)
p <- p + geom_vline(data=means, aes(xintercept=SPR_Plaus_avg, color=Condition), linetype="dashed") + scale_x_continuous(breaks=seq(1,7))
p <- p + scale_color_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p <- p + scale_fill_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p <- p + labs(title = "Plausibility (SPR)", y="Density", x="Plausibility" )
ggsave("DensityPlot_Plausibility_SPR.pdf", p, device=cairo_pdf, width=4, height=4)
p

# barplot
q <- ggplot(means, aes(x=Condition, y=SPR_Plaus_avg)) + geom_bar(stat="identity") + labs(title = "Average Plausibility Ratings per Condition (SPR)", y = "Plausibility",  x = "Condition") + geom_errorbar(aes(ymin=SPR_Plaus_avg-Plaus_SE, ymax=SPR_Plaus_avg+Plaus_SE), width=.4, position=position_dodge(.9)) + theme_minimal() + coord_cartesian(ylim = c(1, 7)) + scale_y_continuous(breaks = c(1:7))
ggsave("BarPlot_Plausibility_SPR.pdf", q, device=cairo_pdf, width=4, height=4)
q

###
# ## Per Condition RTs per region
# #df_bal_excl <- rbind(df_first, df_second, df_third, df_fourth, df_fifth, df_sixth)
# #df_bal_excl <- rbind(df_precrit_2, df_precrit, df_crit, df_spill, df_postspill)
# df_bal_excl <- rbind(df_precrit, df_crit, df_spill, df_postspill)


# # PLOT WORDSTEPS
# df_bal_excl$ReadingTime <- log(df_bal_excl$ReadingTime)
# plusminus <- aggregate(ReadingTime ~ Region + Condition, df_bal_excl, FUN=mean)
# #plusminus$Region <- factor(plusminus$Region, levels=c("First", "Second", "Third", "Fourth", "Fifth", "Sixth"))
# plusminus$Region <- factor(plusminus$Region, levels=c("Pre-critical", "Critical", "Spillover", "Post-spillover"))
# plusminus$SE <- aggregate(ReadingTime ~ Region + Condition, df_bal_excl, FUN=se)$ReadingTime

# p <- ggplot(plusminus, aes(x=Region, y=ReadingTime, color=Condition, group=Condition)) + geom_point(size=2.2) + geom_line(size=0.5)
# p <- p + theme_minimal() + geom_errorbar(aes(ymin=ReadingTime-SE, ymax=ReadingTime+SE), width=.1, size=0.3)
# p <- p + scale_color_manual(name="Condition", labels=c("A: Expected Plausible", "B: Unexpected Less Plausible", "C: Unexpected Implausible"), values=c("#000000", "#BB5566", "#004488", "#DDAA33"))
# p <- p + theme(legend.position="bottom", legend.text=element_text(size=7), legend.title=element_text(size=7))
# p <- p + labs(x="Region", y="Reading Time")
# p
# ggsave("/Users/chr/Desktop/35Subjects_SPR.pdf", p, width=4, height=4)

# ## Reaction Time
# # Histogram Reaction Time
# df_react <- df_crit[!is.na(df_crit$ReactionTime),]

# p <- ggplot(df_react, aes(ReactionTime)) + geom_histogram() + theme_minimal() 
# p <- p + geom_vline(xintercept=mean(df_react$ReactionTime)) + geom_vline(xintercept=median(df_react$ReactionTime))
# p

# p <- ggplot(df_react, aes(log(ReactionTime))) + geom_histogram() + theme_minimal()
# p

# # Barplot Reaction Time
# reac_agg <- df_react[, lapply(.SD, mean, na.rm=TRUE), by=Condition, .SDcols=c("ReactionTime")]
# reac_agg$SE <- df_react[, lapply(.SD, se, na.rm=TRUE), by=Condition, .SDcols=c("ReactionTime")]$ReactionTime
# p <- ggplot(reac_agg, aes(x=Condition, y=ReactionTime)) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=ReactionTime-SE, ymax=ReactionTime+SE), width=.2)
# p <- p + theme_minimal() + labs(x="Condition", y="Reaction Time", title="CAP SPR Reaction Times", subtitle="A: Expected Plausible B: Unexpected Less Plausible C: Unexpected Implausible")
# #p <- p + ylim(0, 3500)
# p

# ## Accuracy
# # Barplot Accuracy
# acc_agg <- df_react[, lapply(.SD, mean, na.rm=TRUE), by=Condition, .SDcols=c("Accuracy")]
# acc_agg$SE <- df_react[, lapply(.SD, se, na.rm=TRUE), by=Condition, .SDcols=c("Accuracy")]$Accuracy
# p <- ggplot(acc_agg, aes(x=Condition, y=Accuracy)) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=Accuracy-SE, ymax=Accuracy+SE), width=.2)
# p <- p + theme_minimal() + labs(x="Condition", y="Accuracy", title="CAP SPR Accuracies", subtitle="A: Expected Plausible B: Unexpected Less Plausible C: Unexpected Implausible")
# p <- p + ylim(0, 1)
# p


