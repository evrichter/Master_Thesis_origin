# Christoph Aurnhammer, 14.10.2021 
# Analyse and Process Plausibility Ratings

library(data.table)

# functions
se <- function(
    x,
    na.rm = FALSE
){
    if (na.rm == TRUE) {
        sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)]))
    } else {
        sd(x)/sqrt(length(x))
    }
}

assign_lists <- function(dt) {
    l1 <- dt[(Item == 1 & Condition == "A"),]$ID
    l2 <- dt[(Item == 1 & Condition == "B"),]$ID
    l3 <- dt[(Item == 1 & Condition == "C"),]$ID
    l4 <- dt[(Item == 1 & Condition == "Ad"),]$ID
    l5 <- dt[(Item == 1 & Condition == "Bd"),]$ID
    l6 <- dt[(Item == 1 & Condition == "Cd"),]$ID
    print(nrow(dt))
    dt$list <- rep(0, nrow(dt))
    dt[ID %in% l1,"list"] <- 1
    dt[ID %in% l2,"list"] <- 2
    dt[ID %in% l3,"list"] <- 3
    dt[ID %in% l4,"list"] <- 4
    dt[ID %in% l5,"list"] <- 5
    dt[ID %in% l6,"list"] <- 6

    dt
}

setwd("~/Downloads/Master_Thesis/1_Plausibility_Rating_Study/")

excluded_participants <- c("de5b35d54ab253562f33dbc220ce6ad1",
                           "2f3782f686d1994ea4a1aad5bc820ab6",
                           "df5a38a1a27c50a04e3f1b94eb0b2fa5",
                           "ec29f1a573fc71d127f3cf69bdfd761e",
                           "18cd916fc4c15ff5b1ce21dab6af7c67",
                           "43c15b8013a137c690185b798ed67b0a")

# Demog
demog <- fread("demog.txt")
# colnames(demog) <- c("time", "ID", "three", "four", "five", "six", "question", "answer")
colnames(demog) <- c("time", "ID", "three", "four", "five", "six", "seven", "question", "answer")
demog[,c("three", "four", "five", "six")] <- NULL
demog <- dcast(demog, time + ID ~ question, value.var="answer")
demog <- demog[time > 1634211007,]
demog <- demog[!(ID %in% excluded_participants),] 


# consent 
consent <- fread("consent.txt")
# colnames(consent) <- c("time", "ID", "three", "four", "five", "six", "question", "answer")
colnames(consent) <- c("time", "ID", "three", "four", "five", "six", "seven", "question", "answer")
consent <- consent[time > 1634211007,]
consent <- consent[!(ID %in% excluded_participants),] 


# Ratings
dt <- fread("rating.txt")
# colnames(dt) <- c("time", "ID", "controller", "four", "Condition", "Item", "text", "Rating", "nine", "ReactionTime")
colnames(dt) <- c("time", "ID", "controller", "four", "five", "Condition", "Item", "text", "Rating", "nine", "ReactionTime")
dt[,c("controller", "four", "text", "nine")] <- NULL
dt <- dt[time > 1634211007,]
dt <- dt[!(ID %in% excluded_participants),] 
probes <- dt[Condition=="P",]
dt <- dt[Condition != "P",]
dt <- assign_lists(dt)

# Looking at inter-rater agreement: Krippendorff's alpha
library(irr)

for (l in c(1:6)) {
    dtl <- dt[list == l,]
    dtlc <- dcast(dtl, ID ~ Item, value.var = "Rating")[, c(2:61)]
    dtlcm <- as.matrix(dtlc)
    print(l)
    print(kripp.alpha(dtlcm, method = "ordinal"))
}

# Probes
correct_probes <- data.table(Item=seq(61,72), Correct=c(1, 7, 7, 1, 1, 7, 1, 1, 7, 1, 7, 7))
probes <- merge(probes, correct_probes, by="Item")
probes$Accurate <- probes$Rating == probes$Correct
probes$AccSum <- probes$Rating == probes$Correct
aggregate(Accurate ~ ID, probes, mean) # accurate proportion per ID
aggregate(Accurate ~ ID, probes, sum) # number / 12 correct per ID
# no participant answered more than 2 out of 12 attention checks wrong

# Checks for completeness / balancing / lists
table(dt$ID, dt$Item)        # Did every subject see each item once and only once
table(dt$ID, dt$Condition)   # Did each condition get presented to each subject the same amount of times (i.e. 10)
table(dt$Item, dt$Condition) # How often was each condition of each item presented
table(dt$list)               # How many trials were collected per list


dt_items <- dt[, lapply(.SD, mean), by=list(Item, Condition), .SDcols=c("Rating", "ReactionTime")]
#dt_items[,c("Rating_SD", "ReactionTime_SD")] <- dt[, lapply(.SD, sd), by=list(Item, Condition), .SDcols=c("Rating", "ReactionTime")][,c(3,4)]
dt_items[,c("Rating_SE", "ReactionTime_SE")] <- dt[, lapply(.SD, se), by=list(Item, Condition), .SDcols=c("Rating", "ReactionTime")][,c(3,4)]
cond <- dt_items[, lapply(.SD, mean), by=list(Condition), .SDcols=c("Rating", "ReactionTime")]
condse <- dt_items[, lapply(.SD, sd), by=list(Condition), .SDcols=c("Rating", "ReactionTime")]
cond <- merge(cond, condse, by="Condition", suffixes=c("", "_SE"))
cond

# ratings per item & condition sorted
stim <- dt_items[,lapply(.SD, mean), by=list(Item, Condition), .SDcols=c("Rating", "ReactionTime")]
dt[,lapply(.SD, mean), by=list(Item, Condition), .SDcols=c("Rating", "ReactionTime")]
condabc <- stim[Condition %in% c("A", "B", "C"),]
condd <- stim[Condition %in% c("Ad", "Bd", "Cd"),]

dt_items[order(dt_items),]
stim[order(stim$Item),]
condabc[order(condabc$Item),]
condd[order(condd$Item),]

items <- data.frame(condabc[order(data.frame(condabc)[,1], data.frame(condabc)[,2])])
items[,c("Ratingdist", "ReactionTimedist")] <- data.frame(condd[order(data.frame(condd)[,1], data.frame(condd)[,2])])[,c("Rating", "ReactionTime")]
fwrite(data.table(items), "plausresults.csv")


# (after doing the switcheroo on the conditions / target words)
# dt <- fread("GradedP6_results.csv")
dt <- fread("plausresults.csv")
# calculate average plausibility per condition
avg_condition_target <- aggregate(Rating ~ Condition, data = dt, FUN = "mean")
avg_condition_target
avg_condition_dist <- aggregate(Ratingdist ~ Condition, data = dt, FUN = "mean")
avg_condition_dist

# calculate sd per condition
sd_condition_target <- aggregate(Rating ~ Condition, data = dt, FUN = "sd")
sd_condition_target
sd_condition_dist <- aggregate(Ratingdist ~ Condition, data = dt, FUN = "sd")
sd_condition_dist

# calculate range per condition
range_condition_target <- aggregate(Rating ~ Condition, data = dt, FUN = "range")
range_condition_target
range_condition_dist <- aggregate(Ratingdist ~ Condition, data = dt, FUN = "range")
range_condition_dist

#condd <- dt_items[, lapply(.SD, mean), by=list(Condition), .SDcols=c("Plaus_distractor")]
#condd <- dt[, lapply(.SD, mean), by=list(Condition), .SDcols=c("Plaus_distractor")]






##### PLAUS DATA VIZ #####
library(ggplot2)
library(gridExtra)

### PLAUSIBILITY ###

# density plot target
means <- aggregate(Rating ~ Condition, dt, FUN=mean)
means$Plaus_SE <- aggregate(Rating ~ Condition, dt, FUN=se)$Rating
dt_items_abc <- dt[, lapply(.SD, mean), by=list(Item, Condition), .SDcols=c("Rating")]

p1 <- ggplot(dt_items_abc, aes(x=Rating, color=Condition, fill=Condition)) + geom_density(alpha=0.4) + theme_minimal() + ylim(0, 1)
p1 <- p1 + geom_vline(data=means, aes(xintercept=Rating, color=Condition), linetype="dashed") + scale_x_continuous(breaks=seq(1,7))
p1 <- p1 + scale_color_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p1 <- p1 + scale_fill_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p1 <- p1 + labs(title = "Target Plausibility", y="Density", x= "Rating") + theme(legend.position = "none") + theme(plot.title = element_text(hjust = 0.5))
ggsave("DensityPlot_Plausibility_Target.pdf", p1, device=cairo_pdf, width=4, height=4)
p1

# barplot target
q1 <- ggplot(means, aes(x=Condition, y=Rating)) + geom_bar(stat="identity") + labs(title = "Average Plausibility Ratings per Condition (Target)", y = "Plausibility", x = "Condition") + geom_errorbar(aes(ymin=Rating-Plaus_SE, ymax=Rating+Plaus_SE), width=.4, position=position_dodge(.9)) + theme_minimal() + coord_cartesian(ylim = c(1, 7)) + scale_y_continuous(breaks = c(1:7))
ggsave("BarPlot_Plausibility_Target.pdf", q1, device=cairo_pdf, width=4, height=4)
q1

# density plot distractor
means <- aggregate(Ratingdist ~ Condition, dt, FUN=mean)
means$Plaus_distractor_SE <- aggregate(Ratingdist ~ Condition, dt, FUN=se)$Ratingdist
dt_items_d <- dt[, lapply(.SD, mean), by=list(Item, Condition), .SDcols=c("Ratingdist")]

p2 <- ggplot(dt_items_d, aes(x=Ratingdist, color=Condition, fill=Condition)) + geom_density(alpha=0.4) + theme_minimal() + ylim(0, 1)
p2 <- p2 + geom_vline(data=means, aes(xintercept=Ratingdist, color=Condition), linetype="dashed") + scale_x_continuous(breaks=seq(1,7))
p2 <- p2 + scale_color_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p2 <- p2 + scale_fill_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p2 <- p2 + labs(title = "Distractor Plausibility", y = "Density", x="Rating") + theme(legend.position = "none") + theme(plot.title = element_text(hjust = 0.5))
ggsave("DensityPlot_Plausibility_Distractor.pdf", p2, device=cairo_pdf, width=4, height=4)
p2

# barplot distractor
q2 <- ggplot(means, aes(x=Condition, y=Ratingdist)) + geom_bar(stat="identity") + labs(title = "Average Plausibility Ratings per Condition (Distractor)", y = "Plausibility", x = "Condition") + geom_errorbar(aes(ymin=Ratingdist-Plaus_distractor_SE, ymax=Ratingdist+Plaus_distractor_SE), width=.4, position=position_dodge(.9)) + theme_minimal() + coord_cartesian(ylim = c(1, 7)) + scale_y_continuous(breaks = c(1:7))
ggsave("BarPlot_Plausibility_Distractor.pdf", q2, device=cairo_pdf, width=4, height=4)
q2


### GPT-2 SURPRISAL ###

setwd("~/Downloads/Master_Thesis/2_Surprisal_Values")
dt <- fread("FollowUp_GPT2_surprisals_edited.csv")
dt <- dt[!(Surprisal_Target %in% NA & Surprisal_Distractor %in% NA),] 

# density plot target (GPT-2)
means <- aggregate(Surprisal_Target ~ Condition, dt, FUN=mean)
means$Plaus_SE <- aggregate(Surprisal_Target ~ Condition, dt, FUN=se)$Surprisal_Target
dt_items_abc2 <- dt[, lapply(.SD, mean), by=list(Item, Condition), .SDcols=c("Surprisal_Target")]

p3 <- ggplot(dt_items_abc2, aes(x=Surprisal_Target, color=Condition, fill=Condition)) + geom_density(alpha=0.4) + theme_minimal() + ylim(0, 0.5)
p3 <- p3 + geom_vline(data=means, aes(xintercept=Surprisal_Target, color=Condition), linetype="dashed") + scale_x_continuous(breaks=seq(0, 20, by = 2))
p3 <- p3 + scale_color_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p3 <- p3 + scale_fill_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p3 <- p3 + labs(title = "Target Surprisal (GPT-2)", y="Density", x= "Surprisal") + theme(legend.position = "none") + theme(plot.title = element_text(hjust = 0.5))
ggsave("DensityPlot_Surprisal_Target.pdf", p3, device=cairo_pdf, width=4, height=4)
p3

# density plot distractor (GPT-2)
means <- aggregate(Surprisal_Distractor ~ Condition, dt, FUN=mean)
means$Plaus_distractor_SE <- aggregate(Surprisal_Distractor ~ Condition, dt, FUN=se)$Surprisal_Distractor
dt_items_d <- dt[, lapply(.SD, mean), by=list(Item, Condition), .SDcols=c("Surprisal_Distractor")]

p4 <- ggplot(dt_items_d, aes(x=Surprisal_Distractor, color=Condition, fill=Condition)) + geom_density(alpha=0.4) + theme_minimal() + ylim(0, 0.5)
p4 <- p4 + geom_vline(data=means, aes(xintercept=Surprisal_Distractor, color=Condition), linetype="dashed") + scale_x_continuous(breaks=seq(0, 22, by = 2))
p4 <- p4 + scale_color_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p4 <- p4 + scale_fill_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p4 <- p4 + labs(title = "Distractor Surprisal (GPT-2)", y = "Density", x="Surprisal") + theme(legend.position = "none") + theme(plot.title = element_text(hjust = 0.5))
ggsave("DensityPlot_Surprisal_Distractor.pdf", p4, device=cairo_pdf, width=4, height=4)
p4


### LeoLM SURPRISAL ###

# density plot target (LeoLM)
means <- aggregate(LeoLM_tar ~ Condition, dt, FUN=mean)
means$Plaus_SE <- aggregate(LeoLM_tar ~ Condition, dt, FUN=se)$LeoLM_tar
dt_items_abc <- dt[, lapply(.SD, mean), by=list(Item, Condition), .SDcols=c("LeoLM_tar")]

p5 <- ggplot(dt_items_abc, aes(x=LeoLM_tar, color=Condition, fill=Condition)) + geom_density(alpha=0.4) + theme_minimal() + ylim(0, 0.5)
p5 <- p5 + geom_vline(data=means, aes(xintercept=LeoLM_tar, color=Condition), linetype="dashed") + scale_x_continuous(breaks=seq(0, 20, by = 2))
p5 <- p5 + scale_color_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p5 <- p5 + scale_fill_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p5 <- p5 + labs(title = "Target Surprisal (LeoLM)", y="Density", x= "Surprisal") + theme(legend.position = "none") + theme(plot.title = element_text(hjust = 0.5))
ggsave("DensityPlot_LeoLM_tar.pdf", p1, device=cairo_pdf, width=4, height=4)
p5

# density plot distractor (LeoLM)
means <- aggregate(LeoLM_dist ~ Condition, dt, FUN=mean)
means$Plaus_distractor_SE <- aggregate(LeoLM_dist ~ Condition, dt, FUN=se)$LeoLM_dist
dt_items_d <- dt[, lapply(.SD, mean), by=list(Item, Condition), .SDcols=c("LeoLM_dist")]

# density plot
p6 <- ggplot(dt_items_d, aes(x=LeoLM_dist, color=Condition, fill=Condition)) + geom_density(alpha=0.4) + theme_minimal() + ylim(0, 0.5)
p6 <- p6 + geom_vline(data=means, aes(xintercept=LeoLM_dist, color=Condition), linetype="dashed") + scale_x_continuous(breaks=seq(0, 24, by = 2))
p6 <- p6 + scale_color_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p6 <- p6 + scale_fill_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue")) + theme(legend.position = "none") + theme(plot.title = element_text(hjust = 0.5))
p6 <- p6 + labs(title = "Distractor Surprisal (LeoLM)", y = "Density", x="Surprisal")
ggsave("DensityPlot_LeoLM_dist.pdf", p1, device=cairo_pdf, width=4, height=4)
p6


### COMBINED PLOT ### 
combined_plot <- grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)  # Replace p1, p2, p3, and p4 with your ggplot objects
setwd("~/Downloads/Master_Thesis/1_Plausibility_Rating_Study/")
dt <- fread("plausresults.csv")

# plot1 with legend
means <- aggregate(Rating ~ Condition, dt, FUN=mean)
means$Plaus_SE <- aggregate(Rating ~ Condition, dt, FUN=se)$Rating
dt_items_abc <- dt[, lapply(.SD, mean), by=list(Item, Condition), .SDcols=c("Rating")]

plot1_legend <- ggplot(dt_items_abc, aes(x=Rating, color=Condition, fill=Condition)) + geom_density(alpha=0.4) + theme_minimal() + ylim(0, 1.5)
plot1_legend <- plot1_legend + geom_vline(data=means, aes(xintercept=Rating, color=Condition), linetype="dashed") + scale_x_continuous(breaks=seq(1,7))
plot1_legend <- plot1_legend + scale_color_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
plot1_legend <- plot1_legend + scale_fill_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
plot1_legend <- plot1_legend + labs(title = "Target Plausibility", y="Density", x= "Rating") + theme(legend.position = "bottom") 

# function to extract legend from plot 
get_only_legend <- function(plot) { 
  plot_table <- ggplot_gtable(ggplot_build(plot)) 
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box") 
  legend <- plot_table$grobs[[legend_plot]] 
  return(legend) 
} 

# extract legend from plot1 using above function 
legend <- get_only_legend(plot1_legend) 
combined_plot_with_legend <- grid.arrange(combined_plot, legend, nrow = 2, heights = c(30, 1))
combined_plot_with_legend

# Save the combined plot
ggsave("Combined_Plot.pdf", combined_plot_with_legend, device = "pdf")

# Attic
# dt[time >= 1634206744 & time < 1634206857,"ID"] <- "Square0"
# dt[time >= 1634206857 & time < 1634206952,"ID"] <- "Square1"
# dt[time >= 1634206952 & time < 1634207069,"ID"] <- "Square2"
# dt[time >= 1634207069 & time < 1634207161,"ID"] <- "Square3"
# dt[time >= 1634207161 & time < 1634207248,"ID"] <- "Square4"
# dt[time >= 1634207248 & time < 1634210796,"ID"] <- "Square5"
# dt[time >= 1634207248 & time < 1634210796,"ID"] <- "Square5"
# dt[time >= 1634211007 & time < 1634211093,"ID"] <- "Nummer3"


# dt <- dt[ID %in% c("Square0", "Square1", "Square2", "Square3", "Square4", "Square5", "Nummer3"),]

