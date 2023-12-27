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

# Demog
demog <- fread("demog.txt")
# colnames(demog) <- c("time", "ID", "three", "four", "five", "six", "question", "answer")
colnames(demog) <- c("time", "ID", "three", "four", "five", "six", "seven", "question", "answer")
demog[,c("three", "four", "five", "six")] <- NULL
demog <- dcast(demog, time + ID ~ question, value.var="answer")
demog <- demog[time > 1634211007,] 


# consent 
consent <- fread("consent.txt")
# colnames(consent) <- c("time", "ID", "three", "four", "five", "six", "question", "answer")
colnames(consent) <- c("time", "ID", "three", "four", "five", "six", "seven", "question", "answer")
consent <- consent[time > 1634211007,]

# Ratings
dt <- fread("rating.txt")
# colnames(dt) <- c("time", "ID", "controller", "four", "Condition", "Item", "text", "Rating", "nine", "ReactionTime")
colnames(dt) <- c("time", "ID", "controller", "four", "five", "Condition", "Item", "text", "Rating", "nine", "ReactionTime")
dt[,c("controller", "four", "text", "nine")] <- NULL
dt <- dt[time > 1634211007,]
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
    # print(kripp.alpha(dtlcm, method = "ordinal"))
}

# Probes
correct_probes <- data.table(Item=seq(61,72), Correct=c(1, 7, 7, 1, 1, 7, 1, 1, 7, 1, 7, 7))
probes <- merge(probes, correct_probes, on=Item)
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

# Condition averages
aggregate(Rating ~ Condition, dt, FUN=mean)
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
items[,c("Ratingdist", "ReacitonTimedist")] <- data.frame(condd[order(data.frame(condd)[,1], data.frame(condd)[,2])])[,c("Rating", "ReactionTime")]
fwrite(data.table(items), "plausresults.csv")


# Plaus Data Viz
library(ggplot2)

# (after doing the switcheroo on the conditions / target words)
# dt <- fread("GradedP6_results.csv")
dt <- fread("plausresults.csv")

#condd <- dt_items[, lapply(.SD, mean), by=list(Condition), .SDcols=c("Plaus_distractor")]
#condd <- dt[, lapply(.SD, mean), by=list(Condition), .SDcols=c("Plaus_distractor")]

means <- aggregate(Plaus ~ Condition, dt, FUN=mean)
means$Plaus_SE <- aggregate(Plaus ~ Condition, dt, FUN=se)$Plaus
dt_items_abc <- dt[, lapply(.SD, mean), by=list(Item, Condition), .SDcols=c("Plaus")]

# density plot
p <- ggplot(dt_items_abc, aes(x=Plaus, color=Condition, fill=Condition)) + geom_density(alpha=0.4) + theme_minimal() + xlim(1,7) + ylim(0, 1.5)
p <- p + geom_vline(data=means, aes(xintercept=Plaus, color=Condition), linetype="dashed") + scale_x_continuous(breaks=seq(1,7))
p <- p + scale_color_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p <- p + scale_fill_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p <- p + labs(y="Density", x="Plausibility (Target Word)")
ggsave("../plots/Design2_Plausibility_Target.pdf", p, device=cairo_pdf, width=4, height=4)
p

# barplot
ggplot(means, aes(x=Condition, y=Plaus)) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=Plaus-Plaus_SE, ymax=Plaus+Plaus_SE), width=.4, position=position_dodge(.9)) + theme_minimal() + coord_cartesian(ylim = c(1, 7)) + scale_y_continuous(breaks = c(1:7))


# AdBdCd
means <- aggregate(Plaus_distractor ~ Condition, dt, FUN=mean)
means$Plaus_distractor_SE <- aggregate(Plaus_distractor ~ Condition, dt, FUN=se)$Plaus_distractor
dt_items_d <- dt[, lapply(.SD, mean), by=list(Item, Condition), .SDcols=c("Plaus_distractor")]

# density plot
p <- ggplot(dt_items_d, aes(x=Plaus_distractor, color=Condition, fill=Condition)) + geom_density(alpha=0.4) + theme_minimal() + xlim(1,7)  + ylim(0, 1.5)
p <- p + geom_vline(data=means, aes(xintercept=Plaus_distractor, color=Condition), linetype="dashed") + scale_x_continuous(breaks=seq(1,7))
p <- p + scale_color_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p <- p + scale_fill_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p <- p + labs(y="Density", x="Plausibility (Distractor Word)")
ggsave("../plots/Design2_Plausibility_Distractor.pdf", p, device=cairo_pdf, width=4, height=4)
p

# barplot
ggplot(means, aes(x=Condition, y=Plaus_distractor)) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=Plaus_distractor-Plaus_distractor_SE, ymax=Plaus_distractor+Plaus_distractor_SE), width=.4, position=position_dodge(.9)) + theme_minimal() + coord_cartesian(ylim = c(1, 7)) + scale_y_continuous(breaks = c(1:7))


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

