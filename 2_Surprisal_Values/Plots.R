# Plaus Data Viz
library(ggplot2)
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

setwd("~/Downloads/Master_Thesis/2_Surprisal_Values/")
dt <- fread("FollowUp_GPT2_surprisals_edited.csv")
dt <- dt[!(Surprisal_Target %in% NA & Surprisal_Distractor %in% NA),]

### TARGET SURPRISAL ###
# ABC
means <- aggregate(Surprisal_Target ~ Condition, dt, FUN=mean)
means$Plaus_SE <- aggregate(Surprisal_Target ~ Condition, dt, FUN=se)$Surprisal_Target
dt_items_abc <- dt[, lapply(.SD, mean), by=list(Item, Condition), .SDcols=c("Surprisal_Target")]

# density plot
p1 <- ggplot(dt_items_abc, aes(x=Surprisal_Target, color=Condition, fill=Condition)) + geom_density(alpha=0.4) + theme_minimal() + ylim(0, 0.5)
p1 <- p1 + geom_vline(data=means, aes(xintercept=Surprisal_Target, color=Condition), linetype="dashed") + scale_x_continuous(breaks=seq(0, 20, by = 2))
p1 <- p1 + scale_color_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p1 <- p1 + scale_fill_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p1 <- p1 + labs(title = "Target Surprisal", y="Density", x= "Surprisal") 
ggsave("DensityPlot_Surprisal_Target.pdf", p1, device=cairo_pdf, width=4, height=4)
p1

# barplot
q1 <- ggplot(means, aes(x=Condition, y=Surprisal_Target)) + geom_bar(stat="identity") + labs(title = "Average Surprisal Values per Condition (Target)", y = "Surprisal", x = "Condition") + geom_errorbar(aes(ymin=Surprisal_Target-Plaus_SE, ymax=Surprisal_Target+Plaus_SE), width=.4, position=position_dodge(.9)) + theme_minimal() + coord_cartesian(ylim = c(1, 8)) + scale_y_continuous(breaks = c(1:8))
ggsave("BarPlot_Surprisal_Target.pdf", q1, device=cairo_pdf, width=4, height=4)
q1



### DISTRACTOR SURPRISAL ###
# AdBdCd
means <- aggregate(Surprisal_Distractor ~ Condition, dt, FUN=mean)
means$Plaus_distractor_SE <- aggregate(Surprisal_Distractor ~ Condition, dt, FUN=se)$Surprisal_Distractor
dt_items_d <- dt[, lapply(.SD, mean), by=list(Item, Condition), .SDcols=c("Surprisal_Distractor")]

# density plot
p1 <- ggplot(dt_items_d, aes(x=Surprisal_Distractor, color=Condition, fill=Condition)) + geom_density(alpha=0.4) + theme_minimal() + ylim(0, 0.5)
p1 <- p1 + geom_vline(data=means, aes(xintercept=Surprisal_Distractor, color=Condition), linetype="dashed") + scale_x_continuous(breaks=seq(0, 25, by = 2))
p1 <- p1 + scale_color_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p1 <- p1 + scale_fill_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p1 <- p1 + labs(title = "Distractor Surprisal", y = "Density", x="Surprisal")
ggsave("DensityPlot_Surprisal_Distractor.pdf", p1, device=cairo_pdf, width=4, height=4)
p1

# barplot
q1 <- ggplot(means, aes(x=Condition, y=Surprisal_Distractor)) + geom_bar(stat="identity") + labs(title = "Average Surprisal Values per Condition (Distractor)", y = "Surprisal", x = "Condition") + geom_errorbar(aes(ymin=Surprisal_Distractor-Plaus_distractor_SE, ymax=Surprisal_Distractor+Plaus_distractor_SE), width=.4, position=position_dodge(.9)) + theme_minimal() + coord_cartesian(ylim = c(1, 8)) + scale_y_continuous(breaks = c(1:8))
ggsave("BarPlot_Surprisal_Distractor.pdf", q1, device=cairo_pdf, width=4, height=4)
q1

