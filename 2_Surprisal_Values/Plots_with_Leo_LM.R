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
dt <- fread("GradedP6_FollowUpStudy_Pretests.csv")
dt <- dt[!(LeoLM_tar %in% NA & LeoLM_dist %in% NA),]

### TARGET SURPRISAL ###
# ABC
means <- aggregate(LeoLM_tar ~ Condition, dt, FUN=mean)
means$Plaus_SE <- aggregate(LeoLM_tar ~ Condition, dt, FUN=se)$LeoLM_tar
dt_items_abc <- dt[, lapply(.SD, mean), by=list(Item, Condition), .SDcols=c("LeoLM_tar")]

# density plot
p1 <- ggplot(dt_items_abc, aes(x=LeoLM_tar, color=Condition, fill=Condition)) + geom_density(alpha=0.4) + theme_minimal() + ylim(0, 0.5)
p1 <- p1 + geom_vline(data=means, aes(xintercept=LeoLM_tar, color=Condition), linetype="dashed") + scale_x_continuous(breaks=seq(0, 20, by = 2))
p1 <- p1 + scale_color_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p1 <- p1 + scale_fill_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p1 <- p1 + labs(title = "Target Surprisal", y="Density", x= "Surprisal") 
ggsave("DensityPlot_LeoLM_tar.pdf", p1, device=cairo_pdf, width=4, height=4)
p1

# barplot
q1 <- ggplot(means, aes(x=Condition, y=LeoLM_tar)) + geom_bar(stat="identity") + labs(title = "Average Surprisal Values per Condition (Target)", y = "Surprisal", x = "Condition") + geom_errorbar(aes(ymin=LeoLM_tar-Plaus_SE, ymax=LeoLM_tar+Plaus_SE), width=.4, position=position_dodge(.9)) + theme_minimal() + coord_cartesian(ylim = c(1, 8)) + scale_y_continuous(breaks = c(1:8))
ggsave("BarPlot_LeoLM_tar.pdf", q1, device=cairo_pdf, width=4, height=4)
q1



### DISTRACTOR SURPRISAL ###
# AdBdCd
means <- aggregate(LeoLM_dist ~ Condition, dt, FUN=mean)
means$Plaus_distractor_SE <- aggregate(LeoLM_dist ~ Condition, dt, FUN=se)$LeoLM_dist
dt_items_d <- dt[, lapply(.SD, mean), by=list(Item, Condition), .SDcols=c("LeoLM_dist")]

# density plot
p1 <- ggplot(dt_items_d, aes(x=LeoLM_dist, color=Condition, fill=Condition)) + geom_density(alpha=0.4) + theme_minimal() + ylim(0, 0.5)
p1 <- p1 + geom_vline(data=means, aes(xintercept=LeoLM_dist, color=Condition), linetype="dashed") + scale_x_continuous(breaks=seq(0, 25, by = 2))
p1 <- p1 + scale_color_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p1 <- p1 + scale_fill_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p1 <- p1 + labs(title = "Distractor Surprisal", y = "Density", x="Surprisal")
ggsave("DensityPlot_LeoLM_dist.pdf", p1, device=cairo_pdf, width=4, height=4)
p1

# barplot
q1 <- ggplot(means, aes(x=Condition, y=LeoLM_dist)) + geom_bar(stat="identity") + labs(title = "Average Surprisal Values per Condition (Distractor)", y = "Surprisal", x = "Condition") + geom_errorbar(aes(ymin=LeoLM_dist-Plaus_distractor_SE, ymax=LeoLM_dist+Plaus_distractor_SE), width=.4, position=position_dodge(.9)) + theme_minimal() + coord_cartesian(ylim = c(1, 8)) + scale_y_continuous(breaks = c(1:8))
ggsave("BarPlot_LeoLM_dist.pdf", q1, device=cairo_pdf, width=4, height=4)
q1

