library(data.table)
setwd("~/Downloads/Master_Thesis/2_Surprisal_Values")

dt <- fread("eva_leo13b_surprisals.csv")

# Add Condition and TarDist columns
dt$Condition <- rep(c("A", "A", "B", "B", "C", "C"), 60)
dt$TarDist <- rep(c("Target", "Distractor"), 180)

# average surprisal per condition
#mean_condition_target <- aggregate(GPT2_s ~ Condition + TarDist, dt, mean)
mean_condition <- aggregate(leo13b_s ~ Condition + TarDist, dt, mean)
mean_condition

# standard deviations per condition
sd_condition <- aggregate(leo13b_s ~ Condition  + TarDist, data = dt, FUN = "sd")
sd_condition

# ranges per condition
range_condition <- aggregate(leo13b_s ~ Condition  + TarDist, data = dt, FUN = "range")
range_condition
