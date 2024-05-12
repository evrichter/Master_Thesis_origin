
library(data.table)
library(dplyr)
library(Kendall)

setwd("~/Downloads/Master_Thesis/3_SPR_Study/Results_SPR_Plaus_single/")

dt <- fread("GP6SPR_processed.csv")

#vars <- dt[, c("Plaus_target_avg", "Plaus_dist_avg", "Surprisal_target", "Surprisal_distractor", "LeoLM_tar", "LeoLM_dist")]
vars <- dt[, c("Plaus_target_avg", "SPR_Plaus_Rating")]

# Correlation Matrix (Pearson Correlation) #exclude single ratings SPR_Plaus_Rating from SPR study in correlation calculations?

correlation_matrix_pearson <- cor(vars)
correlation_matrix_pearson

# Correlation Matrix (Spearman'r rho)
correlation_matrix_spearman <- cor(vars, method = "spearman")
correlation_matrix_spearman

# Correlation Matrix (Kendall's tau)
correlation_matrix_kendall <- cor(vars, method = "kendall")
correlation_matrix_kendall

