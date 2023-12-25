setwd("~/Downloads/Master_Thesis/3_SPR_Study/not_log_transformed/")

# exclude participants with low accuracy on comprehension questions
excluded_participants <- c("ffdd30f484d223ac5999ce51deb40e33",
                           "6b1c3b3f4a456ceb42f68dedb8f5b931",
                           "f2ea935ab3f48bf10da6c309500a7647")

#### FUNCTIONS
get_reacts <- function(
    reac_path
) {
    # read in react.txt and compute RTs from it
    dt <- fread(reac_path, fill = TRUE)
    
    dt <-  dt[!(V8 %in% c("Controller-AcceptabilityJudgment")),]
    dt <-  dt[V10 %in% c("Print", "PressedKey"),]
    
    colnames(dt) <- c("timeReceipt", "IPhash", "Controller", "IbexItemNum", "IbexElementNum", "Block", "Group", 
                    "PennElType", "PennElName", "Header", "PressedKey", "EventTime", "Item", "CondCode",
                    "List", "Critical", "CorrectKey", "QuestionText", "QuestionCondition", "Comments", "New1", "New2")
    
    dt <- dt[!(IPhash %in% excluded_participants),]
    
    # Compute reaction times, selecting on IP x Item
    reaction_times <- dt[seq(2, nrow(dt), 2),]$EventTime - dt[seq(1, nrow(dt) - 1, 2),]$EventTime
    dt <- dt[PennElType == "Key",]
    dt$ReactionTime <- reaction_times

    # Compute Accuracy
    dt$Accuracy <- ifelse(dt$PressedKey == dt$CorrectKey, 1, 0)

    # Drop first row of each trials
    dt <- dt[Accuracy %in% c(0,1),]
    # Drop fillers 
    dt <- dt[!(CondCode>250),]
    
    # Map Condition and Block values
    dt$Condition <- ifelse(dt$CondCode == 151, "A", ifelse(dt$CondCode == 152, "B", ifelse(dt$CondCode == 153, "C", "UNKNOWN")))
    dt$Block <- as.integer(ifelse(dt$Block == "block1", 1, ifelse(dt$Block == "block2", 2, ifelse(dt$Block == "block3", 3, "UNKNOWN"))))
    
    # Drop unnneeded columns
    dt <- dt[,c("timeReceipt", "IPhash", "Controller", "Block", "PressedKey", "CorrectKey", "Item", "Condition", "List", "Critical", "QuestionText", "QuestionCondition", "ReactionTime", "Accuracy",  "New1", "New2")]  
    
    dt
}

get_reads <- function(
    filepath
) {
    # read in read.txt and compute RTs from it
    dt <- fread(filepath,header=FALSE)
    
    colnames(dt) <- c("timeReceipt", "IPhash", "Controller", "IbexItemNum", "IbexElementNum", "Block", "Group", 
                    "PennElType", "PennElName", "WordNum", "Word", "EventTime", "Item", "CondCode",
                    "List", "Critical", "Correct", "QuestionText", "QuestionCondition", "ReadingTime", "Newline", "Sentence", "Comments")

    dt <- dt[!(IPhash %in% excluded_participants),] 
    
    dt[,c("Controller", "IbexElementNum", "Group", "PennElType", "PennElName", "Newline", "Comments", "List")] <- NULL
    
    # Add column for trial number (per subject)
    # dt$TrialNum <- dt$IbexItemNum - 15

    # map Block coding
    dt$Block <- as.integer(ifelse(dt$Block == "block1", 1, ifelse(dt$Block == "block2", 2, ifelse(dt$Block == "block3", 3, "UNKNOWN"))))

    # Remove fillers & map cond coding
    dt <- dt[CondCode < 200,]
    dt$Condition <- ifelse(dt$CondCode == 151, "A", ifelse(dt$CondCode == 152, "B", ifelse(dt$CondCode == 153, "C", "UNKNOWN")))

    # Extract regions
    dt$Region <- ifelse(dt$WordNum == dt$Critical, "Critical", 
                    ifelse(dt$WordNum == (dt$Critical+1), "Spillover", 
                    ifelse(dt$WordNum == (dt$Critical+2), "Post-spillover",
                    ifelse(dt$WordNum == (dt$Critical-1), "Pre-critical", 
                    ifelse(dt$WordNum == (dt$Critical-2), "Pre-critical_2", "noncritical")))))

    # dt$Region <- ifelse(dt$WordNum == 1, "First", 
    #                 ifelse(dt$WordNum == 2, "Second", 
    #                 ifelse(dt$WordNum == 3, "Third",
    #                 ifelse(dt$WordNum == 4, "Fourth", 
    #                 ifelse(dt$WordNum == 5, "Fifth", 
    #                 ifelse(dt$WordNum == 6, "Sixth", "noncritical"))))))


    dt <- dt[Region != "noncritical"]

    dt
}

get_demog <- function(
    demogpath
) {
    dt <- fread(demogpath)
    colnames(dt) <- c("timeReceipt", "IPhash", "Controller", "IbexItemNum", "Type", "Group", 
                    "PennElType", "Controller2", "PennElName", "Parameter", "Value", "EventTime", "survey", "Comments")
    
    dt <- dt[!(IPhash %in% excluded_participants),] 
    
    # remove trial start entries
    dt <- dt[Parameter != "_Trial_"]
    dt <- dt[Parameter != "_Header_"]

    df <- dcast(dt, IPhash ~ Parameter, value.var="Value")

    df
}

get_consent <- function(
    consentpath
) {
    dt <- fread(consentpath)
    colnames(dt) <- c("timeReceipt", "IPhash", "Controller", "IbexItemNum", "Type", "Group", 
                    "PennElType", "Controller2", "PennElName", "Parameter", "Value", "EventTime", "survey", "Comments")
    
    dt <- dt[!(IPhash %in% excluded_participants),]

    # remove trial start entries
    dt <- dt[Parameter != "_Trial_"]
    dt <- dt[Parameter != "_Header_"]

    df <- dcast(dt, IPhash ~ Parameter, value.var="Value")
    df
}

get_survey <- function(
    surveypath
) {
    dt <- fread(surveypath)
    colnames(dt) <- c("timeReceipt", "IPhash", "Controller", "IbexItemNum", "Type", "Group", 
                    "PennElType", "Controller2", "PennElName", "Parameter", "Value", "EventTime", "survey", "Comments")
    
    dt <- dt[!(IPhash %in% excluded_participants),] 

    # remove trial start entries
    dt <- dt[Parameter != "_Trial_"]
    dt <- dt[Parameter != "_Header_"]

    df <- dcast(dt, IPhash ~ Parameter, value.var="Value")
    df
}

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

exclude <- function(
    df,
    sd_cutoff = 2 # how many SDs + is acceptable
){
    n <- nrow(df)
    # compute SD per subject and exclude based on that
    for (s in unique(df$Subject)){
        # compute mean + sd per subject
        s_mean <- mean(df[Subject==s,]$ReadingTime)
        s_sd <- sd(df[Subject==s,]$ReadingTime)
        df <- df[!(Subject == s & (ReadingTime > (s_mean + sd_cutoff * s_sd)) | (ReadingTime < (s_mean - sd_cutoff * s_sd))),]
    } 
    percent_excluded <- ((n - nrow(df)) / nrow(df) * 100)
    print(paste("Excluded ", round(percent_excluded,2), "% of data based on per-subject SD."))

    df
}

remove_outliers <- function(
    df
)
{
    n_total <- as.numeric(nrow(df) / 5)
  
    reading_time_outliers <- df[ReadingTime < 50 | ReadingTime > 2500]
    reaction_time_outliers <- df[ReactionTime < 50 | ReactionTime > 10000]
    
    for(i in 1:nrow(reading_time_outliers)) {
      trial_row <- reading_time_outliers[i, ]
      
      trial_item <- trial_row$Item
      trial_condition <- trial_row$Condition
      trial_subject <- trial_row$Subject
      
      df <- df[!(Item == trial_item & Condition == trial_condition & Subject == trial_subject)]
    }
    
    for(i in 1:nrow(reaction_time_outliers)) {
      trial_row <- reaction_time_outliers[i, ]
      
      trial_item <- trial_row$Item
      trial_condition <- trial_row$Condition
      trial_subject <- trial_row$Subject
      
      df <- df[!(Item == trial_item & Condition == trial_condition & Subject == trial_subject)]
    }
    n_after_exclusion <- as.numeric(nrow(df) / 5)
    
    percent_excluded <- (n_total - n_after_exclusion) / n_total * 100
    print(paste((n_total - n_after_exclusion) , " out of ", n_total, " trials were excluded (", round(percent_excluded,2), "% of data based on reading times and reaction times were excluded)"))
    
    df
}

get_plausibility_rating <- function(
    reac_path
){
    dt <- fread(reac_path, fill = TRUE)
    
    plausibility_rating_not_filtered <- dt[(V8 %in% c("Controller-AcceptabilityJudgment")),]
    plausibility_rating <- plausibility_rating_not_filtered[(V11 %in% c(1,2,3,4,5,6,7)),]
    
    colnames(plausibility_rating) <- c("timeReceipt", "IPhash", "Controller", "IbexItemNum", "IbexElementNum", "Block", "Group", 
                                       "PennElType", "PennElName", "Header", "SPR_Plaus_Rating", "EventTime", "Item", "CondCode", 
                                       "List", "Critical", "CorrectKey", "QuestionText", "QuestionCondition", "Comments",
                                       "RatingTime", "Comments2")
    
    plausibility_rating <- plausibility_rating[!(IPhash %in% excluded_participants),]
    
    plausibility_rating <- plausibility_rating[!(CondCode>250),]
    
    plausibility_rating$Condition <- ifelse(plausibility_rating$CondCode == 151, "A", ifelse(plausibility_rating$CondCode == 152, "B", ifelse(plausibility_rating$CondCode == 153, "C", "UNKNOWN")))
    
    plausibility_rating_with_avg <- calculate_avg_plausibility_rating(plausibility_rating)
    
    plausibility_rating <- merge(plausibility_rating, plausibility_rating_with_avg[,c("IPhash", "Item", "Condition", "SPR_Plaus_avg")], by=c("IPhash", "Item", "Condition"), all=TRUE)
    
    plausibility_rating
}

calculate_avg_plausibility_rating <- function(
    plausibility_rating
) 
{
  conditions <- c("A", "B", "C") 
  items <- c(numbers = seq(1, 60))
  
  plausibility_rating$SPR_Plaus_avg <- 0
  
  for (condition in conditions) 
  {
    plausibility_rating_per_condition <- plausibility_rating[(Condition %in% c(condition)),]
    avg_plausrating_per_condition <- round(mean(as.numeric(plausibility_rating_per_condition$SPR_Plaus_Rating)), 2)
    
    for (item in items) 
    {
      plausibility_rating_per_item <-plausibility_rating_per_condition[(Item %in% c(item)),]
      
      SPR_Plaus_avg <- round(mean(as.numeric(plausibility_rating_per_item$SPR_Plaus_Rating)), 2)
      
      cond <- plausibility_rating$Condition == condition & plausibility_rating$Item == item
      
      plausibility_rating$SPR_Plaus_avg[cond] <- SPR_Plaus_avg
    }
    
    #print(paste("Average plausibility rating for condition ", condition, " : ", avg_plausrating_per_condition))
  }
  
  plausibility_rating
}

