#### FUNCTIONS
get_reacts <- function(
    reac_path
) {
    # read in react.txt and compute RTs from it
    dt <- fread(reac_path)
    dt <-  dt[V9 %in% c("Print", "PressedKey"),]

    colnames(dt) <- c("timeReceipt", "IPhash", "IbexItemNum", "IbexElementNum", "Block", "Group", 
                    "PennElType", "PennElName", "Header", "PressedKey", "EventTime", "Item", "CondCode",
                    "List", "Critical", "CorrectKey", "QuestionText", "QuestionCondition", "Comments")    
    
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
    dt <- dt[,c("timeReceipt", "IPhash", "Block", "PressedKey", "CorrectKey", "Item", "Condition", "List", "Critical", "QuestionText", "QuestionCondition", "ReactionTime", "Accuracy")]  
    
    dt <- dt[!(IPhash %in% c("c6c58a4e2da5ed0040e8e1fbcf513083")),]

    dt
}

get_reads <- function(
    filepath
) {
    # read in read.txt and compute RTs from it
    dt <- fread(filepath,header=FALSE)
    colnames(dt) <- c("timeReceipt", "IPhash", "IbexItemNum", "IbexElementNum", "Block", "Group", 
                    "PennElType", "PennElName", "WordNum", "Word", "EventTime", "Item", "CondCode",
                    "List", "Critical", "Correct", "QuestionText", "QuestionCondition", "ReadingTime", "Newline", "Sentence", "Comments")

    dt[,c("IbexElementNum", "Group", "PennElType", "PennElName", "Newline", "Comments", "List")] <- NULL
    
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

    dt <- dt[!(IPhash %in% c("c6c58a4e2da5ed0040e8e1fbcf513083")),]

    dt
}

get_demog <- function(
    demogpath
) {
    dt <- fread(demogpath)
    colnames(dt) <- c("timeReceipt", "IPhash", "Controller", "IbexItemNum", "Type", "Group", 
                    "PennElType", "PennElName", "Parameter", "Value", "EventTime", "survey", "Comments")

    # remove trial start entries
    dt <- dt[Parameter != "_Trial_"]
    dt <- dt[Parameter != "_Header_"]

    df <- dcast(dt, IPhash ~ Parameter, value.var="Value")

    df <- df[!(IPhash %in% c("c6c58a4e2da5ed0040e8e1fbcf513083")),]
    df
}

get_consent <- function(
    consentpath
) {
    dt <- fread(consentpath)
    colnames(dt) <- c("timeReceipt", "IPhash", "Controller", "IbexItemNum", "Type", "Group", 
                    "PennElType", "PennElName", "Parameter", "Value", "EventTime", "survey", "Comments")

    # remove trial start entries
    dt <- dt[Parameter != "_Trial_"]
    dt <- dt[Parameter != "_Header_"]

    df <- dcast(dt, IPhash ~ Parameter, value.var="Value")
    df <- df[!(IPhash %in% c("c6c58a4e2da5ed0040e8e1fbcf513083")),]
    df
}

get_survey <- function(
    surveypath
) {
    dt <- fread(surveypath)
    colnames(dt) <- c("timeReceipt", "IPhash", "Controller", "IbexItemNum", "Type", "Group", 
                    "PennElType", "PennElName", "Parameter", "Value", "EventTime", "survey", "Comments")

    # remove trial start entries
    dt <- dt[Parameter != "_Trial_"]
    dt <- dt[Parameter != "_Header_"]

    df <- dcast(dt, IPhash ~ Parameter, value.var="Value")
    df <- df[!(IPhash %in% c("c6c58a4e2da5ed0040e8e1fbcf513083")),]
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
