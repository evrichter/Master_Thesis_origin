| - chr_avg_surprisal.r: script Christoph sent to calculate average surprisal values for target    
|                        & distractor in conditions A, B, C
|
| - FollowUp_GPT_surprisal.csv: surprisal values calculated by the language model sent by Benedict,
|                               A (stimulus) = whole item, B (stimulus_tf) = item until  
|                               (including) target/distractor, C (stimulus_tf_sep) = item until  
|                               (including) target/distractor with whitespace, D (GTP2_s =  
|                               surprisal value for target/distractor, E (GTP2_s_sep = surprisal
|                               value for target/distractor with whitespace, G (BPE_split) = 0 or 1
|                                   
| - GP6_FollowUp_Stimuli.csv: csv file I sent to Benedict containing all the stimuli. One item per |                             row = 360 rows (A, B, C, Ad, Bd, Cd x 60)
|
| - FollowUp_GPT2_surprisals_edited.csv: Contains only few columns of GP6_FollowUp_Stimuli.csv, which |                                        are also in more readable format
|
| - Plots.ipynb: Python script used to generate the average surprisal values for target and 
|                distractor in conditions A, B, C and plot them
|
| - Plots.r: Script to create density and bar plots for the target and distractor surprisal values
|
| - surprisal_separated.jpg: Plot displaying surprisal values for punctuation and final word of the |                            context paragraph counted as separated (by whitespace) entities
|
| - surprisal_unseparated.jpg: Plot displaying surprisal values for punctuation and final word of 
|                              the context paragraph counted as one entity
|
| - DensityPlot_Surprisal_Target.pdf: Plot displaying density of average surprisal values per item 
|                                     for each condition (A, B, C)
|
| - DensityPlot_Surprisal_Distractor.pdf: Plot displaying density of average surprisal values per | |                                         item for each condition (A, B, C)
