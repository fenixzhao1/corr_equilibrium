##### Data preparation #####
# load packages
rm(list = ls())
library(here)
library(ggplot2)
library(dplyr)
library(xtable)
library(haven)

full_data = read.csv(here('Data','data_all.csv'))

##### Transition probability matrix #####
# leave only MV data
mv_data = filter(full_data, game=='MV')

# set up matrix
uniquetreatment = unique(mv_data$treatment)
transition_matrix = list()

# loop over treatments
for (i in 1:length(uniquetreatment)){
  
  # build treatment data
  df = filter(mv_data, treatment == uniquetreatment[i])
  pairs = unique(df$session_round_pair_id)
  
  # skip CH treatments
  if(length(df$tick)==0){next}
  
  else{
    # generate the empty matrix
    transition = matrix(0, nrow = 2, ncol = 6)
    rownames(transition) = c('diagonal at t', 'off-diagonal at t')
    colnames(transition) = c('stay at t+1', 'BR at t+1', 'BR*2 at t+1', 'other 1 step', 'one 2 steps', 'obs')
    
    # loop over pairs
    for (j in 1:length(pairs)){
      
      df_pair = filter(df, session_round_pair_id == pairs[j])
      # loop over observations
      for (k in 2:length(df_pair$tick)){
        
        # transitions from diagonal
        if (df_pair$type2[k-1] == 1){
          if (df_pair$type2[k] == 1){transition[1,1] = transition[1,1] + 1}
          else if (df_pair$type2[k] == 2){transition[1,5] = transition[1,5] + 1}
          else if (df_pair$type2[k] == 3){transition[1,5] = transition[1,5] + 1}
          else if (df_pair$type2[k] == 4){transition[1,4] = transition[1,4] + 1}
          else if (df_pair$type2[k] == 5){transition[1,2] = transition[1,2] + 1}
          else if (df_pair$type2[k] == 6){transition[1,3] = transition[1,3] + 1}
          else if (df_pair$type2[k] == 7){transition[1,2] = transition[1,2] + 1}
          else if (df_pair$type2[k] == 8){transition[1,3] = transition[1,3] + 1}
          else{transition[1,4] = transition[1,4] + 1}
        }
        
        if (df_pair$type2[k-1] == 2){
          if (df_pair$type2[k] == 1){transition[1,5] = transition[1,5] + 1}
          else if (df_pair$type2[k] == 2){transition[1,1] = transition[1,1] + 1}
          else if (df_pair$type2[k] == 3){transition[1,5] = transition[1,5] + 1}
          else if (df_pair$type2[k] == 4){transition[1,3] = transition[1,3] + 1}
          else if (df_pair$type2[k] == 5){transition[1,4] = transition[1,4] + 1}
          else if (df_pair$type2[k] == 6){transition[1,2] = transition[1,2] + 1}
          else if (df_pair$type2[k] == 7){transition[1,4] = transition[1,4] + 1}
          else if (df_pair$type2[k] == 8){transition[1,2] = transition[1,2] + 1}
          else{transition[1,3] = transition[1,3] + 1}
        }
        
        if (df_pair$type2[k-1] == 3){
          if (df_pair$type2[k] == 1){transition[1,5] = transition[1,5] + 1}
          else if (df_pair$type2[k] == 2){transition[1,5] = transition[1,5] + 1}
          else if (df_pair$type2[k] == 3){transition[1,1] = transition[1,1] + 1}
          else if (df_pair$type2[k] == 4){transition[1,2] = transition[1,2] + 1}
          else if (df_pair$type2[k] == 5){transition[1,3] = transition[1,3] + 1}
          else if (df_pair$type2[k] == 6){transition[1,4] = transition[1,4] + 1}
          else if (df_pair$type2[k] == 7){transition[1,3] = transition[1,3] + 1}
          else if (df_pair$type2[k] == 8){transition[1,4] = transition[1,4] + 1}
          else{transition[1,2] = transition[1,2] + 1}
        }
        
        # transitions from off-diagonal
        if (df_pair$type2[k-1] == 4){
          if (df_pair$type2[k] == 1){transition[2,4] = transition[2,4] + 1}
          else if (df_pair$type2[k] == 2){transition[2,5] = transition[2,5] + 1}
          else if (df_pair$type2[k] == 3){transition[2,4] = transition[2,4] + 1}
          else if (df_pair$type2[k] == 4){transition[2,1] = transition[2,1] + 1}
          else if (df_pair$type2[k] == 5){transition[2,5] = transition[2,5] + 1}
          else if (df_pair$type2[k] == 6){transition[2,3] = transition[2,3] + 1}
          else if (df_pair$type2[k] == 7){transition[2,2] = transition[2,2] + 1}
          else if (df_pair$type2[k] == 8){transition[2,4] = transition[2,4] + 1}
          else{transition[2,5] = transition[2,5] + 1}
        }
        
        if (df_pair$type2[k-1] == 5){
          if (df_pair$type2[k] == 1){transition[2,4] = transition[2,4] + 1}
          else if (df_pair$type2[k] == 2){transition[2,4] = transition[2,4] + 1}
          else if (df_pair$type2[k] == 3){transition[2,5] = transition[2,5] + 1}
          else if (df_pair$type2[k] == 4){transition[2,3] = transition[2,3] + 1}
          else if (df_pair$type2[k] == 5){transition[2,1] = transition[2,1] + 1}
          else if (df_pair$type2[k] == 6){transition[2,5] = transition[2,5] + 1}
          else if (df_pair$type2[k] == 7){transition[2,5] = transition[2,5] + 1}
          else if (df_pair$type2[k] == 8){transition[2,2] = transition[2,2] + 1}
          else{transition[2,4] = transition[2,4] + 1}
        }
        
        if (df_pair$type2[k-1] == 6){
          if (df_pair$type2[k] == 1){transition[2,5] = transition[2,5] + 1}
          else if (df_pair$type2[k] == 2){transition[2,4] = transition[2,4] + 1}
          else if (df_pair$type2[k] == 3){transition[2,4] = transition[2,4] + 1}
          else if (df_pair$type2[k] == 4){transition[2,5] = transition[2,5] + 1}
          else if (df_pair$type2[k] == 5){transition[2,3] = transition[2,3] + 1}
          else if (df_pair$type2[k] == 6){transition[2,1] = transition[2,1] + 1}
          else if (df_pair$type2[k] == 7){transition[2,4] = transition[2,4] + 1}
          else if (df_pair$type2[k] == 8){transition[2,5] = transition[2,5] + 1}
          else{transition[2,2] = transition[2,2] + 1}
        }
        
        if (df_pair$type2[k-1] == 7){
          if (df_pair$type2[k] == 1){transition[2,4] = transition[2,4] + 1}
          else if (df_pair$type2[k] == 2){transition[2,4] = transition[2,4] + 1}
          else if (df_pair$type2[k] == 3){transition[2,5] = transition[2,5] + 1}
          else if (df_pair$type2[k] == 4){transition[2,4] = transition[2,4] + 1}
          else if (df_pair$type2[k] == 5){transition[2,5] = transition[2,5] + 1}
          else if (df_pair$type2[k] == 6){transition[2,2] = transition[2,2] + 1}
          else if (df_pair$type2[k] == 7){transition[2,1] = transition[2,1] + 1}
          else if (df_pair$type2[k] == 8){transition[2,5] = transition[2,5] + 1}
          else{transition[2,3] = transition[2,3] + 1}
        }
        
        if (df_pair$type2[k-1] == 8){
          if (df_pair$type2[k] == 1){transition[2,5] = transition[2,5] + 1}
          else if (df_pair$type2[k] == 2){transition[2,4] = transition[2,4] + 1}
          else if (df_pair$type2[k] == 3){transition[2,4] = transition[2,4] + 1}
          else if (df_pair$type2[k] == 4){transition[2,2] = transition[2,2] + 1}
          else if (df_pair$type2[k] == 5){transition[2,4] = transition[2,4] + 1}
          else if (df_pair$type2[k] == 6){transition[2,5] = transition[2,5] + 1}
          else if (df_pair$type2[k] == 7){transition[2,3] = transition[2,3] + 1}
          else if (df_pair$type2[k] == 8){transition[2,1] = transition[2,1] + 1}
          else{transition[2,5] = transition[2,5] + 1}
        }
        
        if (df_pair$type2[k-1] == 9){
          if (df_pair$type2[k] == 1){transition[2,4] = transition[2,4] + 1}
          else if (df_pair$type2[k] == 2){transition[2,5] = transition[2,5] + 1}
          else if (df_pair$type2[k] == 3){transition[2,4] = transition[2,4] + 1}
          else if (df_pair$type2[k] == 4){transition[2,5] = transition[2,5] + 1}
          else if (df_pair$type2[k] == 5){transition[2,2] = transition[2,2] + 1}
          else if (df_pair$type2[k] == 6){transition[2,4] = transition[2,4] + 1}
          else if (df_pair$type2[k] == 7){transition[2,5] = transition[2,5] + 1}
          else if (df_pair$type2[k] == 8){transition[2,3] = transition[2,3] + 1}
          else{transition[2,1] = transition[2,1] + 1}
        }
      }
    }
    
    # calculation transition probability matrix
    transition_prob = transition
    for (m in 1:2){
      transition_prob[m,1] = round(transition[m,1] / sum(transition[m,1:5]), 2)
      transition_prob[m,2] = round(transition[m,2] / sum(transition[m,1:5]), 2)
      transition_prob[m,3] = round(transition[m,3] / sum(transition[m,1:5]), 2)
      transition_prob[m,4] = round(transition[m,4] / sum(transition[m,1:5]), 2)
      transition_prob[m,5] = round(transition[m,5] / sum(transition[m,1:5]), 2)
      transition_prob[m,6] = sum(transition[m,1:5])
    }
    
    # update matrix in the list
    transition_matrix[[i]] = transition_prob
  }
}

# data export
xtable(transition_matrix[[1]], digits = 2, caption = as.character(uniquetreatment[1]))
xtable(transition_matrix[[2]], digits = 2, caption = as.character(uniquetreatment[2]))
xtable(transition_matrix[[3]], digits = 2, caption = as.character(uniquetreatment[3]))
xtable(transition_matrix[[4]], digits = 2, caption = as.character(uniquetreatment[4]))