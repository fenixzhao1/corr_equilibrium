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
# set up matrix
uniquetreatment = unique(full_data$treatment)
transition_matrix = list()

# loop over treatments
for (i in 1:length(uniquetreatment)){
  
  df = filter(full_data, treatment == uniquetreatment[i])
  pairs = unique(df$session_round_pair_id)
  
  # transitions for BM
  if (df$game[1] == 'BM'){
    
    # generate the empty matrix
    transition = matrix(0, nrow = 4, ncol = 5)
    rownames(transition) = c('UL at t', 'Nash1 at t', 'Nash2 at t', 'Collude at t')
    colnames(transition) = c('UL at t+1', 'Nash1 at t+1', 'Nash2 at t+1', 'Collude at t+1', 'obs')
    
    # loop over pairs
    for (j in 1:length(pairs)){
      
      df_pair = filter(df, session_round_pair_id == pairs[j])
      # loop over observations
      for (k in 2:length(df_pair$tick)){
        
        # add transitions
        if (df_pair$type2[k-1] == 1){
          if (df_pair$type2[k] == 1){transition[1,1] = transition[1,1] + 1}
          if (df_pair$type2[k] == 2){transition[1,2] = transition[1,2] + 1}
          if (df_pair$type2[k] == 3){transition[1,3] = transition[1,3] + 1}
          if (df_pair$type2[k] == 4){transition[1,4] = transition[1,4] + 1}
        }
        if (df_pair$type2[k-1] == 2){
          if (df_pair$type2[k] == 1){transition[2,1] = transition[2,1] + 1}
          if (df_pair$type2[k] == 2){transition[2,2] = transition[2,2] + 1}
          if (df_pair$type2[k] == 3){transition[2,3] = transition[2,3] + 1}
          if (df_pair$type2[k] == 4){transition[2,4] = transition[2,4] + 1}
        }
        if (df_pair$type2[k-1] == 3){
          if (df_pair$type2[k] == 1){transition[3,1] = transition[3,1] + 1}
          if (df_pair$type2[k] == 2){transition[3,2] = transition[3,2] + 1}
          if (df_pair$type2[k] == 3){transition[3,3] = transition[3,3] + 1}
          if (df_pair$type2[k] == 4){transition[3,4] = transition[3,4] + 1}
        }
        if (df_pair$type2[k-1] == 4){
          if (df_pair$type2[k] == 1){transition[4,1] = transition[4,1] + 1}
          if (df_pair$type2[k] == 2){transition[4,2] = transition[4,2] + 1}
          if (df_pair$type2[k] == 3){transition[4,3] = transition[4,3] + 1}
          if (df_pair$type2[k] == 4){transition[4,4] = transition[4,4] + 1}
        }
      }
    }
    
    # calculation transition probability matrix
    transition_prob = transition
    for (m in 1:4){
      transition_prob[m,1] = round(transition[m,1] / sum(transition[m,1:4]), 2)
      transition_prob[m,2] = round(transition[m,2] / sum(transition[m,1:4]), 2)
      transition_prob[m,3] = round(transition[m,3] / sum(transition[m,1:4]), 2)
      transition_prob[m,4] = round(transition[m,4] / sum(transition[m,1:4]), 2)
      transition_prob[m,5] = sum(transition[m,1:4])
    }
    
    # update matrix in the list
    transition_matrix[[i]] = transition_prob
  }
  
  # transitions for MV
  else{
    
    # generate the empty matrix
    transition = matrix(0, nrow = 3, ncol = 5)
    rownames(transition) = c('diagonal at t', 'p1adv at t', 'p2adv at t')
    colnames(transition) = c('stay at t+1', 'diagonal at t+1', 'p1adv at t+1', 'p2adv at t+1', 'obs')
    
    # loop over pairs
    for (j in 1:length(pairs)){
      
      df_pair = filter(df, session_round_pair_id == pairs[j])
      # loop over observations
      for (k in 2:length(df_pair$tick)){
        
        # transitions from diagonal
        if (df_pair$type2[k-1] <= 3){
          if (df_pair$type2[k] == df_pair$type2[k-1]){transition[1,1] = transition[1,1] + 1}
          else if (df_pair$type2[k] <= 3){transition[1,2] = transition[1,2] + 1}
          else if (df_pair$type2[k] >= 7){transition[1,4] = transition[1,4] + 1}
          else{transition[1,3] = transition[1,3] + 1}
        }
        # transitions from p2adv
        else if (df_pair$type2[k-1] >= 7){
          if (df_pair$type2[k] == df_pair$type2[k-1]){transition[3,1] = transition[3,1] + 1}
          else if (df_pair$type2[k] <= 3){transition[3,2] = transition[3,2] + 1}
          else if (df_pair$type2[k] >= 7){transition[3,4] = transition[3,4] + 1}
          else{transition[3,3] = transition[3,3] + 1}
        }
        # transitions from p1 adv
        else{
          if (df_pair$type2[k] == df_pair$type2[k-1]){transition[2,1] = transition[2,1] + 1}
          else if (df_pair$type2[k] <= 3){transition[2,2] = transition[2,2] + 1}
          else if (df_pair$type2[k] >= 7){transition[2,4] = transition[2,4] + 1}
          else{transition[2,3] = transition[2,3] + 1}
        }
      }
    }
    
    # calculation transition probability matrix
    transition_prob = transition
    for (m in 1:3){
      transition_prob[m,1] = round(transition[m,1] / sum(transition[m,1:4]), 2)
      transition_prob[m,2] = round(transition[m,2] / sum(transition[m,1:4]), 2)
      transition_prob[m,3] = round(transition[m,3] / sum(transition[m,1:4]), 2)
      transition_prob[m,4] = round(transition[m,4] / sum(transition[m,1:4]), 2)
      transition_prob[m,5] = sum(transition[m,1:4])
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
xtable(transition_matrix[[5]], digits = 2, caption = as.character(uniquetreatment[5]))
xtable(transition_matrix[[6]], digits = 2, caption = as.character(uniquetreatment[6]))
xtable(transition_matrix[[7]], digits = 2, caption = as.character(uniquetreatment[7]))
xtable(transition_matrix[[8]], digits = 2, caption = as.character(uniquetreatment[8]))