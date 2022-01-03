##### Load packages #####
# add package
rm(list = ls())
library(ggplot2)
library(xtable)
library(dplyr)
library(here)

# import CH action dataset
dfch<-read.csv(here("Data/sim_ch.csv"), header=T, stringsAsFactors = FALSE)
dfch$treatment = paste(dfch$regret, dfch$response, sep = ' ')

# add CH action profile type variable
dfch = dfch %>% mutate(
  type = ifelse(is_11==1, 1, ifelse(is_12==1, 2, ifelse(is_21==1, 3, 4)))
)

# import MV action dataset
dfmv<-read.csv(here("Data/sim_mv.csv"), header=T, stringsAsFactors = FALSE)
dfmv$treatment = paste(dfmv$regret, dfmv$response, sep = ' ')

# add MV action profile type variable
dfmv = dfmv %>% mutate(
  type = ifelse(is_11==1, 1, ifelse(is_22==1, 2, ifelse(is_33==1, 3, ifelse(is_13==1, 4, 
         ifelse(is_21==1, 5, ifelse(is_32==1, 6, ifelse(is_12==1, 7, ifelse(is_23==1, 8, 9))))))))
)


##### CH game transition prob matrix #####
# set up treatment list
uniquetreatment = unique(dfch$treatment)

# set up transition matrix container
trans = list()
transprob = list()

# loop over treatments
for (i in 1:length(uniquetreatment)){
  df = filter(dfch, treatment == uniquetreatment[i])
  
  # set up transition matrix
  trans[[i]] = matrix(0, nrow = 4, ncol = 5)
  rownames(trans[[i]]) = c('(U,L) at t', 'p1 NE at t', 'p2 NE at t', 'collusion at t')
  colnames(trans[[i]]) = c('(U,L) at t+1', 'p1 NE at t+1', 'p2 NE at t+1', 'collusion at t+1', 'obs')
  
  # loop over simulations
  for (j in 1:max(df$sim)){
    df_pair = filter(df, sim==j)
    
    # loop over observations
    for (k in 2:length(df_pair$period)){
      
      # transitions from (U,L)
      if (df_pair$type[k-1] == 1){
        if (df_pair$type[k] == 1){trans[[i]][1,1] = trans[[i]][1,1] + 1}
        else if (df_pair$type[k] == 2){trans[[i]][1,2] = trans[[i]][1,2] + 1}
        else if (df_pair$type[k] == 3){trans[[i]][1,3] = trans[[i]][1,3] + 1}
        else{trans[[i]][1,4] = trans[[i]][1,4] + 1}
      }
      
      # transitions from NE
      if (df_pair$type[k-1] == 2){
        if (df_pair$type[k] == 1){trans[[i]][2,1] = trans[[i]][2,1] + 1}
        else if (df_pair$type[k] == 2){trans[[i]][2,2] = trans[[i]][2,2] + 1}
        else if (df_pair$type[k] == 3){trans[[i]][2,3] = trans[[i]][2,3] + 1}
        else{trans[[i]][2,4] = trans[[i]][2,4] + 1}
      }
      
      if (df_pair$type[k-1] == 3){
        if (df_pair$type[k] == 1){trans[[i]][3,1] = trans[[i]][3,1] + 1}
        else if (df_pair$type[k] == 2){trans[[i]][3,2] = trans[[i]][3,2] + 1}
        else if (df_pair$type[k] == 3){trans[[i]][3,3] = trans[[i]][3,3] + 1}
        else{trans[[i]][3,4] = trans[[i]][3,4] + 1}
      }
      
      # transitions from collusion
      if (df_pair$type[k-1] == 4){
        if (df_pair$type[k] == 1){trans[[i]][4,1] = trans[[i]][4,1] + 1}
        else if (df_pair$type[k] == 2){trans[[i]][4,2] = trans[[i]][4,2] + 1}
        else if (df_pair$type[k] == 3){trans[[i]][4,3] = trans[[i]][4,3] + 1}
        else{trans[[i]][4,4] = trans[[i]][4,4] + 1}
      }
    }
  }
  
  # calculation transition probability matrix
  transprob[[i]] = trans[[i]]
  for (m in 1:4){
    transprob[[i]][m,1] = trans[[i]][m,1] / sum(trans[[i]][m,1:4])
    transprob[[i]][m,2] = trans[[i]][m,2] / sum(trans[[i]][m,1:4])
    transprob[[i]][m,3] = trans[[i]][m,3] / sum(trans[[i]][m,1:4])
    transprob[[i]][m,4] = trans[[i]][m,4] / sum(trans[[i]][m,1:4])
    transprob[[i]][m,5] = sum(trans[[i]][m,1:4])
  }
  
  # data export
  print(xtable(transprob[[i]], digits = 3, caption = uniquetreatment[[i]]))
}

rm(df, df_pair, trans, transprob)


##### MV game transition prob matrix - BR cycles #####
# set up treatment list
uniquetreatment = unique(dfmv$treatment)

# set up transition matrix container
trans = list()
transprob = list()

# loop over treatments
for (i in 1:length(uniquetreatment)){
  df = filter(dfmv, treatment == uniquetreatment[i])
  
  # set up transition matrix
  trans[[i]] = matrix(0, nrow = 2, ncol = 6)
  rownames(trans[[i]]) = c('diagonal at t', 'off-diagonal at t')
  colnames(trans[[i]]) = c('stay at t+1', 'BR at t+1', 'BR*2 at t+1', 'other 1 step', 'one 2 steps', 'obs')
  
  # loop over simulations
  for (j in 1:max(df$sim)){
    df_pair = filter(df, sim==j)
    
    # loop over observations
    for (k in 2:length(df_pair$period)){
      
      # transitions from diagonal
      if (df_pair$type[k-1] == 1){
        if (df_pair$type[k] == 1){trans[[i]][1,1] = trans[[i]][1,1] + 1}
        else if (df_pair$type[k] == 2){trans[[i]][1,5] = trans[[i]][1,5] + 1}
        else if (df_pair$type[k] == 3){trans[[i]][1,5] = trans[[i]][1,5] + 1}
        else if (df_pair$type[k] == 4){trans[[i]][1,4] = trans[[i]][1,4] + 1}
        else if (df_pair$type[k] == 5){trans[[i]][1,2] = trans[[i]][1,2] + 1}
        else if (df_pair$type[k] == 6){trans[[i]][1,3] = trans[[i]][1,3] + 1}
        else if (df_pair$type[k] == 7){trans[[i]][1,2] = trans[[i]][1,2] + 1}
        else if (df_pair$type[k] == 8){trans[[i]][1,3] = trans[[i]][1,3] + 1}
        else{trans[[i]][1,4] = trans[[i]][1,4] + 1}
      }
      
      if (df_pair$type[k-1] == 2){
        if (df_pair$type[k] == 1){trans[[i]][1,5] = trans[[i]][1,5] + 1}
        else if (df_pair$type[k] == 2){trans[[i]][1,1] = trans[[i]][1,1] + 1}
        else if (df_pair$type[k] == 3){trans[[i]][1,5] = trans[[i]][1,5] + 1}
        else if (df_pair$type[k] == 4){trans[[i]][1,3] = trans[[i]][1,3] + 1}
        else if (df_pair$type[k] == 5){trans[[i]][1,4] = trans[[i]][1,4] + 1}
        else if (df_pair$type[k] == 6){trans[[i]][1,2] = trans[[i]][1,2] + 1}
        else if (df_pair$type[k] == 7){trans[[i]][1,4] = trans[[i]][1,4] + 1}
        else if (df_pair$type[k] == 8){trans[[i]][1,2] = trans[[i]][1,2] + 1}
        else{trans[[i]][1,3] = trans[[i]][1,3] + 1}
      }
      
      if (df_pair$type[k-1] == 3){
        if (df_pair$type[k] == 1){trans[[i]][1,5] = trans[[i]][1,5] + 1}
        else if (df_pair$type[k] == 2){trans[[i]][1,5] = trans[[i]][1,5] + 1}
        else if (df_pair$type[k] == 3){trans[[i]][1,1] = trans[[i]][1,1] + 1}
        else if (df_pair$type[k] == 4){trans[[i]][1,2] = trans[[i]][1,2] + 1}
        else if (df_pair$type[k] == 5){trans[[i]][1,3] = trans[[i]][1,3] + 1}
        else if (df_pair$type[k] == 6){trans[[i]][1,4] = trans[[i]][1,4] + 1}
        else if (df_pair$type[k] == 7){trans[[i]][1,3] = trans[[i]][1,3] + 1}
        else if (df_pair$type[k] == 8){trans[[i]][1,4] = trans[[i]][1,4] + 1}
        else{trans[[i]][1,2] = trans[[i]][1,2] + 1}
      }
      
      # transitions from off-diagonal
      if (df_pair$type[k-1] == 4){
        if (df_pair$type[k] == 1){trans[[i]][2,4] = trans[[i]][2,4] + 1}
        else if (df_pair$type[k] == 2){trans[[i]][2,5] = trans[[i]][2,5] + 1}
        else if (df_pair$type[k] == 3){trans[[i]][2,4] = trans[[i]][2,4] + 1}
        else if (df_pair$type[k] == 4){trans[[i]][2,1] = trans[[i]][2,1] + 1}
        else if (df_pair$type[k] == 5){trans[[i]][2,5] = trans[[i]][2,5] + 1}
        else if (df_pair$type[k] == 6){trans[[i]][2,3] = trans[[i]][2,3] + 1}
        else if (df_pair$type[k] == 7){trans[[i]][2,2] = trans[[i]][2,2] + 1}
        else if (df_pair$type[k] == 8){trans[[i]][2,4] = trans[[i]][2,4] + 1}
        else{trans[[i]][2,5] = trans[[i]][2,5] + 1}
      }
      
      if (df_pair$type[k-1] == 5){
        if (df_pair$type[k] == 1){trans[[i]][2,4] = trans[[i]][2,4] + 1}
        else if (df_pair$type[k] == 2){trans[[i]][2,4] = trans[[i]][2,4] + 1}
        else if (df_pair$type[k] == 3){trans[[i]][2,5] = trans[[i]][2,5] + 1}
        else if (df_pair$type[k] == 4){trans[[i]][2,3] = trans[[i]][2,3] + 1}
        else if (df_pair$type[k] == 5){trans[[i]][2,1] = trans[[i]][2,1] + 1}
        else if (df_pair$type[k] == 6){trans[[i]][2,5] = trans[[i]][2,5] + 1}
        else if (df_pair$type[k] == 7){trans[[i]][2,5] = trans[[i]][2,5] + 1}
        else if (df_pair$type[k] == 8){trans[[i]][2,2] = trans[[i]][2,2] + 1}
        else{trans[[i]][2,4] = trans[[i]][2,4] + 1}
      }
      
      if (df_pair$type[k-1] == 6){
        if (df_pair$type[k] == 1){trans[[i]][2,5] = trans[[i]][2,5] + 1}
        else if (df_pair$type[k] == 2){trans[[i]][2,4] = trans[[i]][2,4] + 1}
        else if (df_pair$type[k] == 3){trans[[i]][2,4] = trans[[i]][2,4] + 1}
        else if (df_pair$type[k] == 4){trans[[i]][2,5] = trans[[i]][2,5] + 1}
        else if (df_pair$type[k] == 5){trans[[i]][2,3] = trans[[i]][2,3] + 1}
        else if (df_pair$type[k] == 6){trans[[i]][2,1] = trans[[i]][2,1] + 1}
        else if (df_pair$type[k] == 7){trans[[i]][2,4] = trans[[i]][2,4] + 1}
        else if (df_pair$type[k] == 8){trans[[i]][2,5] = trans[[i]][2,5] + 1}
        else{trans[[i]][2,2] = trans[[i]][2,2] + 1}
      }
      
      if (df_pair$type[k-1] == 7){
        if (df_pair$type[k] == 1){trans[[i]][2,4] = trans[[i]][2,4] + 1}
        else if (df_pair$type[k] == 2){trans[[i]][2,4] = trans[[i]][2,4] + 1}
        else if (df_pair$type[k] == 3){trans[[i]][2,5] = trans[[i]][2,5] + 1}
        else if (df_pair$type[k] == 4){trans[[i]][2,4] = trans[[i]][2,4] + 1}
        else if (df_pair$type[k] == 5){trans[[i]][2,5] = trans[[i]][2,5] + 1}
        else if (df_pair$type[k] == 6){trans[[i]][2,2] = trans[[i]][2,2] + 1}
        else if (df_pair$type[k] == 7){trans[[i]][2,1] = trans[[i]][2,1] + 1}
        else if (df_pair$type[k] == 8){trans[[i]][2,5] = trans[[i]][2,5] + 1}
        else{trans[[i]][2,3] = trans[[i]][2,3] + 1}
      }
      
      if (df_pair$type[k-1] == 8){
        if (df_pair$type[k] == 1){trans[[i]][2,5] = trans[[i]][2,5] + 1}
        else if (df_pair$type[k] == 2){trans[[i]][2,4] = trans[[i]][2,4] + 1}
        else if (df_pair$type[k] == 3){trans[[i]][2,4] = trans[[i]][2,4] + 1}
        else if (df_pair$type[k] == 4){trans[[i]][2,2] = trans[[i]][2,2] + 1}
        else if (df_pair$type[k] == 5){trans[[i]][2,4] = trans[[i]][2,4] + 1}
        else if (df_pair$type[k] == 6){trans[[i]][2,5] = trans[[i]][2,5] + 1}
        else if (df_pair$type[k] == 7){trans[[i]][2,3] = trans[[i]][2,3] + 1}
        else if (df_pair$type[k] == 8){trans[[i]][2,1] = trans[[i]][2,1] + 1}
        else{trans[[i]][2,5] = trans[[i]][2,5] + 1}
      }
      
      if (df_pair$type[k-1] == 9){
        if (df_pair$type[k] == 1){trans[[i]][2,4] = trans[[i]][2,4] + 1}
        else if (df_pair$type[k] == 2){trans[[i]][2,5] = trans[[i]][2,5] + 1}
        else if (df_pair$type[k] == 3){trans[[i]][2,4] = trans[[i]][2,4] + 1}
        else if (df_pair$type[k] == 4){trans[[i]][2,5] = trans[[i]][2,5] + 1}
        else if (df_pair$type[k] == 5){trans[[i]][2,2] = trans[[i]][2,2] + 1}
        else if (df_pair$type[k] == 6){trans[[i]][2,4] = trans[[i]][2,4] + 1}
        else if (df_pair$type[k] == 7){trans[[i]][2,5] = trans[[i]][2,5] + 1}
        else if (df_pair$type[k] == 8){trans[[i]][2,3] = trans[[i]][2,3] + 1}
        else{trans[[i]][2,1] = trans[[i]][2,1] + 1}
      }
    }
  }
  
  # calculation transition probability matrix
  transprob[[i]] = trans[[i]]
  for (m in 1:2){
    transprob[[i]][m,1] = trans[[i]][m,1] / sum(trans[[i]][m,1:5])
    transprob[[i]][m,2] = trans[[i]][m,2] / sum(trans[[i]][m,1:5])
    transprob[[i]][m,3] = trans[[i]][m,3] / sum(trans[[i]][m,1:5])
    transprob[[i]][m,4] = trans[[i]][m,4] / sum(trans[[i]][m,1:5])
    transprob[[i]][m,5] = trans[[i]][m,5] / sum(trans[[i]][m,1:5])
    transprob[[i]][m,6] = sum(trans[[i]][m,1:5])
  }

  # data export
  print(xtable(transprob[[i]], digits = 3, caption = uniquetreatment[[i]]))
}

rm(df, df_pair, trans, transprob)


##### MV game transition prob matrix - between types #####
# set up treatment list
uniquetreatment = unique(dfmv$treatment)

# set up transition matrix container
trans = list()
transprob = list()

# loop over treatments
for (i in 1:length(uniquetreatment)){
  df = filter(dfmv, treatment == uniquetreatment[i])
  
  # set up the transition matrix
  trans[[i]] = matrix(0, nrow = 3, ncol = 5)
  rownames(trans[[i]]) = c('diagonal at t', 'p1adv at t', 'p2adv at t')
  colnames(trans[[i]]) = c('stay at t+1', 'diagonal at t+1', 'p1adv at t+1', 'p2adv at t+1', 'obs')
  
  # loop over simulations
  for (j in 1:max(df$sim)){
    df_sim = filter(df, sim==j)
    
    # loop over observations
    for (k in 2:length(df_sim$period)){
      
      # transitions from diagonal
      if (df_sim$type[k-1] <= 3){
        if (df_sim$type[k] == df_sim$type[k-1]){trans[[i]][1,1] = trans[[i]][1,1] + 1}
        else if (df_sim$type[k] <= 3){trans[[i]][1,2] = trans[[i]][1,2] + 1}
        else if (df_sim$type[k] >= 7){trans[[i]][1,4] = trans[[i]][1,4] + 1}
        else{trans[[i]][1,3] = trans[[i]][1,3] + 1}
      }
      # transitions from p2adv
      else if (df_sim$type[k-1] >= 7){
        if (df_sim$type[k] == df_sim$type[k-1]){trans[[i]][3,1] = trans[[i]][3,1] + 1}
        else if (df_sim$type[k] <= 3){trans[[i]][3,2] = trans[[i]][3,2] + 1}
        else if (df_sim$type[k] >= 7){trans[[i]][3,4] = trans[[i]][3,4] + 1}
        else{trans[[i]][3,3] = trans[[i]][3,3] + 1}
      }
      # transitions from p1 adv
      else{
        if (df_sim$type[k] == df_sim$type[k-1]){trans[[i]][2,1] = trans[[i]][2,1] + 1}
        else if (df_sim$type[k] <= 3){trans[[i]][2,2] = trans[[i]][2,2] + 1}
        else if (df_sim$type[k] >= 7){trans[[i]][2,4] = trans[[i]][2,4] + 1}
        else{trans[[i]][2,3] = trans[[i]][2,3] + 1}
      }
    }
  }
  
  # calculation transition probability matrix
  transprob[[i]] = trans[[i]]
  for (b in 1:3){
    transprob[[i]][b,1] = trans[[i]][b,1] / sum(trans[[i]][b,1:4])
    transprob[[i]][b,2] = trans[[i]][b,2] / sum(trans[[i]][b,1:4])
    transprob[[i]][b,3] = trans[[i]][b,3] / sum(trans[[i]][b,1:4])
    transprob[[i]][b,4] = trans[[i]][b,4] / sum(trans[[i]][b,1:4])
    transprob[[i]][b,5] = sum(trans[[i]][b,1:4])
  }
  
  # data export
  print(xtable(transprob[[i]], digits = 3, caption = uniquetreatment[[i]]))
}

rm(df, df_sim, trans, transprob)
