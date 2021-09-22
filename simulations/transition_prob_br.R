##### Run MV simulations and build dataset #####
rm(list = ls())
library(ggplot2)
library(xtable)
library(dplyr)
library(here)
source(here("simulations/MVregrets.R"))

# set up the parameters for the simulations
mu = 1000 # HM2000 probability parameter
n = 500 # number of periods in each simulation
sim = 500 # number of simulations
experiment = 100 # number of experimentation periods where players randomly make decisions
pay_MV = matrix(c(0,200,100,100,0,200,200,100,0),3,3) # payoff matrix 3x3

# set up regret parameters
beta = 1.64
Delta = 0.85

# run the simulations
for (s in 1:sim){
  
  # set up the vectors for choices and game parameters
  history_p1 = rep(0, n)
  history_p2 = rep(0, n)
  
  # calculate the experimentation periods with random starting decisions
  history_p1[1:experiment] = sample(1:3, experiment, replace = TRUE)
  history_p2[1:experiment] = sample(1:3, experiment, replace = TRUE)
  
  # calculate the rest of the decisions to n periods
  for (i in (experiment+1):n){
    history_p1[i] = decision_avgpay_InertiaLogit(mu, beta, Delta, i, history_p1, history_p2)
    history_p2[i] = decision_avgpay_InertiaLogit(mu, beta, Delta, i, history_p2, history_p1)
  }
  
  # create the dataset for this simulation
  df = data.frame(
    p1_choice = history_p1[(experiment+1):n],
    p2_choice = history_p2[(experiment+1):n],
    period = (experiment+1):n,
    sim_id = s)
  
  if(s==1){
    full_data = df}
  else{
    full_data = rbind(full_data, df)}
}

# create and update type variables
full_data$type2 = NA

for (i in 1:length(full_data$period)){
  # update MV types
  if (full_data$p1_choice[i] == 1 & full_data$p2_choice[i] == 1){full_data$type2[i] = 1}
  else if (full_data$p1_choice[i] == 2 & full_data$p2_choice[i] == 2){full_data$type2[i] = 2}
  else if (full_data$p1_choice[i] == 3 & full_data$p2_choice[i] == 3){full_data$type2[i] = 3}
  else if (full_data$p1_choice[i] == 1 & full_data$p2_choice[i] == 3){full_data$type2[i] = 4}
  else if (full_data$p1_choice[i] == 2 & full_data$p2_choice[i] == 1){full_data$type2[i] = 5}
  else if (full_data$p1_choice[i] == 3 & full_data$p2_choice[i] == 2){full_data$type2[i] = 6}
  else if (full_data$p1_choice[i] == 1 & full_data$p2_choice[i] == 2){full_data$type2[i] = 7}
  else if (full_data$p1_choice[i] == 2 & full_data$p2_choice[i] == 3){full_data$type2[i] = 8}
  else{full_data$type2[i] = 9}
}


##### Transition prob matrix on br cycle with sim data #####
# generate the empty matrix
transition = matrix(0, nrow = 2, ncol = 6)
rownames(transition) = c('diagonal at t', 'off-diagonal at t')
colnames(transition) = c('stay at t+1', 'BR at t+1', 'BR*2 at t+1', 'other 1 step', 'one 2 steps', 'obs')

# loop over pairs
for (j in 1:sim){
  
  df_pair = filter(full_data, sim_id == j)
  
  # loop over observations
  for (k in 2:length(df_pair$period)){
    
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

# data export
xtable(transition_prob, digits = 2)



