##### Package and Payoff #####
# add package
rm(list = ls())
library(ggplot2)
library(xtable)
library(here)
setwd("~/Desktop/jotarepos/correq/corr_equilibrium/simulations/")
source("BMregrets.R")
##### Pair level graph and joint density - HM2001 #####
# set up the parameters for the simulation
n = 1000 # number of periods in each simulation
sim = 1 # number of simulations
experiment = 1 # number of experimentation periods where players randomly make decisions
delta = 0.1
gamma = 0.0001
beta= 0.01
pay_chicken = matrix(c(100,200,600,500),2,2) # payoff matrix 2x2

# set up the joint density matrix
joint_density_all = matrix(c(0,0,0,0),2,2)

# set up the joint density matrix for each simulation
joint_density = list()


# run the simulations a la logit. 
for (s in 1:sim){
  # set up the probability choices for HM2001
  prob_p1 = rep(0.5, n)
  prob_p2 = rep(0.5, n)
  
    # set up the vectors for choices and game parameters
  history_p1 = rep(1, n)
  history_p2 = rep(2, n)

  # calculate the experimentation periods with random starting decisions
  history_p1[1:experiment] = rep(2, experiment)
  history_p2[1:experiment] = rep(2, experiment)
  
  # set up the joint density matrix for the current simulation
  joint_density[[s]] = matrix(c(0,0,0,0),2,2)
  
  # calculate the rest of the decisions to n periods
  for (i in (experiment+1):n){
    history_p1[i] = decision_logit_hm2001(beta, i, history_p1, history_p2,prob_p1)[1]
    history_p2[i] = decision_logit_hm2001(beta, i, history_p2, history_p1, prob_p2)[1]
    
    prob_p1[i] = decision_logit_hm2001(beta, i, history_p1, history_p2,prob_p1)[2]
    prob_p2[i] = decision_logit_hm2001(beta, i, history_p2, history_p1,prob_p2)[2]
    # update the joint density matrix
    if (history_p1[i]==1 & history_p2[i]==1){joint_density[[s]][1,1]=joint_density[[s]][1,1]+1}
    else if (history_p1[i]==1 & history_p2[i]==2){joint_density[[s]][1,2]=joint_density[[s]][1,2]+1}
    else if (history_p1[i]==2 & history_p2[i]==1){joint_density[[s]][2,1]=joint_density[[s]][2,1]+1}
    else{joint_density[[s]][2,2]=joint_density[[s]][2,2]+1}
  }
  
  # normalize the frequency to probability
  joint_density[[s]] = round(joint_density[[s]]/sum(joint_density[[s]]), 3)
  
  # create the dataset for figures
  df = data.frame(
    p1_choice = history_p1,
    p2_choice = history_p2,
    period = 1:n
  )
}

# finalize the joint density matrix
xtable(joint_density[[1]])

for (a in 1:sim){
  joint_density_all = joint_density_all + joint_density[[a]]
}
joint_density_all = joint_density_all / sim
xtable(joint_density_all, caption = "")





### run the simulations a la HM2001
for (s in 1:sim){
  # set up the probability choices for HM2001
  prob_p1 = rep(0.5, n)
  prob_p2 = rep(0.5, n)
  
  # set up the vectors for choices and game parameters
  history_p1 = rep(0, n)
  history_p2 = rep(0, n)
  
  # calculate the experimentation periods with random starting decisions
  history_p1[1:experiment] = sample(1:2, experiment, replace = TRUE)
  history_p2[1:experiment] = sample(1:2, experiment, replace = TRUE)
  
  # set up the joint density matrix for the current simulation
  joint_density[[s]] = matrix(c(0,0,0,0),2,2)
  
  # calculate the rest of the decisions to n periods
  for (i in (experiment+1):n){
    history_p1[i] = decision_hm2001(mu, delta, gamma, i, history_p1, history_p2, prob_p1)[1]
    prob_p1[i] = decision_hm2001(mu, delta, gamma, i, history_p1, history_p2, prob_p1)[2]
    history_p2[i] = decision_hm2001(mu, delta, gamma, i, history_p2, history_p1, prob_p2)[1]
    prob_p2[i] = decision_hm2001(mu, delta, gamma, i, history_p2, history_p1, prob_p2)[2]
    
    # update the joint density matrix
    if (history_p1[i]==1 & history_p2[i]==1){joint_density[[s]][1,1]=joint_density[[s]][1,1]+1}
    else if (history_p1[i]==1 & history_p2[i]==2){joint_density[[s]][1,2]=joint_density[[s]][1,2]+1}
    else if (history_p1[i]==2 & history_p2[i]==1){joint_density[[s]][2,1]=joint_density[[s]][2,1]+1}
    else{joint_density[[s]][2,2]=joint_density[[s]][2,2]+1}
  }
  
  # normalize the frequency to probability
  joint_density[[s]] = round(joint_density[[s]]/sum(joint_density[[s]]), 3)
  
  # create the dataset for figures
  df = data.frame(
    p1_choice = history_p1,
    p2_choice = history_p2,
    period = 1:n
  )
}

# finalize the joint density matrix
xtable(joint_density[[1]])

for (a in 1:sim){
  joint_density_all = joint_density_all + joint_density[[a]]
}
joint_density_all = joint_density_all / sim
xtable(joint_density_all, caption = title)

