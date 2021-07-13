##### Package and Payoff #####
# add package
# rm(list = ls())
library(ggplot2)
library(xtable)
library(dplyr)
library(here)

source(here("simulations/BMregrets.R"))

##### Pair level graph, joint density, and pair level table #####
# set up the parameters for the simulation
mu = 1800 # HM2000 probability parameter
n = 500 # number of periods in each simulation
sim = 500 # number of simulations
experiment = 100 # number of experimentation periods where players randomly make decisions
beta = c(0.39,0.15)
Delta = 0.49
pay_chicken = matrix(c(100,200,600,500),2,2) # payoff matrix 2x2

# set up the joint density matrix
joint_density_all = matrix(c(0,0,0,0),2,2)

# set up the joint density matrix for each simulation
joint_density = list()

# set up the overall dataset
length = rep(0, sim)
df_sim = data.frame(sim = length, jd_11 = length, jd_12 = length, jd_21 = length, jd_22 = length,
                    p1_payoff = length, p2_payoff = length)

# run the simulations
for (s in 1:sim){
  
  # set up the vectors for choices and game parameters
  history_p1 = rep(0, n)
  history_p2 = rep(0, n)
  
  # calculate the experimentation periods with random starting decisions
  history_p1[1:experiment] = sample(1:2, experiment, replace = TRUE)
  history_p2[1:experiment] = sample(1:2, experiment, replace = TRUE)
  #history_p1[1:experiment] = rep(1, experiment)
  #history_p2[1:experiment] = rep(2, experiment)
  
  # set up the joint density matrix for the current simulation
  joint_density[[s]] = matrix(c(0,0,0,0),2,2)
  
  # calculate the rest of the decisions to n periods
  for (i in (experiment+1):n){
    #history_p1[i] = decision_avgpay_InertiaLogit_truncate(mu, beta, Delta, i, history_p1, history_p2)
    #history_p2[i] = decision_avgpay_InertiaLogit_truncate(mu, beta, Delta, i, history_p2, history_p1)
    #history_p1[i] = decision_avgpay_InertiaLogit(mu, beta, Delta, i, history_p1, history_p2)
    #history_p2[i] = decision_avgpay_InertiaLogit(mu, beta, Delta, i, history_p2, history_p1)
    #history_p1[i] = decision_avgpay_logit(mu, beta, i, history_p1, history_p2)
    #history_p2[i] = decision_avgpay_logit(mu, beta, i, history_p2, history_p1)
    history_p1[i] = decision_avgpay(mu, i, history_p1, history_p2)
    history_p2[i] = decision_avgpay(mu, i, history_p2, history_p1)
    
    # update the joint density matrix
    if (history_p1[i]==1 & history_p2[i]==1){joint_density[[s]][1,1]=joint_density[[s]][1,1]+1}
    else if (history_p1[i]==1 & history_p2[i]==2){joint_density[[s]][1,2]=joint_density[[s]][1,2]+1}
    else if (history_p1[i]==2 & history_p2[i]==1){joint_density[[s]][2,1]=joint_density[[s]][2,1]+1}
    else{joint_density[[s]][2,2]=joint_density[[s]][2,2]+1}
  }
  
  # normalize the frequency to probability
  joint_density[[s]] = round(joint_density[[s]]/sum(joint_density[[s]]), 3)
  
  # create the dataset for this simulation
  df = data.frame(
    p1_choice = history_p1[(experiment+1):n],
    p2_choice = history_p2[(experiment+1):n],
    period = (experiment+1):n)
  df = df %>% mutate(
    is_11 = ifelse(p1_choice==1 & p2_choice==1, 1, 0),
    is_12 = ifelse(p1_choice==1 & p2_choice==2, 1, 0),
    is_21 = ifelse(p1_choice==2 & p2_choice==1, 1, 0),
    is_22 = ifelse(p1_choice==2 & p2_choice==2, 1, 0))
  for (j in 1:length(df$period)){
    df$p1_payoff[j] = pay_chicken[df$p1_choice[j], df$p2_choice[j]]
    df$p2_payoff[j] = pay_chicken[df$p2_choice[j], df$p1_choice[j]]
  }
  
  # record the sim ID to the overall data
  df_sim$sim[s] = s
  
  # record the joint density
  df_sim$jd_11[s] = mean(df$is_11)
  df_sim$jd_12[s] = mean(df$is_12)
  df_sim$jd_21[s] = mean(df$is_21)
  df_sim$jd_22[s] = mean(df$is_22)
  df_sim$p1_payoff[s] = mean(df$p1_payoff)
  df_sim$p2_payoff[s] = mean(df$p2_payoff)
}

# calculate the overall joint distribution
for (a in 1:sim){
  joint_density_all = joint_density_all + joint_density[[a]]
}
joint_density_all = joint_density_all / sim
xtable(joint_density_all)

# calculate the fraction in CE
df_sim$in_ce <- 0
df_sim <- df_sim %>%
  mutate(in_ce = ifelse(p1_payoff/p2_payoff>=1 & abs((p2_payoff-200)/(p1_payoff-600))>=1/3 & abs((p2_payoff-200)/(p1_payoff-600))<=7/5,1,in_ce), 
         in_ce = ifelse(p1_payoff/p2_payoff<1 & abs((p2_payoff-600)/(p1_payoff-200))>=5/7 & abs((p2_payoff-600)/(p1_payoff-200))<=3,1,in_ce))
df_sim <- df_sim %>%
  mutate(in_ce = ifelse(p1_payoff/p2_payoff>1 & p1_payoff==600,1,in_ce), 
         in_ce = ifelse(p1_payoff/p2_payoff<1 & p1_payoff==200,1,in_ce))

# calculate the distance
df_sim = df_sim %>%
  mutate(
    d_mne = sqrt((p1_payoff-350)^2+(p2_payoff-350)^2),
    d_tce = sqrt((p1_payoff-1300/3)^2+(p2_payoff-1300/3)^2))

# display the results
mean(df_sim$in_ce)
mean(df_sim$d_mne)
mean(df_sim$d_tce)
t.test(df_sim$d_mne, df_sim$d_tce, mu=0, paired = TRUE)$p.value

#rm(df, df_sim, joint_density, joint_density_all, history_p1, history_p2)