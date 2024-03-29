##### MV Preparation #####
# add package
rm(list = ls())
library(ggplot2)
library(xtable)
library(dplyr)
library(here)
source(here("simulations/MVregrets.R"))

# set up the parameters for the simulation
pay_MV = matrix(c(0,200,100,100,0,200,200,100,0),3,3) # payoff matrix 3x3
n = 500 # number of periods in each simulation
sim = 500 # number of simulations
experiment = 50 # number of experimentation periods where players randomly make decisions
experiment2 = 25
start = 50 # number of periods when the data start counting
mu = 600 # HM response parameter
beta = 1 # logit response parameter
Delta = 0.8 # inertia logit parameter


##### MV counterfactual regret - logit response #####
# set up the initial joint density dataset
length = rep(0, sim)
dfsim_jd = data.frame(sim = length, jd_11 = length, jd_12 = length, jd_13 = length, jd_21 = length,
                      jd_22 = length, jd_23 = length, jd_31 = length, jd_32 = length, jd_33 = length,
                      p1_payoff = length, p2_payoff = length)

# run the simulations
for (s in 1:sim){
  
  # set up the vectors for choices and game parameters
  history_p1 = rep(0, n)
  history_p2 = rep(0, n)
  
  # calculate the experimentation periods with random starting decisions
  # history_p1[1:9] = c(1,2,3,1,2,3,1,2,3)
  # history_p2[1:9] = c(1,1,1,2,2,2,3,3,3)
  # history_p1[10:experiment] = sample(1:3, experiment-9, replace = TRUE)
  # history_p2[10:experiment] = sample(1:3, experiment-9, replace = TRUE)
  history_p1[1:experiment2] = sample(1:3, experiment2, replace = TRUE)
  history_p2[1:experiment2] = sample(1:3, experiment2, replace = TRUE)
  history_p1[(experiment2+1):experiment] = rep(3, experiment-experiment2) 
  history_p2[(experiment2+1):experiment] = rep(3, experiment-experiment2)
  
  # calculate the rest of the decisions to n periods
  for (i in (experiment+1):n){
    
    history_p1[i] = decision_hm2000r_logitR(mu, beta, i, history_p1, history_p2)
    history_p2[i] = decision_hm2000r_logitR(mu, beta, i, history_p2, history_p1)
  }
  
  # create the dataset for this simulation
  dfsim = data.frame(
    p1_choice = history_p1[(experiment+1):n],
    p2_choice = history_p2[(experiment+1):n],
    period = (experiment+1):n
  )
  dfsim = dfsim %>% mutate(
    sim = s,
    regret = 'C',
    response = 'Logit',
    mu = mu,
    beta = beta,
    Delta = Delta
  )
  dfsim = dfsim %>% mutate(
    is_11 = ifelse(p1_choice==1 & p2_choice==1, 1, 0),
    is_12 = ifelse(p1_choice==1 & p2_choice==2, 1, 0),
    is_13 = ifelse(p1_choice==1 & p2_choice==3, 1, 0),
    is_21 = ifelse(p1_choice==2 & p2_choice==1, 1, 0),
    is_22 = ifelse(p1_choice==2 & p2_choice==2, 1, 0),
    is_23 = ifelse(p1_choice==2 & p2_choice==3, 1, 0),
    is_31 = ifelse(p1_choice==3 & p2_choice==1, 1, 0),
    is_32 = ifelse(p1_choice==3 & p2_choice==2, 1, 0),
    is_33 = ifelse(p1_choice==3 & p2_choice==3, 1, 0)
  )
  for (j in 1:length(dfsim$period)){
    dfsim$p1_payoff[j] = pay_MV[dfsim$p1_choice[j], dfsim$p2_choice[j]]
    dfsim$p2_payoff[j] = pay_MV[dfsim$p2_choice[j], dfsim$p1_choice[j]]
  }
  
  # for joint density data, only use the last 100 periods.
  dfsim = filter(dfsim, period > start)
  
  # record the joint density row s
  dfsim_jd$sim[s] = s
  dfsim_jd$jd_11[s] = mean(dfsim$is_11)
  dfsim_jd$jd_12[s] = mean(dfsim$is_12)
  dfsim_jd$jd_13[s] = mean(dfsim$is_13)
  dfsim_jd$jd_21[s] = mean(dfsim$is_21)
  dfsim_jd$jd_22[s] = mean(dfsim$is_22)
  dfsim_jd$jd_23[s] = mean(dfsim$is_23)
  dfsim_jd$jd_31[s] = mean(dfsim$is_31)
  dfsim_jd$jd_32[s] = mean(dfsim$is_32)
  dfsim_jd$jd_33[s] = mean(dfsim$is_33)
  dfsim_jd$p1_payoff[s] = mean(dfsim$p1_payoff)
  dfsim_jd$p2_payoff[s] = mean(dfsim$p2_payoff)
}

# add the simulation characteristics to the joint density dataset
dfsim_jd = dfsim_jd %>% mutate(
  regret = 'C',
  response = 'Logit',
  mu = mu,
  beta = beta,
  Delta = Delta
)

# calculate the fraction in CE
dfsim_jd$in_ce <- 0
dfsim_jd <- dfsim_jd %>%
  mutate(in_ce = ifelse(p1_payoff>100 & abs((p2_payoff-100)/(p1_payoff-100))>=1/2 & abs((p2_payoff-100)/(p1_payoff-100))<=2,1,in_ce))
dfsim_jd <- dfsim_jd %>%
  mutate(in_ce = ifelse(p1_payoff==100 & p2_payoff==100,1,in_ce))

# calculate the distance
dfsim_jd = dfsim_jd %>%
  mutate(d_mne = sqrt((p1_payoff-100)^2+(p2_payoff-100)^2),
         d_tce = sqrt((p1_payoff-150)^2+(p2_payoff-150)^2))

# generate joint distribution table
jd = matrix(c(0,0,0,0,0,0,0,0,0),3,3)

jd[1,1] = mean(dfsim_jd$jd_11)
jd[1,2] = mean(dfsim_jd$jd_12)
jd[1,3] = mean(dfsim_jd$jd_13)
jd[2,1] = mean(dfsim_jd$jd_21)
jd[2,2] = mean(dfsim_jd$jd_22)
jd[2,3] = mean(dfsim_jd$jd_23)
jd[3,1] = mean(dfsim_jd$jd_31)
jd[3,2] = mean(dfsim_jd$jd_32)
jd[3,3] = mean(dfsim_jd$jd_33)

# table output
print(xtable(jd, digits = 3, caption = 'MV games Counterfactural Logit Response'))


##### MV average regret - logit response #####
# set up the initial joint density dataset
length = rep(0, sim)
dfsim_jd = data.frame(sim = length, jd_11 = length, jd_12 = length, jd_13 = length, jd_21 = length,
                      jd_22 = length, jd_23 = length, jd_31 = length, jd_32 = length, jd_33 = length,
                      p1_payoff = length, p2_payoff = length)

# run the simulations
for (s in 1:sim){
  
  # set up the vectors for choices and game parameters
  history_p1 = rep(0, n)
  history_p2 = rep(0, n)
  
  # calculate the experimentation periods with random starting decisions
  # history_p1[1:9] = c(1,2,3,1,2,3,1,2,3)
  # history_p2[1:9] = c(1,1,1,2,2,2,3,3,3)
  # history_p1[10:experiment] = sample(1:3, experiment-9, replace = TRUE)
  # history_p2[10:experiment] = sample(1:3, experiment-9, replace = TRUE)
  history_p1[1:experiment2] = sample(1:3, experiment2, replace = TRUE)
  history_p2[1:experiment2] = sample(1:3, experiment2, replace = TRUE)
  history_p1[(experiment2+1):experiment] = rep(1, experiment-experiment2) 
  history_p2[(experiment2+1):experiment] = rep(3, experiment-experiment2)
  
  # calculate the rest of the decisions to n periods
  for (i in (experiment+1):n){
    
    history_p1[i] = decision_avgpay_logitR(mu, beta, i, history_p1, history_p2)
    history_p2[i] = decision_avgpay_logitR(mu, beta, i, history_p2, history_p1)
  }
  
  # create the dataset for this simulation
  dfsim = data.frame(
    p1_choice = history_p1[(experiment+1):n],
    p2_choice = history_p2[(experiment+1):n],
    period = (experiment+1):n
  )
  dfsim = dfsim %>% mutate(
    sim = s,
    regret = 'A',
    response = 'Logit',
    mu = mu,
    beta = beta,
    Delta = Delta
  )
  dfsim = dfsim %>% mutate(
    is_11 = ifelse(p1_choice==1 & p2_choice==1, 1, 0),
    is_12 = ifelse(p1_choice==1 & p2_choice==2, 1, 0),
    is_13 = ifelse(p1_choice==1 & p2_choice==3, 1, 0),
    is_21 = ifelse(p1_choice==2 & p2_choice==1, 1, 0),
    is_22 = ifelse(p1_choice==2 & p2_choice==2, 1, 0),
    is_23 = ifelse(p1_choice==2 & p2_choice==3, 1, 0),
    is_31 = ifelse(p1_choice==3 & p2_choice==1, 1, 0),
    is_32 = ifelse(p1_choice==3 & p2_choice==2, 1, 0),
    is_33 = ifelse(p1_choice==3 & p2_choice==3, 1, 0)
  )
  for (j in 1:length(dfsim$period)){
    dfsim$p1_payoff[j] = pay_MV[dfsim$p1_choice[j], dfsim$p2_choice[j]]
    dfsim$p2_payoff[j] = pay_MV[dfsim$p2_choice[j], dfsim$p1_choice[j]]
  }
  
  # for joint density data, only use the last 100 periods.
  dfsim = filter(dfsim, period > start)
  
  # record the joint density row s
  dfsim_jd$sim[s] = s
  dfsim_jd$jd_11[s] = mean(dfsim$is_11)
  dfsim_jd$jd_12[s] = mean(dfsim$is_12)
  dfsim_jd$jd_13[s] = mean(dfsim$is_13)
  dfsim_jd$jd_21[s] = mean(dfsim$is_21)
  dfsim_jd$jd_22[s] = mean(dfsim$is_22)
  dfsim_jd$jd_23[s] = mean(dfsim$is_23)
  dfsim_jd$jd_31[s] = mean(dfsim$is_31)
  dfsim_jd$jd_32[s] = mean(dfsim$is_32)
  dfsim_jd$jd_33[s] = mean(dfsim$is_33)
  dfsim_jd$p1_payoff[s] = mean(dfsim$p1_payoff)
  dfsim_jd$p2_payoff[s] = mean(dfsim$p2_payoff)
}

# add the simulation characteristics to the joint density dataset
dfsim_jd = dfsim_jd %>% mutate(
  regret = 'A',
  response = 'Logit',
  mu = mu,
  beta = beta,
  Delta = Delta
)

# calculate the fraction in CE
dfsim_jd$in_ce <- 0
dfsim_jd <- dfsim_jd %>%
  mutate(in_ce = ifelse(p1_payoff>100 & abs((p2_payoff-100)/(p1_payoff-100))>=1/2 & abs((p2_payoff-100)/(p1_payoff-100))<=2,1,in_ce))
dfsim_jd <- dfsim_jd %>%
  mutate(in_ce = ifelse(p1_payoff==100 & p2_payoff==100,1,in_ce))

# calculate the distance
dfsim_jd = dfsim_jd %>%
  mutate(d_mne = sqrt((p1_payoff-100)^2+(p2_payoff-100)^2),
         d_tce = sqrt((p1_payoff-150)^2+(p2_payoff-150)^2))

# generate joint distribution table
jd = matrix(c(0,0,0,0,0,0,0,0,0),3,3)

jd[1,1] = mean(dfsim_jd$jd_11)
jd[1,2] = mean(dfsim_jd$jd_12)
jd[1,3] = mean(dfsim_jd$jd_13)
jd[2,1] = mean(dfsim_jd$jd_21)
jd[2,2] = mean(dfsim_jd$jd_22)
jd[2,3] = mean(dfsim_jd$jd_23)
jd[3,1] = mean(dfsim_jd$jd_31)
jd[3,2] = mean(dfsim_jd$jd_32)
jd[3,3] = mean(dfsim_jd$jd_33)

# table output
print(xtable(jd, digits = 3, caption = 'MV games Average Logit Response'))


##### CH Preparation #####
# add package
rm(list = ls())
library(ggplot2)
library(xtable)
library(dplyr)
library(here)
source(here("simulations/BMregrets.R"))

# set up the parameters for the simulation
pay_chicken = matrix(c(100,200,600,500),2,2) # payoff matrix 2x2
n = 500 # number of periods in each simulation
sim = 500 # number of simulations
experiment = 50 # number of experimentation periods where players randomly make decisions
experiment2 = 25
start = 50 # number of periods when the data start counting
mu = 600 # HM response parameter
beta = 1 # logit response parameter
Delta = 0.8 # inertia logit parameter


##### CH counterfactual regret - logit response #####
# set up the initial joint density dataset
length = rep(0, sim)
dfsim_jd = data.frame(sim = length, jd_11 = length, jd_12 = length, jd_21 = length, 
                      jd_22 = length, p1_payoff = length, p2_payoff = length)

# run the simulations
for (s in 1:sim){
  
  # set up the vectors for choices and game parameters
  history_p1 = rep(0, n)
  history_p2 = rep(0, n)
  
  # calculate the experimentation periods with random starting decisions
  #history_p1[1:4] = c(1,1,2,2)
  #history_p2[1:4] = c(1,2,1,2)
  #history_p1[5:experiment] = sample(1:2, experiment-4, replace = TRUE)
  #history_p2[5:experiment] = sample(1:2, experiment-4, replace = TRUE)
  history_p1[1:experiment2] = sample(1:2, experiment2, replace = TRUE)
  history_p2[1:experiment2] = sample(1:2, experiment2, replace = TRUE)
  history_p1[(experiment2+1):experiment] = rep(2, experiment-experiment2) 
  history_p2[(experiment2+1):experiment] = rep(2, experiment-experiment2)
  
  # calculate the rest of the decisions to n periods
  for (i in (experiment+1):n){
    
    history_p1[i] = decision_hm2000r_logitR(mu, beta, i, history_p1, history_p2)
    history_p2[i] = decision_hm2000r_logitR(mu, beta, i, history_p2, history_p1)
  }
  
  # create the dataset for this simulation
  dfsim = data.frame(
    p1_choice = history_p1[(experiment+1):n],
    p2_choice = history_p2[(experiment+1):n],
    period = (experiment+1):n
  )
  dfsim = dfsim %>% mutate(
    sim = s,
    regret = 'C',
    response = 'Logit',
    mu = mu,
    beta = beta,
    Delta = Delta
  )
  dfsim = dfsim %>% mutate(
    is_11 = ifelse(p1_choice==1 & p2_choice==1, 1, 0),
    is_12 = ifelse(p1_choice==1 & p2_choice==2, 1, 0),
    is_21 = ifelse(p1_choice==2 & p2_choice==1, 1, 0),
    is_22 = ifelse(p1_choice==2 & p2_choice==2, 1, 0)
  )
  for (j in 1:length(dfsim$period)){
    dfsim$p1_payoff[j] = pay_chicken[dfsim$p1_choice[j], dfsim$p2_choice[j]]
    dfsim$p2_payoff[j] = pay_chicken[dfsim$p2_choice[j], dfsim$p1_choice[j]]
  }
  
  # for joint density data, only use the last 100 periods.
  dfsim = filter(dfsim, period > start)
  
  # record the joint density row s
  dfsim_jd$sim[s] = s
  dfsim_jd$jd_11[s] = mean(dfsim$is_11)
  dfsim_jd$jd_12[s] = mean(dfsim$is_12)
  dfsim_jd$jd_21[s] = mean(dfsim$is_21)
  dfsim_jd$jd_22[s] = mean(dfsim$is_22)
  dfsim_jd$p1_payoff[s] = mean(dfsim$p1_payoff)
  dfsim_jd$p2_payoff[s] = mean(dfsim$p2_payoff)
}

# add the simulation characteristics to the joint density dataset
dfsim_jd = dfsim_jd %>% mutate(
  regret = 'C',
  response = 'Logit',
  mu = mu,
  beta = beta,
  Delta = Delta
)

# calculate the fraction in CE
dfsim_jd$in_ce <- 0
dfsim_jd <- dfsim_jd %>%
  mutate(in_ce = ifelse(p1_payoff/p2_payoff>=1 & abs((p2_payoff-200)/(p1_payoff-600))>=1/3 & abs((p2_payoff-200)/(p1_payoff-600))<=7/5,1,in_ce), 
         in_ce = ifelse(p1_payoff/p2_payoff<1 & abs((p2_payoff-600)/(p1_payoff-200))>=5/7 & abs((p2_payoff-600)/(p1_payoff-200))<=3,1,in_ce))
dfsim_jd <- dfsim_jd %>%
  mutate(in_ce = ifelse(p1_payoff/p2_payoff>1 & p1_payoff==600,1,in_ce), 
         in_ce = ifelse(p1_payoff/p2_payoff<1 & p1_payoff==200,1,in_ce))

# calculate the distance
dfsim_jd = dfsim_jd %>%
  mutate(d_mne = sqrt((p1_payoff-350)^2+(p2_payoff-350)^2),
         d_tce = sqrt((p1_payoff-1300/3)^2+(p2_payoff-1300/3)^2))

# joint distribution table
jd = matrix(c(0,0,0,0),2,2)

jd[1,1] = mean(dfsim_jd$jd_11)
jd[1,2] = mean(dfsim_jd$jd_12)
jd[2,1] = mean(dfsim_jd$jd_21)
jd[2,2] = mean(dfsim_jd$jd_22)

# table output
print(xtable(jd, digits = 3, caption = 'CH games Counterfactual Logit Response'))


##### CH average regret - logit response #####
# set up the initial joint density dataset
length = rep(0, sim)
dfsim_jd = data.frame(sim = length, jd_11 = length, jd_12 = length, jd_21 = length, 
                      jd_22 = length, p1_payoff = length, p2_payoff = length)

# run the simulations
for (s in 1:sim){
  
  # set up the vectors for choices and game parameters
  history_p1 = rep(0, n)
  history_p2 = rep(0, n)
  
  # calculate the experimentation periods with random starting decisions
  # history_p1[1:4] = c(1,1,2,2)
  # history_p2[1:4] = c(1,2,1,2)
  # history_p1[5:experiment] = sample(1:2, experiment-4, replace = TRUE)
  # history_p2[5:experiment] = sample(1:2, experiment-4, replace = TRUE)
  history_p1[1:experiment2] = sample(1:2, experiment2, replace = TRUE)
  history_p2[1:experiment2] = sample(1:2, experiment2, replace = TRUE)
  history_p1[(experiment2+1):experiment] = rep(1, experiment-experiment2) 
  history_p2[(experiment2+1):experiment] = rep(1, experiment-experiment2)
  
  # calculate the rest of the decisions to n periods
  for (i in (experiment+1):n){
    
    history_p1[i] = decision_avgpay_logitR(mu, beta, i, history_p1, history_p2)
    history_p2[i] = decision_avgpay_logitR(mu, beta, i, history_p2, history_p1)
  }
  
  # create the dataset for this simulation
  dfsim = data.frame(
    p1_choice = history_p1[(experiment+1):n],
    p2_choice = history_p2[(experiment+1):n],
    period = (experiment+1):n
  )
  dfsim = dfsim %>% mutate(
    sim = s,
    regret = 'A',
    response = 'Logit',
    mu = mu,
    beta = beta,
    Delta = Delta
  )
  dfsim = dfsim %>% mutate(
    is_11 = ifelse(p1_choice==1 & p2_choice==1, 1, 0),
    is_12 = ifelse(p1_choice==1 & p2_choice==2, 1, 0),
    is_21 = ifelse(p1_choice==2 & p2_choice==1, 1, 0),
    is_22 = ifelse(p1_choice==2 & p2_choice==2, 1, 0)
  )
  for (j in 1:length(dfsim$period)){
    dfsim$p1_payoff[j] = pay_chicken[dfsim$p1_choice[j], dfsim$p2_choice[j]]
    dfsim$p2_payoff[j] = pay_chicken[dfsim$p2_choice[j], dfsim$p1_choice[j]]
  }
  
  # for joint density data, only use the last 100 periods.
  dfsim = filter(dfsim, period > start)
  
  # record the joint density row s
  dfsim_jd$sim[s] = s
  dfsim_jd$jd_11[s] = mean(dfsim$is_11)
  dfsim_jd$jd_12[s] = mean(dfsim$is_12)
  dfsim_jd$jd_21[s] = mean(dfsim$is_21)
  dfsim_jd$jd_22[s] = mean(dfsim$is_22)
  dfsim_jd$p1_payoff[s] = mean(dfsim$p1_payoff)
  dfsim_jd$p2_payoff[s] = mean(dfsim$p2_payoff)
}

# add the simulation characteristics to the joint density dataset
dfsim_jd = dfsim_jd %>% mutate(
  regret = 'A',
  response = 'Logit',
  mu = mu,
  beta = beta,
  Delta = Delta
)

# calculate the fraction in CE
dfsim_jd$in_ce <- 0
dfsim_jd <- dfsim_jd %>%
  mutate(in_ce = ifelse(p1_payoff/p2_payoff>=1 & abs((p2_payoff-200)/(p1_payoff-600))>=1/3 & abs((p2_payoff-200)/(p1_payoff-600))<=7/5,1,in_ce), 
         in_ce = ifelse(p1_payoff/p2_payoff<1 & abs((p2_payoff-600)/(p1_payoff-200))>=5/7 & abs((p2_payoff-600)/(p1_payoff-200))<=3,1,in_ce))
dfsim_jd <- dfsim_jd %>%
  mutate(in_ce = ifelse(p1_payoff/p2_payoff>1 & p1_payoff==600,1,in_ce), 
         in_ce = ifelse(p1_payoff/p2_payoff<1 & p1_payoff==200,1,in_ce))

# calculate the distance
dfsim_jd = dfsim_jd %>%
  mutate(d_mne = sqrt((p1_payoff-350)^2+(p2_payoff-350)^2),
         d_tce = sqrt((p1_payoff-1300/3)^2+(p2_payoff-1300/3)^2))

# joint distribution table
jd = matrix(c(0,0,0,0),2,2)

jd[1,1] = mean(dfsim_jd$jd_11)
jd[1,2] = mean(dfsim_jd$jd_12)
jd[2,1] = mean(dfsim_jd$jd_21)
jd[2,2] = mean(dfsim_jd$jd_22)

# table output
print(xtable(jd, digits = 3, caption = 'CH games Average Logit Response'))

