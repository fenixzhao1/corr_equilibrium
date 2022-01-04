##### Preparation #####
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
experiment = 100 # number of experimentation periods where players randomly make decisions
start = 400 # number of periods when the data start counting
mu = 1000 # HM response parameter
beta = 1 # logit response parameter
Delta = 0.5 # inertia logit parameter

# set up the aggregate dataset
df = data.frame()
df_jd = data.frame()


##### signed regret - HM response #####
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
  history_p1[1:experiment] = sample(1:3, experiment, replace = TRUE)
  history_p2[1:experiment] = sample(1:3, experiment, replace = TRUE)
  
  # calculate the rest of the decisions to n periods
  for (i in (experiment+1):n){

    history_p1[i] = decision_hm2000(mu, i, history_p1, history_p2)
    history_p2[i] = decision_hm2000(mu, i, history_p2, history_p1)
  }
  
  # create the dataset for this simulation
  dfsim = data.frame(
    p1_choice = history_p1[(experiment+1):n],
    p2_choice = history_p2[(experiment+1):n],
    period = (experiment+1):n
  )
  dfsim = dfsim %>% mutate(
    sim = s,
    regret = 'SignedRegret',
    response = 'HMResponse',
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
  
  # add the choice dataset to the main dataset
  df = rbind(df, dfsim)
  
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
  regret = 'SignedRegret',
  response = 'HMResponse',
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

# add the joint density dataset to the main dataset
df_jd = rbind(df_jd, dfsim_jd)

# remove temporary values
rm(dfsim, dfsim_jd)


##### counterfactual regret - HM response #####
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
  history_p1[1:experiment] = sample(1:3, experiment, replace = TRUE)
  history_p2[1:experiment] = sample(1:3, experiment, replace = TRUE)
  
  # calculate the rest of the decisions to n periods
  for (i in (experiment+1):n){
    
    history_p1[i] = decision_hm2000r(mu, i, history_p1, history_p2)
    history_p2[i] = decision_hm2000r(mu, i, history_p2, history_p1)
  }
  
  # create the dataset for this simulation
  dfsim = data.frame(
    p1_choice = history_p1[(experiment+1):n],
    p2_choice = history_p2[(experiment+1):n],
    period = (experiment+1):n
  )
  dfsim = dfsim %>% mutate(
    sim = s,
    regret = 'CounterfactualRegret',
    response = 'HMResponse',
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
  
  # add the choice dataset to the main dataset
  df = rbind(df, dfsim)
  
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
  regret = 'CounterfactualRegret',
  response = 'HMResponse',
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

# add the joint density dataset to the main dataset
df_jd = rbind(df_jd, dfsim_jd)

# remove temporary values
rm(dfsim, dfsim_jd)


##### average regret - HM response #####
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
  history_p1[1:experiment] = sample(1:3, experiment, replace = TRUE)
  history_p2[1:experiment] = sample(1:3, experiment, replace = TRUE)
  
  # calculate the rest of the decisions to n periods
  for (i in (experiment+1):n){
    
    history_p1[i] = decision_avgpay(mu, i, history_p1, history_p2)
    history_p2[i] = decision_avgpay(mu, i, history_p2, history_p1)
  }
  
  # create the dataset for this simulation
  dfsim = data.frame(
    p1_choice = history_p1[(experiment+1):n],
    p2_choice = history_p2[(experiment+1):n],
    period = (experiment+1):n
  )
  dfsim = dfsim %>% mutate(
    sim = s,
    regret = 'AverageRegret',
    response = 'HMResponse',
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
  
  # add the choice dataset to the main dataset
  df = rbind(df, dfsim)
  
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
  regret = 'AverageRegret',
  response = 'HMResponse',
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

# add the joint density dataset to the main dataset
df_jd = rbind(df_jd, dfsim_jd)

# remove temporary values
rm(dfsim, dfsim_jd)


##### signed regret - logit response #####
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
  history_p1[1:experiment] = sample(1:3, experiment, replace = TRUE)
  history_p2[1:experiment] = sample(1:3, experiment, replace = TRUE)
  
  # calculate the rest of the decisions to n periods
  for (i in (experiment+1):n){
    
    history_p1[i] = decision_hm2000_logitR(mu, beta, i, history_p1, history_p2)
    history_p2[i] = decision_hm2000_logitR(mu, beta, i, history_p2, history_p1)
  }
  
  # create the dataset for this simulation
  dfsim = data.frame(
    p1_choice = history_p1[(experiment+1):n],
    p2_choice = history_p2[(experiment+1):n],
    period = (experiment+1):n
  )
  dfsim = dfsim %>% mutate(
    sim = s,
    regret = 'SignedRegret',
    response = 'LogitResponse',
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
  
  # add the choice dataset to the main dataset
  df = rbind(df, dfsim)
  
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
  regret = 'SignedRegret',
  response = 'LogitResponse',
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

# add the joint density dataset to the main dataset
df_jd = rbind(df_jd, dfsim_jd)

# remove temporary values
rm(dfsim, dfsim_jd)


##### counterfactual regret - logit response #####
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
  history_p1[1:experiment] = sample(1:3, experiment, replace = TRUE)
  history_p2[1:experiment] = sample(1:3, experiment, replace = TRUE)
  
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
    regret = 'CounterfactualRegret',
    response = 'LogitResponse',
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
  
  # add the choice dataset to the main dataset
  df = rbind(df, dfsim)
  
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
  regret = 'CounterfactualRegret',
  response = 'LogitResponse',
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

# add the joint density dataset to the main dataset
df_jd = rbind(df_jd, dfsim_jd)

# remove temporary values
rm(dfsim, dfsim_jd)


##### average regret - logit response #####
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
  history_p1[1:experiment] = sample(1:3, experiment, replace = TRUE)
  history_p2[1:experiment] = sample(1:3, experiment, replace = TRUE)
  
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
    regret = 'AverageRegret',
    response = 'LogitResponse',
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
  
  # add the choice dataset to the main dataset
  df = rbind(df, dfsim)
  
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
  regret = 'AverageRegret',
  response = 'LogitResponse',
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

# add the joint density dataset to the main dataset
df_jd = rbind(df_jd, dfsim_jd)

# remove temporary values
rm(dfsim, dfsim_jd)


##### signed regret - inertia logit response #####
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
  history_p1[1:experiment] = sample(1:3, experiment, replace = TRUE)
  history_p2[1:experiment] = sample(1:3, experiment, replace = TRUE)
  
  # calculate the rest of the decisions to n periods
  for (i in (experiment+1):n){
    
    history_p1[i] = decision_hm2000_InertiaLogit(mu, beta, Delta, i, history_p1, history_p2)
    history_p2[i] = decision_hm2000_InertiaLogit(mu, beta, Delta, i, history_p2, history_p1)
  }
  
  # create the dataset for this simulation
  dfsim = data.frame(
    p1_choice = history_p1[(experiment+1):n],
    p2_choice = history_p2[(experiment+1):n],
    period = (experiment+1):n
  )
  dfsim = dfsim %>% mutate(
    sim = s,
    regret = 'SignedRegret',
    response = 'InertiaLogit',
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
  
  # add the choice dataset to the main dataset
  df = rbind(df, dfsim)
  
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
  regret = 'SignedRegret',
  response = 'InertiaLogit',
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

# add the joint density dataset to the main dataset
df_jd = rbind(df_jd, dfsim_jd)

# remove temporary values
rm(dfsim, dfsim_jd)


##### counterfactual regret - inertia logit response #####
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
  history_p1[1:experiment] = sample(1:3, experiment, replace = TRUE)
  history_p2[1:experiment] = sample(1:3, experiment, replace = TRUE)
  
  # calculate the rest of the decisions to n periods
  for (i in (experiment+1):n){
    
    history_p1[i] = decision_hm2000r_InertiaLogit(mu, beta, Delta, i, history_p1, history_p2)
    history_p2[i] = decision_hm2000r_InertiaLogit(mu, beta, Delta, i, history_p2, history_p1)
  }
  
  # create the dataset for this simulation
  dfsim = data.frame(
    p1_choice = history_p1[(experiment+1):n],
    p2_choice = history_p2[(experiment+1):n],
    period = (experiment+1):n
  )
  dfsim = dfsim %>% mutate(
    sim = s,
    regret = 'CounterfactualRegret',
    response = 'InertiaLogit',
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
  
  # add the choice dataset to the main dataset
  df = rbind(df, dfsim)
  
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
  regret = 'CounterfactualRegret',
  response = 'InertiaLogit',
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

# add the joint density dataset to the main dataset
df_jd = rbind(df_jd, dfsim_jd)

# remove temporary values
rm(dfsim, dfsim_jd)


##### average regret - inertia logit response #####
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
  history_p1[1:experiment] = sample(1:3, experiment, replace = TRUE)
  history_p2[1:experiment] = sample(1:3, experiment, replace = TRUE)
  
  # calculate the rest of the decisions to n periods
  for (i in (experiment+1):n){
    
    history_p1[i] = decision_avgpay_InertiaLogit(mu, beta, Delta, i, history_p1, history_p2)
    history_p2[i] = decision_avgpay_InertiaLogit(mu, beta, Delta, i, history_p2, history_p1)
  }
  
  # create the dataset for this simulation
  dfsim = data.frame(
    p1_choice = history_p1[(experiment+1):n],
    p2_choice = history_p2[(experiment+1):n],
    period = (experiment+1):n
  )
  dfsim = dfsim %>% mutate(
    sim = s,
    regret = 'AverageRegret',
    response = 'InertiaLogit',
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
  
  # add the choice dataset to the main dataset
  df = rbind(df, dfsim)
  
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
  regret = 'AverageRegret',
  response = 'InertiaLogit',
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

# add the joint density dataset to the main dataset
df_jd = rbind(df_jd, dfsim_jd)

# remove temporary values
rm(dfsim, dfsim_jd)


##### Data Output #####
write.csv(df, here("Data", "sim_mv.csv"))
write.csv(df_jd, here("Data", "sim_pair_mv.csv"))
