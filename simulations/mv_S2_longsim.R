##### CDF of S2 (the sum of the probability density of the most two frequently-played cells) #####
# add packages
library(here)
library(ggplot2)
dfmv<-read.csv(here("Data/sim_pair100_mv.csv"), header=T, stringsAsFactors = FALSE)

# draw cdf
for (i in 1:length(dfmv$sim)){
  jd = c(dfmv$jd_11[i],dfmv$jd_12[i],dfmv$jd_13[i],dfmv$jd_21[i],dfmv$jd_22[i],
         dfmv$jd_23[i],dfmv$jd_31[i],dfmv$jd_32[i],dfmv$jd_33[i])
  first = max(jd)
  second = max(jd[-which.max(jd)])
  dfmv$s2[i] = first+second
}

# plot cdf of s2
png(here("Figures/sim_s2_cdf.png"), width = 500, height = 300)
ggplot(data=dfmv) +
  stat_ecdf(geom='step', aes(x=s2, colour=regret, linetype=response)) +
  #geom_density(aes(x=s2, color=regret, linetype=response)) +
  scale_y_continuous(name='CDF', waiver(), limits=c(0,1)) +
  scale_x_continuous(name='the sum of the probability density of the most two frequently-played cells') +
  scale_colour_manual(values=c('blue', 'red', 'green')) +
  scale_linetype_manual(values=c(1,2,3))
dev.off()


##### One trail of long simulation in MV #####
# add package
rm(list = ls())
library(ggplot2)
library(xtable)
library(dplyr)
library(here)

# build the payoff function (chicken, can be switched to MV)
payoff = function(my_choice, your_choice){
  return(pay_MV[my_choice, your_choice])
}

# regret under HM2000 revised
regret_hm2000r = function(m, iteration, my_history, your_history){
  
  # calculate the length of history
  len_history = iteration - 1
  
  # get my most recent decision
  lastchoice = my_history[len_history]
  
  # create a regret payoff term
  my_regret = 0
  period = 0
  
  # loop over previous periods to replace the choice with decision 1
  for (i in 1:len_history){
    if (my_history[i] == lastchoice){
      my_regret = my_regret + payoff(m, your_history[i])
      period = period + 1
    }
  }
  
  # calculate and return the regret
  return(my_regret/period)
}

# regret under hist-avg
regret_avgpay = function(m, iteration, my_history, your_history){
  
  # calculate the length of history
  len_history = iteration - 1
  
  # create a regret payoff term
  my_regret = 0
  period = 0
  
  # loop over previous periods to calculate the total historical payoff
  for (i in 1:len_history){
    if (my_history[i] == m){
      my_regret = my_regret + payoff(my_history[i], your_history[i])
      period = period + 1
    }
  }
  
  # return the average historical payoff
  if (period == 0){return(0)}
  else{
    return(my_regret/period)
  }
}

# decision under HM response
decision_hm2000r = function(mu, iteration, my_history, your_history){
  
  # get my most recent decision
  lastchoice = my_history[iteration-1]
  
  # compute regret for all possible decisions
  regret1 = regret_avgpay(1, iteration, my_history, your_history)
  regret2 = regret_avgpay(2, iteration, my_history, your_history)
  regret3 = regret_avgpay(3, iteration, my_history, your_history)
  
  # calculate the decision when my last choice is 1
  if (lastchoice == 1){
    if (regret1 == max(regret1, regret2, regret3)){return(c(1,max(regret2-regret1,regret3-regret1),min(regret2-regret1,regret3-regret1)))}
    else{
      # switch with a positive probability from 1 to 2 or from 1 to 3
      seed = runif(1,0,1)
      prob2 = max((regret2-regret1),0)/mu
      prob3 = max((regret3-regret1),0)/mu
      if (seed <= prob2){return(c(2,max(regret2-regret1,regret3-regret1),min(regret2-regret1,regret3-regret1)))}
      else if (seed <= prob2 + prob3){return(c(3,max(regret2-regret1,regret3-regret1),min(regret2-regret1,regret3-regret1)))}
      else{return(c(1,max(regret2-regret1,regret3-regret1),min(regret2-regret1,regret3-regret1)))}
    }
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 2){
    if (regret2 == max(regret1, regret2, regret3)){return(c(2,max(regret1-regret2,regret3-regret2),min(regret1-regret2,regret3-regret2)))}
    else{
      # switch with a positive probability from 2 to 1 or from 2 to 3
      seed = runif(1,0,1)
      prob1 = max((regret1-regret2),0)/mu
      prob3 = max((regret3-regret2),0)/mu
      if (seed <= prob1){return(c(1,max(regret1-regret2,regret3-regret2),min(regret1-regret2,regret3-regret2)))}
      else if (seed <= prob1 + prob3){return(c(3,max(regret1-regret2,regret3-regret2),min(regret1-regret2,regret3-regret2)))}
      else{return(c(2,max(regret1-regret2,regret3-regret2),min(regret1-regret2,regret3-regret2)))}
    }
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 3){
    if (regret3 == max(regret1, regret2, regret3)){return(c(3,max(regret1-regret3,regret2-regret3),min(regret1-regret3,regret2-regret3)))}
    else{
      # switch with a positive probability from 2 to 1 or from 2 to 3
      seed = runif(1,0,1)
      prob1 = max((regret1-regret3),0)/mu
      prob2 = max((regret2-regret3),0)/mu
      if (seed <= prob1){return(c(1,max(regret1-regret3,regret2-regret3),min(regret1-regret3,regret2-regret3)))}
      else if (seed <= prob1 + prob2){return(c(2,max(regret1-regret3,regret2-regret3),min(regret1-regret3,regret2-regret3)))}
      else{return(c(3,max(regret1-regret3,regret2-regret3),min(regret1-regret3,regret2-regret3)))}
    }
  }
}

# decision under logit response
decision_hm2000r_logitR = function(mu, beta, iteration, my_history, your_history){
  
  # get my most recent decision
  lastchoice = my_history[iteration-1]
  
  # compute regret for all possible decisions
  regret1 = regret_avgpay(1, iteration, my_history, your_history)
  regret2 = regret_avgpay(2, iteration, my_history, your_history)
  regret3 = regret_avgpay(3, iteration, my_history, your_history)
  
  # calculate the decision when my last choice is 1
  if (lastchoice == 1){
    
    # using logit response to calculate the probability of choosing each strategy
    prob = c(0,0,0)
    prob[1] = exp(beta*(regret1-regret1))/(exp(beta*(regret1-regret1)) + exp(beta*(regret2-regret1)) + exp(beta*(regret3-regret1)))
    prob[2] = exp(beta*(regret2-regret1))/(exp(beta*(regret1-regret1)) + exp(beta*(regret2-regret1)) + exp(beta*(regret3-regret1)))
    prob[3] = exp(beta*(regret3-regret1))/(exp(beta*(regret1-regret1)) + exp(beta*(regret2-regret1)) + exp(beta*(regret3-regret1)))
  
    # randomly determine the decision
    seed = runif(1,0,1)
    if (seed <= prob[1]){return(c(1,max(regret2-regret1,regret3-regret1),min(regret2-regret1,regret3-regret1)))}
    else if (seed > prob[1] & seed <= prob[1]+prob[2]){return(c(2,max(regret2-regret1,regret3-regret1),min(regret2-regret1,regret3-regret1)))}
    else{return(c(3,max(regret2-regret1,regret3-regret1),min(regret2-regret1,regret3-regret1)))}
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 2){
    
    # using logit response to calculate the probability of choosing each strategy
    prob = c(0,0,0)
    prob[1] = exp(beta*(regret1-regret2))/(exp(beta*(regret1-regret2)) + exp(beta*(regret2-regret2)) + exp(beta*(regret3-regret2)))
    prob[2] = exp(beta*(regret2-regret2))/(exp(beta*(regret1-regret2)) + exp(beta*(regret2-regret2)) + exp(beta*(regret3-regret2)))
    prob[3] = exp(beta*(regret3-regret2))/(exp(beta*(regret1-regret2)) + exp(beta*(regret2-regret2)) + exp(beta*(regret3-regret2)))
  
    # randomly determine the decision
    seed = runif(1,0,1)
    if (seed <= prob[1]){return(c(1,max(regret1-regret2,regret3-regret2),min(regret1-regret2,regret3-regret2)))}
    else if (seed > prob[1] & seed <= prob[1]+prob[2]){return(c(2,max(regret1-regret2,regret3-regret2),min(regret1-regret2,regret3-regret2)))}
    else{return(c(3,max(regret1-regret2,regret3-regret2),min(regret1-regret2,regret3-regret2)))}
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 3){
    
    # using logit response to calculate the probability of choosing each strategy
    prob = c(0,0,0)
    prob[1] = exp(beta*(regret1-regret3))/(exp(beta*(regret1-regret3)) + exp(beta*(regret2-regret3)) + exp(beta*(regret3-regret3)))
    prob[2] = exp(beta*(regret2-regret3))/(exp(beta*(regret1-regret3)) + exp(beta*(regret2-regret3)) + exp(beta*(regret3-regret3)))
    prob[3] = exp(beta*(regret3-regret3))/(exp(beta*(regret1-regret3)) + exp(beta*(regret2-regret3)) + exp(beta*(regret3-regret3)))
  
    # randomly determine the decision
    seed = runif(1,0,1)
    if (seed <= prob[1]){return(c(1,max(regret1-regret3,regret2-regret3),min(regret1-regret3,regret2-regret3)))}
    else if (seed > prob[1] & seed <= prob[1]+prob[2]){return(c(2,max(regret1-regret3,regret2-regret3),min(regret1-regret3,regret2-regret3)))}
    else{return(c(3,max(regret1-regret3,regret2-regret3),min(regret1-regret3,regret2-regret3)))}
  }
}

# set up the parameters for the simulation
pay_MV = matrix(c(0,200,100,100,0,200,200,100,0),3,3) # payoff matrix 3x3
n = 20000 # number of periods in each simulation
experiment = 50 # number of experimentation periods where players randomly make decisions
mu = 600 # HM response parameter
beta = 1 # logit response parameter
Delta = 0.8 # inertia logit parameter

# set up the vectors for choices and game parameters
history_p1 = rep(0, n)
history_p2 = rep(0, n)
regretmax_p1 = rep(0, n)
regretmax_p2 = rep(0, n)
regretmin_p1 = rep(0, n)
regretmin_p2 = rep(0, n)

# calculate the experimentation periods with random starting decisions
history_p1[1:experiment] = sample(1:3, experiment, replace = TRUE)
history_p2[1:experiment] = sample(1:3, experiment, replace = TRUE)

# calculate the rest of the decisions to n periods
for (j in (experiment+1):n){
  history_p1[j] = decision_hm2000r_logitR(mu, beta, j, history_p1, history_p2)[1]
  history_p2[j] = decision_hm2000r_logitR(mu, beta, j, history_p2, history_p1)[1]
  regretmax_p1[j] = decision_hm2000r_logitR(mu, beta, j, history_p1, history_p2)[2]
  regretmax_p2[j] = decision_hm2000r_logitR(mu, beta, j, history_p2, history_p1)[2]
  regretmin_p1[j] = decision_hm2000r_logitR(mu, beta, j, history_p1, history_p2)[3]
  regretmin_p2[j] = decision_hm2000r_logitR(mu, beta, j, history_p2, history_p1)[3]
  
  if (j%%1000==0){print(j)}
}

# create the dataset for this simulation
dfsim = data.frame(
  p1_choice = history_p1[(experiment+1):n],
  p2_choice = history_p2[(experiment+1):n],
  p1_regretmax = regretmax_p1[(experiment+1):n],
  p2_regretmax = regretmax_p2[(experiment+1):n],
  p1_regretmin = regretmin_p1[(experiment+1):n],
  p2_regretmin = regretmin_p2[(experiment+1):n],
  period = (experiment+1):n
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

# # calculate joint density
# jd = matrix(0, nrow = 3, ncol = 3)
# 
# # record the joint density
# jd[1,1] = round(mean(dfsim$is_11),3)
# jd[1,2] = round(mean(dfsim$is_12),3)
# jd[1,3] = round(mean(dfsim$is_13),3)
# jd[2,1] = round(mean(dfsim$is_21),3)
# jd[2,2] = round(mean(dfsim$is_22),3)
# jd[2,3] = round(mean(dfsim$is_23),3)
# jd[3,1] = round(mean(dfsim$is_31),3)
# jd[3,2] = round(mean(dfsim$is_32),3)
# jd[3,3] = round(mean(dfsim$is_33),3)
# print(jd)
# 
# # pick a subset of periods.
# df = filter(dfsim, period>20000&period<=30000)
# jd = matrix(0, nrow = 3, ncol = 3)
# jd[1,1] = round(mean(df$is_11),3)
# jd[1,2] = round(mean(df$is_12),3)
# jd[1,3] = round(mean(df$is_13),3)
# jd[2,1] = round(mean(df$is_21),3)
# jd[2,2] = round(mean(df$is_22),3)
# jd[2,3] = round(mean(df$is_23),3)
# jd[3,1] = round(mean(df$is_31),3)
# jd[3,2] = round(mean(df$is_32),3)
# jd[3,3] = round(mean(df$is_33),3)
# print(jd)

# plot action
png(here("Figures/sim_MVlongavg_logit_action.png"), width = 800, height = 400)
ggplot(data=dfsim) +
  geom_line(aes(x=period,y=p1_choice, colour='blue')) +
  geom_line(aes(x=period,y=p2_choice, colour='red')) +
  scale_y_continuous(name='choice', waiver(), limits=c(1,3), breaks = c(1,2,3),
                     labels = c('T/L','M/C','B/R')) +
  scale_x_continuous(name='periods', waiver(), limits=c(0,20000), breaks = c(100,1000,5000,10000,20000)) +
  scale_colour_manual(values=c('blue', 'red'), labels=c('row player', 'column player'))
dev.off()

# plot regret
png(here("Figures/sim_MVlongavg_logit_regret.png"), width = 800, height = 400)
ggplot(data=dfsim) +
  geom_line(aes(x=period,y=p1_regretmax, colour='blue')) +
  #geom_line(aes(x=period,y=p1_regretmin, colour='blue')) +
  geom_line(aes(x=period,y=p2_regretmax, colour='red')) +
  #geom_line(aes(x=period,y=p2_regretmin, colour='red')) +
  scale_y_continuous(name='regret', waiver(), limits=c(-120,120), breaks = c(-100,0,100)) +
  scale_x_continuous(name='periods', waiver(), limits=c(0,20000), breaks = c(100,1000,5000,10000,20000)) +
  scale_colour_manual(values=c('blue', 'red'), labels=c('row player', 'column player'))
dev.off()

# # export data
# dfsim = dfsim %>% mutate(
#   regret = 'C',
#   response = 'HM'
# )
# write.csv(dfsim, here("Data", "sim_mvlong_c.csv"))
