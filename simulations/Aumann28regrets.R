##### Aumann example 2.8 regret funtions #####

# build the payoff function (chicken, can be switched to MV)
payoff = function(my_choice, your_choice){
  return(pay[my_choice, your_choice])
}


##### Average Historical Payoff (regret2) #####
# build the regret function for action m (m is 1,2,or 3) under HM2000
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


# build the decision function based on the regret function under HM2000
decision_avgpay = function(mu, iteration, my_history, your_history){
  
  # get my most recent decision
  lastchoice = my_history[iteration-1]
  
  # compute regret for all possible decisions
  regret1 = regret_avgpay(1, iteration, my_history, your_history)
  regret2 = regret_avgpay(2, iteration, my_history, your_history)
  regret3 = regret_avgpay(3, iteration, my_history, your_history)
  
  # calculate the decision when my last choice is 1
  if (lastchoice == 1){
    if (regret1 == max(regret1, regret2, regret3)){return(1)}
    else{
      # switch with a positive probability from 1 to 2 or from 1 to 3
      seed = runif(1,0,1)
      prob2 = (regret2-regret1)/mu
      prob3 = (regret3-regret1)/mu
      if (seed <= prob2){return(2)}
      else if (seed <= prob2 + prob3){return(3)}
      else{return(1)}
    }
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 2){
    if (regret2 == max(regret1, regret2, regret3)){return(2)}
    else{
      # switch with a positive probability from 2 to 1 or from 2 to 3
      seed = runif(1,0,1)
      prob1 = (regret1-regret2)/mu
      prob3 = (regret3-regret2)/mu
      if (seed <= prob1){return(1)}
      else if (seed <= prob1 + prob3){return(3)}
      else{return(2)}
    }
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 3){
    if (regret3 == max(regret1, regret2, regret3)){return(3)}
    else{
      # switch with a positive probability from 2 to 1 or from 2 to 3
      seed = runif(1,0,1)
      prob1 = (regret1-regret3)/mu
      prob2 = (regret2-regret3)/mu
      if (seed <= prob1){return(1)}
      else if (seed <= prob1 + prob2){return(2)}
      else{return(3)}
    }
  }
}


# build the decision function based on logit response
decision_avgpay_logit = function(mu, beta, iteration, my_history, your_history){
  
  # compute regret for all possible decisions
  regret1 = regret_avgpay(1, iteration, my_history, your_history)
  regret2 = regret_avgpay(2, iteration, my_history, your_history)
  regret3 = regret_avgpay(3, iteration, my_history, your_history)
  
  # using logit response to calculate the probability of choosing each strategy
  prob = c(0,0,0)
  prob[1] = exp(beta*regret1)/(exp(beta*regret1) + exp(beta*regret2) + exp(beta*regret3))
  prob[2] = exp(beta*regret2)/(exp(beta*regret1) + exp(beta*regret2) + exp(beta*regret3))
  prob[3] = exp(beta*regret3)/(exp(beta*regret1) + exp(beta*regret2) + exp(beta*regret3))
  
  # randomly determine the decision
  seed = runif(1,0,1)
  if (seed <= prob[1]){return(c(1, prob[1], prob[2], prob[3]))}
  else if (seed > prob[1] & seed <= prob[1]+prob[2]){return(c(2, prob[1], prob[2], prob[3]))}
  else{return(c(3, prob[1], prob[2], prob[3]))}
}


# build the decision function based on logit response and conditional regret
decision_avgpay_logitR = function(mu, beta, iteration, my_history, your_history){
  
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
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 2){
    
    # using logit response to calculate the probability of choosing each strategy
    prob = c(0,0,0)
    prob[1] = exp(beta*(regret1-regret2))/(exp(beta*(regret1-regret2)) + exp(beta*(regret2-regret2)) + exp(beta*(regret3-regret2)))
    prob[2] = exp(beta*(regret2-regret2))/(exp(beta*(regret1-regret2)) + exp(beta*(regret2-regret2)) + exp(beta*(regret3-regret2)))
    prob[3] = exp(beta*(regret3-regret2))/(exp(beta*(regret1-regret2)) + exp(beta*(regret2-regret2)) + exp(beta*(regret3-regret2)))
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 3){
    
    # using logit response to calculate the probability of choosing each strategy
    prob = c(0,0,0)
    prob[1] = exp(beta*(regret1-regret3))/(exp(beta*(regret1-regret3)) + exp(beta*(regret2-regret3)) + exp(beta*(regret3-regret3)))
    prob[2] = exp(beta*(regret2-regret3))/(exp(beta*(regret1-regret3)) + exp(beta*(regret2-regret3)) + exp(beta*(regret3-regret3)))
    prob[3] = exp(beta*(regret3-regret3))/(exp(beta*(regret1-regret3)) + exp(beta*(regret2-regret3)) + exp(beta*(regret3-regret3)))
  }
  
  # randomly determine the decision
  seed = runif(1,0,1)
  if (seed <= prob[1]){return(1)}
  else if (seed > prob[1] & seed <= prob[1]+prob[2]){return(2)}
  else{return(3)}
}


##### HM2000 revised (regret3) #####
# build the regret function for action m (m is 1,2,or 3) under HM2000 revised algorithm
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


# build the decision function based on the regret function under HM2000 revised algorithm
decision_hm2000r = function(mu, iteration, my_history, your_history){
  
  # get my most recent decision
  lastchoice = my_history[iteration-1]
  
  # compute regret for all possible decisions
  regret1 = regret_hm2000r(1, iteration, my_history, your_history)
  regret2 = regret_hm2000r(2, iteration, my_history, your_history)
  regret3 = regret_hm2000r(3, iteration, my_history, your_history)
  
  # calculate the decision when my last choice is 1
  if (lastchoice == 1){
    if (regret1 == max(regret1, regret2, regret3)){return(1)}
    else{
      # switch with a positive probability from 1 to 2 or from 1 to 3
      seed = runif(1,0,1)
      prob2 = (regret2-regret1)/mu
      prob3 = (regret3-regret1)/mu
      if (seed <= prob2){return(2)}
      else if (seed <= prob2 + prob3){return(3)}
      else{return(1)}
    }
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 2){
    if (regret2 == max(regret1, regret2, regret3)){return(2)}
    else{
      # switch with a positive probability from 2 to 1 or from 2 to 3
      seed = runif(1,0,1)
      prob1 = (regret1-regret2)/mu
      prob3 = (regret3-regret2)/mu
      if (seed <= prob1){return(1)}
      else if (seed <= prob1 + prob3){return(3)}
      else{return(2)}
    }
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 3){
    if (regret3 == max(regret1, regret2, regret3)){return(3)}
    else{
      # switch with a positive probability from 2 to 1 or from 2 to 3
      seed = runif(1,0,1)
      prob1 = (regret1-regret3)/mu
      prob2 = (regret2-regret3)/mu
      if (seed <= prob1){return(1)}
      else if (seed <= prob1 + prob2){return(2)}
      else{return(3)}
    }
  }
}


# build the decision function based on logit response
decision_hm2000r_logit = function(mu, beta, iteration, my_history, your_history){
  
  # compute regret for all possible decisions
  regret1 = regret_hm2000r(1, iteration, my_history, your_history)
  regret2 = regret_hm2000r(2, iteration, my_history, your_history)
  regret3 = regret_hm2000r(3, iteration, my_history, your_history)
  
  # using logit response to calculate the probability of choosing each strategy
  prob = c(0,0,0)
  prob[1] = exp(beta*regret1)/(exp(beta*regret1) + exp(beta*regret2) + exp(beta*regret3))
  prob[2] = exp(beta*regret2)/(exp(beta*regret1) + exp(beta*regret2) + exp(beta*regret3))
  prob[3] = exp(beta*regret3)/(exp(beta*regret1) + exp(beta*regret2) + exp(beta*regret3))
  
  # randomly determine the decision
  seed = runif(1,0,1)
  if (seed <= prob[1]){return(1)}
  else if (seed > prob[1] & seed <= prob[1]+prob[2]){return(2)}
  else{return(3)}
}


# build the decision function based on logit response and conditional regret
decision_hm2000r_logitR = function(mu, beta, iteration, my_history, your_history){
  
  # get my most recent decision
  lastchoice = my_history[iteration-1]
  
  # compute regret for all possible decisions
  regret1 = regret_hm2000r(1, iteration, my_history, your_history)
  regret2 = regret_hm2000r(2, iteration, my_history, your_history)
  regret3 = regret_hm2000r(3, iteration, my_history, your_history)
  
  # calculate the decision when my last choice is 1
  if (lastchoice == 1){
    
    # using logit response to calculate the probability of choosing each strategy
    prob = c(0,0,0)
    prob[1] = exp(beta*(regret1-regret1))/(exp(beta*(regret1-regret1)) + exp(beta*(regret2-regret1)) + exp(beta*(regret3-regret1)))
    prob[2] = exp(beta*(regret2-regret1))/(exp(beta*(regret1-regret1)) + exp(beta*(regret2-regret1)) + exp(beta*(regret3-regret1)))
    prob[3] = exp(beta*(regret3-regret1))/(exp(beta*(regret1-regret1)) + exp(beta*(regret2-regret1)) + exp(beta*(regret3-regret1)))
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 2){
    
    # using logit response to calculate the probability of choosing each strategy
    prob = c(0,0,0)
    prob[1] = exp(beta*(regret1-regret2))/(exp(beta*(regret1-regret2)) + exp(beta*(regret2-regret2)) + exp(beta*(regret3-regret2)))
    prob[2] = exp(beta*(regret2-regret2))/(exp(beta*(regret1-regret2)) + exp(beta*(regret2-regret2)) + exp(beta*(regret3-regret2)))
    prob[3] = exp(beta*(regret3-regret2))/(exp(beta*(regret1-regret2)) + exp(beta*(regret2-regret2)) + exp(beta*(regret3-regret2)))
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 3){
    
    # using logit response to calculate the probability of choosing each strategy
    prob = c(0,0,0)
    prob[1] = exp(beta*(regret1-regret3))/(exp(beta*(regret1-regret3)) + exp(beta*(regret2-regret3)) + exp(beta*(regret3-regret3)))
    prob[2] = exp(beta*(regret2-regret3))/(exp(beta*(regret1-regret3)) + exp(beta*(regret2-regret3)) + exp(beta*(regret3-regret3)))
    prob[3] = exp(beta*(regret3-regret3))/(exp(beta*(regret1-regret3)) + exp(beta*(regret2-regret3)) + exp(beta*(regret3-regret3)))
  }
  
  # randomly determine the decision
  seed = runif(1,0,1)
  if (seed <= prob[1]){return(1)}
  else if (seed > prob[1] & seed <= prob[1]+prob[2]){return(2)}
  else{return(3)}
}


##### Joint distribution #####
# set up the parameters for the simulation
library(xtable)
mu = 3000 # HM2000 probability parameter
n = 500 # number of periods in each simulation
sim = 500 # number of simulations
experiment = 100 # number of experimentation periods where players randomly make decisions
beta = 0.1
pay = matrix(c(600,0,700,0,400,0,200,300,0),3,3)

# set up the joint density matrix
joint_density_all = matrix(c(0,0,0,0,0,0,0,0,0),3,3)

# set up the joint density matrix for each simulation
joint_density = list()

# run the simulations
for (s in 1:sim){
  
  # set up the vectors for choices and game parameters
  history_p1 = rep(0, n)
  history_p2 = rep(0, n)
  
  # calculate the experimentation periods with random starting decisions
  history_p1[1:experiment] = sample(1:3, experiment, replace = TRUE)
  history_p2[1:experiment] = sample(1:3, experiment, replace = TRUE)
  
  # set up the joint density matrix for the current simulation
  joint_density[[s]] = matrix(c(0,0,0,0,0,0,0,0,0),3,3)
  
  # calculate the rest of the decisions to n periods
  for (i in (experiment+1):n){
    history_p1[i] = decision_hm2000r(mu, i, history_p1, history_p2)
    history_p2[i] = decision_hm2000r(mu, i, history_p2, history_p1)
    
    # update the joint density matrix
    if (history_p1[i]==1 & history_p2[i]==1){joint_density[[s]][1,1]=joint_density[[s]][1,1]+1}
    else if (history_p1[i]==1 & history_p2[i]==2){joint_density[[s]][1,2]=joint_density[[s]][1,2]+1}
    else if (history_p1[i]==1 & history_p2[i]==3){joint_density[[s]][1,3]=joint_density[[s]][1,3]+1}
    else if (history_p1[i]==2 & history_p2[i]==1){joint_density[[s]][2,1]=joint_density[[s]][2,1]+1}
    else if (history_p1[i]==2 & history_p2[i]==2){joint_density[[s]][2,2]=joint_density[[s]][2,2]+1}
    else if (history_p1[i]==2 & history_p2[i]==3){joint_density[[s]][2,3]=joint_density[[s]][2,3]+1}
    else if (history_p1[i]==3 & history_p2[i]==1){joint_density[[s]][3,1]=joint_density[[s]][3,1]+1}
    else if (history_p1[i]==3 & history_p2[i]==2){joint_density[[s]][3,2]=joint_density[[s]][3,2]+1}
    else{joint_density[[s]][3,3]=joint_density[[s]][3,3]+1}
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

for (a in 1:sim){
  joint_density_all = joint_density_all + joint_density[[a]]
}
joint_density_all = joint_density_all / sim
xtable(joint_density_all)


