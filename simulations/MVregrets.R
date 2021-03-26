##### MV regret funtions #####

# build the payoff function (chicken, can be switched to MV)
payoff = function(my_choice, your_choice){
  return(pay_MV[my_choice, your_choice])
}


##### HM2000 (regret1) #####
# build the regret function for action m (m is 1,2,or 3) under HM2000
regret_hm2000 = function(m, iteration, my_history, your_history){
  
  # calculate the length of history
  len_history = iteration - 1
  
  # get my most recent decision
  lastchoice = my_history[len_history]
  
  # create a regret payoff term
  my_regret = rep(0, len_history)
  
  # loop over previous periods to replace the choice with decision 1
  for (i in 1:len_history){
    if (my_history[i] == lastchoice){
      my_regret[i] = payoff(m, your_history[i])
    }
    else{
      my_regret[i] = payoff(my_history[i], your_history[i])
    }
  }
  
  # calculate and return the regret
  return(mean(my_regret))
}


# build the decision function based on the regret function under HM2000
decision_hm2000 = function(mu, iteration, my_history, your_history){
  
  # get my most recent decision
  lastchoice = my_history[iteration-1]
  
  # compute regret for all possible decisions
  regret1 = regret_hm2000(1, iteration, my_history, your_history)
  regret2 = regret_hm2000(2, iteration, my_history, your_history)
  regret3 = regret_hm2000(3, iteration, my_history, your_history)
  
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
decision_hm2000_logit = function(mu, beta, iteration, my_history, your_history){
  
  # compute regret for all possible decisions
  regret1 = regret_hm2000(1, iteration, my_history, your_history)
  regret2 = regret_hm2000(2, iteration, my_history, your_history)
  regret3 = regret_hm2000(3, iteration, my_history, your_history)
  
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
decision_hm2000_logitR = function(mu, beta, iteration, my_history, your_history){
  
  # get my most recent decision
  lastchoice = my_history[iteration-1]
  
  # compute regret for all possible decisions
  regret1 = regret_hm2000(1, iteration, my_history, your_history)
  regret2 = regret_hm2000(2, iteration, my_history, your_history)
  regret3 = regret_hm2000(3, iteration, my_history, your_history)
  
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


##### HM2001 #####
regret_hm2001 = function(m, iteration, my_history, your_history, my_prob_last, my_prob_other){
  
  # calculate the length of history
  len_history = iteration - 1
  
  # create a regret payoff term
  my_regret = 0
  period = 0
  
  # loop over previous periods to calculate the total historical payoff
  for (i in 1:len_history){
    if (my_history[i] == m){
      my_regret = my_regret + payoff(my_history[i], your_history[i])*((my_prob_last)/(my_prob_other))
    }
  }
  
  # return the average historical payoff
  if (period == 0){return(0)}
  else{
    return(my_regret/len_history)
  }  
}


# build the revised avgpay
regret_avgpayr = function(m, iteration, my_history, your_history){
  
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
    return(my_regret/len_history)
  }
}


# build the decision function based on HM2001
decision_hm2001 = function(mu, delta, gamma, iteration, my_history, your_history, my_prob_1,  my_prob_2){
  
  # get my most recent decision
  lastchoice = my_history[iteration-1]
  
  # compute regret for all possible decisions
  regret1 = regret_avgpayr(1, iteration, my_history, your_history)
  regret2 = regret_avgpayr(2, iteration, my_history, your_history)
  regret3 = regret_avgpayr(3, iteration, my_history, your_history)
  regret2m1 = regret_hm2001(2, iteration, my_history, your_history, my_prob_1, my_prob_2)
  regret3m1 = regret_hm2001(3, iteration, my_history, your_history, my_prob_1, 1-my_prob_1-my_prob_2)
  regret1m2 = regret_hm2001(1, iteration, my_history, your_history, my_prob_2,  my_prob_1)
  regret3m2 = regret_hm2001(3, iteration, my_history, your_history, my_prob_2, 1-my_prob_1-my_prob_2)
  regret1m3 = regret_hm2001(1, iteration, my_history, your_history, 1-my_prob_1-my_prob_2,  my_prob_1)
  regret2m3 = regret_hm2001(2, iteration, my_history, your_history, 1-my_prob_1-my_prob_2, my_prob_2)
  
  seed = runif(1,0,1)
  # calculate the decision when my last choice is 1
  if (lastchoice == 1){
    cj2 = regret2m1 - regret1
    q2 = max(cj2,0)
    psw2 = (1-delta/(iteration^gamma))*min(q2/mu,1/2)+(delta/(iteration^gamma))*(1/3)
    
    cj3 = regret3m1 - regret1
    q3 = max(cj3,0)
    psw3 = (1-delta/(iteration^gamma))*min(q3/mu,1/2)+(delta/(iteration^gamma))*(1/3)
    
    if (seed <= psw2){return(c(2,1-psw2-psw3,psw2))}
    else if (seed>psw2 & seed<=psw3+psw2){return(c(3,1-psw2-psw3,psw2))}
    else{return(c(1,1-psw2-psw3,psw2))}
  }
  else if (lastchoice == 2){
    cj3 = regret3m2 - regret2
    q3 = max(cj3,0)
    psw3 = (1-delta/(iteration^gamma))*min(q3/mu,1/2)+(delta/(iteration^gamma))*(1/3)
    
    cj1 = regret1m2 - regret2
    q1 = max(cj1,0)
    psw1 = (1-delta/(iteration^gamma))*min(q1/mu,1/2)+(delta/(iteration^gamma))*(1/3)
    
    if (seed <= psw1){return(c(1,psw1,1-psw3-psw1))}
    else if (seed>psw1 & seed<=psw3+psw1){return(c(3,psw1,1-psw3-psw1))}
    else{return(c(2,psw1,1-psw3-psw1))}
  }
  else {
    cj2 = regret2m3 - regret3
    q2 = max(cj2,0)
    psw2 = (1-delta/(iteration^gamma))*min(q2/mu,1/2)+(delta/(iteration^gamma))*(1/3)
    
    cj1 = regret1m3 - regret3
    q1 = max(cj1,0)
    psw1 = (1-delta/(iteration^gamma))*min(q1/mu,1/2)+(delta/(iteration^gamma))*(1/3)
    
    if (seed <= psw1){return(c(1,psw1,psw2))}
    else if (seed>psw1 & seed<= psw2+psw1){return(c(2,psw1,psw2))}
    else{return(c(3,psw1,psw2))}
  }
}    



# build the decision function based on HM2001
decision_logit_hm2001 = function(beta, iteration, my_history, your_history, my_prob_1,  my_prob_2){
  
  lastchoice = my_history[iteration-1]
  
  # compute regret for all possible decisions
  regret1 = regret_avgpayr(1, iteration, my_history, your_history)
  regret2 = regret_avgpayr(2, iteration, my_history, your_history)
  regret3 = regret_avgpayr(3, iteration, my_history, your_history)
  regret2m1 = regret_hm2001(2, iteration, my_history, your_history, my_prob_1, my_prob_2)
  regret3m1 = regret_hm2001(3, iteration, my_history, your_history, my_prob_1, 1-my_prob_1-my_prob_2)
  regret1m2 = regret_hm2001(1, iteration, my_history, your_history, my_prob_2,  my_prob_1)
  regret3m2 = regret_hm2001(3, iteration, my_history, your_history, my_prob_2, 1-my_prob_1-my_prob_2)
  regret1m3 = regret_hm2001(1, iteration, my_history, your_history, 1-my_prob_1-my_prob_2,  my_prob_1)
  regret2m3 = regret_hm2001(2, iteration, my_history, your_history, 1-my_prob_1-my_prob_2, my_prob_2)
  
  seed = runif(1,0,1)
  prob = c(0,0,0)
  if (lastchoice == 1){
    prob[1] = exp(beta*regret1)/(exp(beta*regret1) + exp(beta*regret2m1)+ exp(beta*regret3m1))
    prob[2] = exp(beta*regret2m1)/(exp(beta*regret1) + exp(beta*regret2m1)+ exp(beta*regret3m1))
  }
  else if (lastchoice == 2){
    prob[1] = exp(beta*regret1m2)/(exp(beta*regret1m2) + exp(beta*regret2)+ exp(beta*regret3m2))
    prob[2] = exp(beta*regret2)/(exp(beta*regret1m2) + exp(beta*regret2)+ exp(beta*regret3m2))
  }
  else{
    prob[1] = exp(beta*regret1m3)/(exp(beta*regret1m3) + exp(beta*regret2m3)+ exp(beta*regret3))
    prob[2] = exp(beta*regret2m3)/(exp(beta*regret1m3) + exp(beta*regret2m3)+ exp(beta*regret3))
  }
  if (seed <= prob[1]){return(c(1,prob[1],prob[2]))}
  else if (seed <= prob[1]+prob[2]){return(c(2,prob[1],prob[2]))}
  else{return(c(3,prob[1],prob[2]))}
}












