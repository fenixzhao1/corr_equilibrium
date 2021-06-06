##### BM regret funtions #####

# build the payoff function (chicken, can be switched to MV)
payoff = function(my_choice, your_choice){
  return(pay_chicken[my_choice, your_choice])
}


##### HM2000 (regret1) #####
# regret under hm2000
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

# decision under hm2000
decision_hm2000 = function(mu, iteration, my_history, your_history){
  
  # get my most recent decision
  lastchoice = my_history[iteration-1]
  
  # compute regret for all possible decisions
  regret1 = regret_hm2000(1, iteration, my_history, your_history)
  regret2 = regret_hm2000(2, iteration, my_history, your_history)
  
  # calculate the decision when my last choice is 1
  if (lastchoice == 1){
    if (regret1 >= regret2){return(1)}
    else{
      # switch with a positive probability from 1 to 2
      seed = runif(1,0,1)
      prob = (regret2-regret1)/mu
      if (seed <= prob){return(2)}
      else{return(1)}
    }
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 2){
    if (regret1 <= regret2){return(2)}
    else{
      # switch with a positive probability from 2 to 1
      seed = runif(1,0,1)
      prob = (regret1-regret2)/mu
      if (seed <= prob){return(1)}
      else{return(2)}
    }
  }
}

# decision under hm2000 logit spec 1
decision_hm2000_logit = function(mu, beta, iteration, my_history, your_history){
  
  # compute regret for all possible decisions
  regret1 = regret_hm2000(1, iteration, my_history, your_history)
  regret2 = regret_hm2000(2, iteration, my_history, your_history)
  
  # using logit response to calculate the probability of choosing each strategy
  prob = c(0,0)
  prob[1] = exp(beta*regret1)/(exp(beta*regret1) + exp(beta*regret2))
  prob[2] = exp(beta*regret2)/(exp(beta*regret1) + exp(beta*regret2))
  
  # randomly determine the decision
  seed = runif(1,0,1)
  if (seed <= prob[1]){return(1)}
  else{return(2)}
}

# decision under hm2000 logit spec 2
decision_hm2000_logitR = function(mu, beta, iteration, my_history, your_history){
  
  # get my most recent decision
  lastchoice = my_history[iteration-1]
  
  # compute regret for all possible decisions
  regret1 = regret_hm2000(1, iteration, my_history, your_history)
  regret2 = regret_hm2000(2, iteration, my_history, your_history)
  
  # calculate the decision when my last choice is 1
  if (lastchoice == 1){
    
    # using logit response to calculate the probability of choosing each strategy
    prob = c(0,0)
    prob[1] = exp(beta*(regret1-regret1))/(exp(beta*(regret1-regret1)) + exp(beta*(regret2-regret1)))
    prob[2] = exp(beta*(regret2-regret1))/(exp(beta*(regret1-regret1)) + exp(beta*(regret2-regret1)))
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 2){
    
    # using logit response to calculate the probability of choosing each strategy
    prob = c(0,0)
    prob[1] = exp(beta*(regret1-regret2))/(exp(beta*(regret1-regret2)) + exp(beta*(regret2-regret2)))
    prob[2] = exp(beta*(regret2-regret2))/(exp(beta*(regret1-regret2)) + exp(beta*(regret2-regret2)))
  }
  
  # randomly determine the decision
  seed = runif(1,0,1)
  if (seed <= prob[1]){return(1)}
  else{return(2)}
}

# decision under hm2000 inertia logit
decision_hm2000_InertiaLogit = function(mu, beta, Delta, iteration, my_history, your_history){
  
  # get my most recent decision
  lastchoice = my_history[iteration-1]
  
  # compute regret for all possible decisions
  regret1 = regret_hm2000(1, iteration, my_history, your_history)
  regret2 = regret_hm2000(2, iteration, my_history, your_history)
  
  # calculate the decision when my last choice is 1
  if (lastchoice == 1){
    
    # using logit response to calculate the probability of choosing each strategy
    prob = c(0,0)
    prob[2] = exp(beta*(regret2-regret1))*Delta/(exp(beta*(regret1-regret1)) + exp(beta*(regret2-regret1)))
    prob[1] = 1 - prob[2]
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 2){
    
    # using logit response to calculate the probability of choosing each strategy
    prob = c(0,0)
    prob[1] = exp(beta*(regret1-regret2))*Delta/(exp(beta*(regret1-regret2)) + exp(beta*(regret2-regret2)))
    prob[2] = 1 - prob[1]
  }
  
  # randomly determine the decision
  seed = runif(1,0,1)
  if (seed <= prob[1]){return(1)}
  else{return(2)}
}

# decision under hm2000 truncated logit
decision_hm2000_logit_truncate = function(mu, b, iteration, my_history, your_history){
  
  # compute regret for all possible decisions
  regret1 = regret_hm2000(1, iteration, my_history, your_history)
  regret2 = regret_hm2000(2, iteration, my_history, your_history)
  
  # get my most recent decision
  lastchoice = my_history[iteration-1]
  
  # calculate the decision when my last choice is 1
  if (lastchoice == 1){
    
    # using logit response to calculate the probability of choosing each strategy
    exp1 = exp(b[1]*(regret1-regret1)+b[2]*(regret1-regret1)*ifelse(regret1-regret1<0,1,0))
    exp2 = exp(b[1]*(regret2-regret1)+b[2]*(regret2-regret1)*ifelse(regret2-regret1<0,1,0))
    
    prob = c(0,0)
    prob[1] = exp1/(exp1+exp2)
    prob[2] = exp2/(exp1+exp2)
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 2){
    
    # using logit response to calculate the probability of choosing each strategy
    exp1 = exp(b[1]*(regret1-regret2)+b[2]*(regret1-regret2)*ifelse(regret1-regret2<0,1,0))
    exp2 = exp(b[1]*(regret2-regret2)+b[2]*(regret2-regret2)*ifelse(regret2-regret2<0,1,0))
    
    prob = c(0,0)
    prob[1] = exp1/(exp1+exp2)
    prob[2] = exp2/(exp1+exp2)
  }
  
  # randomly determine the decision
  seed = runif(1,0,1)
  if (seed <= prob[1]){return(1)}
  else{return(2)}
}

# decision under hm2000 inertia truncated logit
decision_hm2000_InertiaLogit_truncate = function(mu, b, Delta, iteration, my_history, your_history){
  
  # compute regret for all possible decisions
  regret1 = regret_hm2000(1, iteration, my_history, your_history)
  regret2 = regret_hm2000(2, iteration, my_history, your_history)
  
  # get my most recent decision
  lastchoice = my_history[iteration-1]
  
  # calculate the decision when my last choice is 1
  if (lastchoice == 1){
    
    # using logit response to calculate the probability of choosing each strategy
    exp1 = exp(b[1]*(regret1-regret1)+b[2]*(regret1-regret1)*ifelse(regret1-regret1<0,1,0))
    exp2 = exp(b[1]*(regret2-regret1)+b[2]*(regret2-regret1)*ifelse(regret2-regret1<0,1,0))
    
    prob = c(0,0)
    prob[2] = exp2*Delta/(exp1+exp2)
    prob[1] = 1 - prob[2]
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 2){
    
    # using logit response to calculate the probability of choosing each strategy
    exp1 = exp(b[1]*(regret1-regret2)+b[2]*(regret1-regret2)*ifelse(regret1-regret2<0,1,0))
    exp2 = exp(b[1]*(regret2-regret2)+b[2]*(regret2-regret2)*ifelse(regret2-regret2<0,1,0))
    
    prob = c(0,0)
    prob[1] = exp1/(exp1+exp2)
    prob[2] = 1 - prob[2]
  }
  
  # randomly determine the decision
  seed = runif(1,0,1)
  if (seed <= prob[1]){return(1)}
  else{return(2)}
}


##### Average Historical Payoff (regret2) #####
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

# decision under hist-avg
decision_avgpay = function(mu, iteration, my_history, your_history){
  
  # get my most recent decision
  lastchoice = my_history[iteration-1]
  
  # compute regret for all possible decisions
  regret1 = regret_avgpay(1, iteration, my_history, your_history)
  regret2 = regret_avgpay(2, iteration, my_history, your_history)
  
  # calculate the decision when my last choice is 1
  if (lastchoice == 1){
    if (regret1 >= regret2){return(1)}
    else{
      # switch with a positive probability from 1 to 2
      seed = runif(1,0,1)
      prob = (regret2-regret1)/mu
      if (seed <= prob){return(2)}
      else{return(1)}
    }
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 2){
    if (regret1 <= regret2){return(2)}
    else{
      # switch with a positive probability from 2 to 1
      seed = runif(1,0,1)
      prob = (regret1-regret2)/mu
      if (seed <= prob){return(1)}
      else{return(2)}
    }
  }
}

# decision under hist-avg logit spec 1
decision_avgpay_logit = function(mu, beta, iteration, my_history, your_history){
  
  # compute regret for all possible decisions
  regret1 = regret_avgpay(1, iteration, my_history, your_history)
  regret2 = regret_avgpay(2, iteration, my_history, your_history)
  
  # using logit response to calculate the probability of choosing each strategy
  prob = c(0,0)
  prob[1] = exp(beta*regret1)/(exp(beta*regret1) + exp(beta*regret2))
  prob[2] = exp(beta*regret2)/(exp(beta*regret1) + exp(beta*regret2))
  
  # randomly determine the decision
  seed = runif(1,0,1)
  if (seed <= prob[1]){return(1)}
  else{return(2)}
}

# decision under hist-avg logit spec 2
decision_avgpay_logitR = function(mu, beta, iteration, my_history, your_history){
  
  # get my most recent decision
  lastchoice = my_history[iteration-1]
  
  # compute regret for all possible decisions
  regret1 = regret_avgpay(1, iteration, my_history, your_history)
  regret2 = regret_avgpay(2, iteration, my_history, your_history)
  
  # calculate the decision when my last choice is 1
  if (lastchoice == 1){
    
    # using logit response to calculate the probability of choosing each strategy
    prob = c(0,0)
    prob[1] = exp(beta*(regret1-regret1))/(exp(beta*(regret1-regret1)) + exp(beta*(regret2-regret1)))
    prob[2] = exp(beta*(regret2-regret1))/(exp(beta*(regret1-regret1)) + exp(beta*(regret2-regret1)))
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 2){
    
    # using logit response to calculate the probability of choosing each strategy
    prob = c(0,0)
    prob[1] = exp(beta*(regret1-regret2))/(exp(beta*(regret1-regret2)) + exp(beta*(regret2-regret2)))
    prob[2] = exp(beta*(regret2-regret2))/(exp(beta*(regret1-regret2)) + exp(beta*(regret2-regret2)))
  }
  
  # randomly determine the decision
  seed = runif(1,0,1)
  if (seed <= prob[1]){return(1)}
  else{return(2)}
}

# decision under hist-avg inertia logit
decision_avgpay_InertiaLogit = function(mu, beta, Delta, iteration, my_history, your_history){
  
  # get my most recent decision
  lastchoice = my_history[iteration-1]
  
  # compute regret for all possible decisions
  regret1 = regret_avgpay(1, iteration, my_history, your_history)
  regret2 = regret_avgpay(2, iteration, my_history, your_history)
  
  # calculate the decision when my last choice is 1
  if (lastchoice == 1){
    
    # using logit response to calculate the probability of choosing each strategy
    prob = c(0,0)
    prob[2] = exp(beta*(regret2-regret1))*Delta/(exp(beta*(regret1-regret1)) + exp(beta*(regret2-regret1)))
    prob[1] = 1 - prob[2]
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 2){
    
    # using logit response to calculate the probability of choosing each strategy
    prob = c(0,0)
    prob[1] = exp(beta*(regret1-regret2))*Delta/(exp(beta*(regret1-regret2)) + exp(beta*(regret2-regret2)))
    prob[2] = 1 - prob[1]
  }
  
  # randomly determine the decision
  seed = runif(1,0,1)
  if (seed <= prob[1]){return(1)}
  else{return(2)}
}

# decision under hist-avg truncated logit
decision_avgpay_logit_truncate = function(mu, b, iteration, my_history, your_history){
  
  # compute regret for all possible decisions
  regret1 = regret_avgpay(1, iteration, my_history, your_history)
  regret2 = regret_avgpay(2, iteration, my_history, your_history)
  
  # get my most recent decision
  lastchoice = my_history[iteration-1]
  
  # calculate the decision when my last choice is 1
  if (lastchoice == 1){
    
    # using logit response to calculate the probability of choosing each strategy
    exp1 = exp(b[1]*(regret1-regret1)+b[2]*(regret1-regret1)*ifelse(regret1-regret1<0,1,0))
    exp2 = exp(b[1]*(regret2-regret1)+b[2]*(regret2-regret1)*ifelse(regret2-regret1<0,1,0))
    
    prob = c(0,0)
    prob[1] = exp1/(exp1+exp2)
    prob[2] = exp2/(exp1+exp2)
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 2){
    
    # using logit response to calculate the probability of choosing each strategy
    exp1 = exp(b[1]*(regret1-regret2)+b[2]*(regret1-regret2)*ifelse(regret1-regret2<0,1,0))
    exp2 = exp(b[1]*(regret2-regret2)+b[2]*(regret2-regret2)*ifelse(regret2-regret2<0,1,0))
    
    prob = c(0,0)
    prob[1] = exp1/(exp1+exp2)
    prob[2] = exp2/(exp1+exp2)
  }
  
  # randomly determine the decision
  seed = runif(1,0,1)
  if (seed <= prob[1]){return(1)}
  else{return(2)}
}

# decision under hist-avg inertia truncated logit
decision_avgpay_InertiaLogit_truncate = function(mu, b, Delta, iteration, my_history, your_history){
  
  # compute regret for all possible decisions
  regret1 = regret_avgpay(1, iteration, my_history, your_history)
  regret2 = regret_avgpay(2, iteration, my_history, your_history)
  
  # get my most recent decision
  lastchoice = my_history[iteration-1]
  
  # calculate the decision when my last choice is 1
  if (lastchoice == 1){
    
    # using logit response to calculate the probability of choosing each strategy
    exp1 = exp(b[1]*(regret1-regret1)+b[2]*(regret1-regret1)*ifelse(regret1-regret1<0,1,0))
    exp2 = exp(b[1]*(regret2-regret1)+b[2]*(regret2-regret1)*ifelse(regret2-regret1<0,1,0))
    
    prob = c(0,0)
    prob[2] = exp2*Delta/(exp1+exp2)
    prob[1] = 1 - prob[2]
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 2){
    
    # using logit response to calculate the probability of choosing each strategy
    exp1 = exp(b[1]*(regret1-regret2)+b[2]*(regret1-regret2)*ifelse(regret1-regret2<0,1,0))
    exp2 = exp(b[1]*(regret2-regret2)+b[2]*(regret2-regret2)*ifelse(regret2-regret2<0,1,0))
    
    prob = c(0,0)
    prob[1] = exp1/(exp1+exp2)
    prob[2] = 1 - prob[2]
  }
  
  # randomly determine the decision
  seed = runif(1,0,1)
  if (seed <= prob[1]){return(1)}
  else{return(2)}
}


##### HM2000 revised (regret3) #####
# regret under hm2000 revised
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

# decision under hm2000 revised
decision_hm2000r = function(mu, iteration, my_history, your_history){
  
  # get my most recent decision
  lastchoice = my_history[iteration-1]
  
  # compute regret for all possible decisions
  regret1 = regret_hm2000r(1, iteration, my_history, your_history)
  regret2 = regret_hm2000r(2, iteration, my_history, your_history)
  
  # calculate the decision when my last choice is 1
  if (lastchoice == 1){
    if (regret1 >= regret2){return(1)}
    else{
      # switch with a positive probability from 1 to 2
      seed = runif(1,0,1)
      prob = (regret2-regret1)/mu
      if (seed <= prob){return(2)}
      else{return(1)}
    }
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 2){
    if (regret1 <= regret2){return(2)}
    else{
      # switch with a positive probability from 2 to 1
      seed = runif(1,0,1)
      prob = (regret1-regret2)/mu
      if (seed <= prob){return(1)}
      else{return(2)}
    }
  }
}

# decision under hm2000 logit spec 1
decision_hm2000r_logit = function(mu, beta, iteration, my_history, your_history){
  
  # compute regret for all possible decisions
  regret1 = regret_hm2000r(1, iteration, my_history, your_history)
  regret2 = regret_hm2000r(2, iteration, my_history, your_history)
  
  # using logit response to calculate the probability of choosing each strategy
  prob = c(0,0)
  prob[1] = exp(beta*regret1)/(exp(beta*regret1) + exp(beta*regret2))
  prob[2] = exp(beta*regret2)/(exp(beta*regret1) + exp(beta*regret2))
  
  # randomly determine the decision
  seed = runif(1,0,1)
  if (seed <= prob[1]){return(1)}
  else{return(2)}
}

# decision under hm2000 logit spec 2
decision_hm2000r_logitR = function(mu, beta, iteration, my_history, your_history){
  
  # get my most recent decision
  lastchoice = my_history[iteration-1]
  
  # compute regret for all possible decisions
  regret1 = regret_hm2000r(1, iteration, my_history, your_history)
  regret2 = regret_hm2000r(2, iteration, my_history, your_history)
  
  # calculate the decision when my last choice is 1
  if (lastchoice == 1){
    
    # using logit response to calculate the probability of choosing each strategy
    prob = c(0,0)
    prob[1] = exp(beta*(regret1-regret1))/(exp(beta*(regret1-regret1)) + exp(beta*(regret2-regret1)))
    prob[2] = exp(beta*(regret2-regret1))/(exp(beta*(regret1-regret1)) + exp(beta*(regret2-regret1)))
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 2){
    
    # using logit response to calculate the probability of choosing each strategy
    prob = c(0,0)
    prob[1] = exp(beta*(regret1-regret2))/(exp(beta*(regret1-regret2)) + exp(beta*(regret2-regret2)))
    prob[2] = exp(beta*(regret2-regret2))/(exp(beta*(regret1-regret2)) + exp(beta*(regret2-regret2)))
  }
  
  # randomly determine the decision
  seed = runif(1,0,1)
  if (seed <= prob[1]){return(1)}
  else{return(2)}
}

# decision under hm2000 inertia logit
decision_hm2000r_InertiaLogit = function(mu, beta, Delta, iteration, my_history, your_history){
  
  # get my most recent decision
  lastchoice = my_history[iteration-1]
  
  # compute regret for all possible decisions
  regret1 = regret_hm2000r(1, iteration, my_history, your_history)
  regret2 = regret_hm2000r(2, iteration, my_history, your_history)
  
  # calculate the decision when my last choice is 1
  if (lastchoice == 1){
    
    # using logit response to calculate the probability of choosing each strategy
    prob = c(0,0)
    prob[2] = exp(beta*(regret2-regret1))*Delta/(exp(beta*(regret1-regret1)) + exp(beta*(regret2-regret1)))
    prob[1] = 1 - prob[2]
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 2){
    
    # using logit response to calculate the probability of choosing each strategy
    prob = c(0,0)
    prob[1] = exp(beta*(regret1-regret2))*Delta/(exp(beta*(regret1-regret2)) + exp(beta*(regret2-regret2)))
    prob[2] = 1 - prob[1]
  }
  
  # randomly determine the decision
  seed = runif(1,0,1)
  if (seed <= prob[1]){return(1)}
  else{return(2)}
}

# decision under hm2000 truncated logit
decision_hm2000r_logit_truncate = function(mu, b, iteration, my_history, your_history){
  
  # compute regret for all possible decisions
  regret1 = regret_hm2000r(1, iteration, my_history, your_history)
  regret2 = regret_hm2000r(2, iteration, my_history, your_history)
  
  # get my most recent decision
  lastchoice = my_history[iteration-1]
  
  # calculate the decision when my last choice is 1
  if (lastchoice == 1){
    
    # using logit response to calculate the probability of choosing each strategy
    exp1 = exp(b[1]*(regret1-regret1)+b[2]*(regret1-regret1)*ifelse(regret1-regret1<0,1,0))
    exp2 = exp(b[1]*(regret2-regret1)+b[2]*(regret2-regret1)*ifelse(regret2-regret1<0,1,0))
    
    prob = c(0,0)
    prob[1] = exp1/(exp1+exp2)
    prob[2] = exp2/(exp1+exp2)
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 2){
    
    # using logit response to calculate the probability of choosing each strategy
    exp1 = exp(b[1]*(regret1-regret2)+b[2]*(regret1-regret2)*ifelse(regret1-regret2<0,1,0))
    exp2 = exp(b[1]*(regret2-regret2)+b[2]*(regret2-regret2)*ifelse(regret2-regret2<0,1,0))
    
    prob = c(0,0)
    prob[1] = exp1/(exp1+exp2)
    prob[2] = exp2/(exp1+exp2)
  }
  
  # randomly determine the decision
  seed = runif(1,0,1)
  if (seed <= prob[1]){return(1)}
  else{return(2)}
}

# decision under hm2000 inertia truncated logit
decision_hm2000r_InertiaLogit_truncate = function(mu, b, Delta, iteration, my_history, your_history){
  
  # compute regret for all possible decisions
  regret1 = regret_hm2000r(1, iteration, my_history, your_history)
  regret2 = regret_hm2000r(2, iteration, my_history, your_history)
  
  # get my most recent decision
  lastchoice = my_history[iteration-1]
  
  # calculate the decision when my last choice is 1
  if (lastchoice == 1){
    
    # using logit response to calculate the probability of choosing each strategy
    exp1 = exp(b[1]*(regret1-regret1)+b[2]*(regret1-regret1)*ifelse(regret1-regret1<0,1,0))
    exp2 = exp(b[1]*(regret2-regret1)+b[2]*(regret2-regret1)*ifelse(regret2-regret1<0,1,0))
    
    prob = c(0,0)
    prob[2] = exp2*Delta/(exp1+exp2)
    prob[1] = 1 - prob[2]
  }
  
  # calculate the decision when my last choice is 2
  if (lastchoice == 2){
    
    # using logit response to calculate the probability of choosing each strategy
    exp1 = exp(b[1]*(regret1-regret2)+b[2]*(regret1-regret2)*ifelse(regret1-regret2<0,1,0))
    exp2 = exp(b[1]*(regret2-regret2)+b[2]*(regret2-regret2)*ifelse(regret2-regret2<0,1,0))
    
    prob = c(0,0)
    prob[1] = exp1/(exp1+exp2)
    prob[2] = 1 - prob[2]
  }
  
  # randomly determine the decision
  seed = runif(1,0,1)
  if (seed <= prob[1]){return(1)}
  else{return(2)}
}


##### HM2001 #####
regret_hm2001 = function(m, iteration, my_history, your_history, my_prob){
  
  # calculate the length of history
  len_history = iteration - 1
  
  # create a regret payoff term
  my_regret = 0
  period = 0
  
  # loop over previous periods to calculate the total historical payoff
  for (i in 1:len_history){
    if (my_history[i] == m){
      my_regret = my_regret + payoff(my_history[i], your_history[i])*((my_prob[i])/(1-my_prob[i]))
    }
  }
  
  # return the average historical payoff
  if (period == 0){return(0)}
  else{
    return(my_regret/len_history)
  }  
}

##### HM2001 second version #####
regret_hm2001_new = function(m, iteration, my_history, your_history, my_prob){
  
  # calculate the length of history
  len_history = iteration - 1
  
  # create a regret payoff term
  my_regret = 0
  period = 0
  
  # loop over previous periods to calculate the total historical payoff
  for (i in 1:len_history){
    if (my_history[i] == m){
      my_regret = my_regret + payoff(my_history[i], your_history[i])*(1/(my_prob[i]))
    }
  }
  
  # return the average historical payoff
  if (period == 0){return(0)}
  else{
    return(my_regret/len_history)
  }  
}



# build the regret avgpay function
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
decision_hm2001 = function(mu, delta, gamma, iteration, my_history, your_history, my_prob){
  
  # get my most recent decision
  lastchoice = my_history[iteration-1]
  
  # compute regret for all possible decisions
  regret1 = regret_avgpayr(1, iteration, my_history, your_history)
  regret1m = regret_hm2001(1, iteration, my_history, your_history, 1-my_prob)
  regret2 = regret_avgpayr(2, iteration, my_history, your_history)
  regret2m = regret_hm2001(2, iteration, my_history, your_history, my_prob)
  seed = runif(1,0,1)
  # calculate the decision when my last choice is 1
  if (lastchoice == 1){
    cjk = regret2m - regret1
    q = max(cjk,0)
    psw = (1-delta/(iteration^gamma))*min(q/mu,1)+(delta/(iteration^gamma))*(1/2)
    if (seed <= psw){return(c(2,1-psw))}
    else{return(c(1,1-psw))}
  }
  else{
    cjk = regret1m - regret2
    q = max(cjk,0)
    psw = (1-delta/(iteration^gamma))*min(q/mu,1)+(delta/(iteration^gamma))*(1/2)
    if (seed <= psw){return(c(1,psw))}
    else{return(c(2,psw))}
  }
}    



# build the decision function based on HM2001
decision_logit_hm2001 = function(beta, iteration, my_history, your_history, my_prob){
  
  lastchoice = my_history[iteration-1]
  
  regret1 = regret_avgpayr(1, iteration, my_history, your_history)
  regret1m = regret_hm2001(1, iteration, my_history, your_history, 1-my_prob)
  regret2 = regret_avgpayr(2, iteration, my_history, your_history)
  regret2m = regret_hm2001(2, iteration, my_history, your_history, my_prob)
  seed = runif(1,0,1)
  prob = c(0,0)
  if (lastchoice == 1){
    prob[1] = exp(beta*regret1)/(exp(beta*regret1) + exp(beta*regret2m))
    prob[2] = exp(beta*regret2m)/(exp(beta*regret1) + exp(beta*regret2m))
  }
  else{
    prob[1] = exp(beta*regret1m)/(exp(beta*regret1m) + exp(beta*regret2))
    prob[2] = exp(beta*regret2)/(exp(beta*regret1m) + exp(beta*regret2))
  }
  if (seed <= prob[1]){return(c(1,prob[1]))}
  else{return(c(2,prob[1]))}
}
  