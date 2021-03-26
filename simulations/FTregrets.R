##### FT 2x2x3 game regrets #####

pay_FT1<-list()
pay_FT1[[1]]<-matrix(c(0,100,0,100),2,2)
pay_FT1[[2]]<-matrix(c(200,200,0,200),2,2)
pay_FT1[[3]]<-matrix(c(0,100,0,100),2,2)
pay_FT2<-list()
pay_FT2[[1]]<-matrix(c(100,100,0,0),2,2)
pay_FT2[[2]]<-matrix(c(200,200,0,200),2,2)
pay_FT2[[3]]<-matrix(c(100,100,0,0),2,2)
pay_FT3<-list()
pay_FT3[[1]]<-matrix(c(300,100,0,0),2,2)
pay_FT3[[2]]<-matrix(c(200,0,0,200),2,2)
pay_FT3[[3]]<-matrix(c(0,0,0,300),2,2)

# original payoff matrix
# \hline
# & L & R & L & R & L & R\\ \hline
# U & 0,1,3 & 0,0,0 & 2,2,2 & 0,0,0 & 0,1,0 & 0,0,0 \\ \hline
# D & 1,1,1 & 1,0,0 & 2,2,0 & 2,2,2 & 1,1,0 & 1,0,3 \\ \hline
# P3 & \multicolumn{2}{|c|}{A} & \multicolumn{2}{|c|}{B} & \multicolumn{2}{|c|}{C} \\ \hline





##### Average Historical Payoff (regret2) #####
# build the regret function for action m (m is 1,2,or 3) under average historical payoff
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


# build the decision function based on the regret function under average historical payoff
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


# build the decision function based on logit response and conditional regret
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



##### HM2000 revised (regret3) #####
# build the regret function for action m (m is 1,2,or 3) under hm2000 revised
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


# build the decision function based on the regret function under hm2000 revised
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


# build the decision function based on logit response and conditional regret
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