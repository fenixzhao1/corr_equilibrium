##### FT 2x2x3 game regrets #####

# payoff function
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

payoff = function(role, p1_choice, p2_choice, p3_choice){
  
  if (role == 1){return(pay_FT1[[p3_choice]][p1_choice,p2_choice])}
  else if (role == 2){return(pay_FT2[[p3_choice]][p1_choice,p2_choice])}
  else{return(pay_FT3[[p3_choice]][p1_choice,p2_choice])}
}

# original payoff matrix
# \hline
# & L & R & L & R & L & R\\ \hline
# U & 0,1,3 & 0,0,0 & 2,2,2 & 0,0,0 & 0,1,0 & 0,0,0 \\ \hline
# D & 1,1,1 & 1,0,0 & 2,2,0 & 2,2,2 & 1,1,0 & 1,0,3 \\ \hline
# P3 & \multicolumn{2}{|c|}{A} & \multicolumn{2}{|c|}{B} & \multicolumn{2}{|c|}{C} \\ \hline


##### HM2000 revised (regret3) #####
# build the regret function for a given action (1,2,or 3) under hm2000 revised
regret_hm2000r = function(action, role, iteration, p1_history, p2_history, p3_history){
  
  # calculate the length of history
  len_history = iteration - 1
  
  # create a regret payoff term
  my_regret = 0
  period = 0
  
  # calculate regret conditional on role
  # player 1
  if (role == 1){
    lastchoice = p1_history[len_history]
    for (i in 1:len_history){
      if (p1_history[i] == lastchoice){
        my_regret = my_regret + payoff(role, action, p2_history[i], p3_history[i])
        period = period + 1
      }
    }
  }
  
  # player 2
  else if (role == 2){
    lastchoice = p2_history[len_history]
    for (i in 1:len_history){
      if (p2_history[i] == lastchoice){
        my_regret = my_regret + payoff(role, p1_history[i], action, p3_history[i])
        period = period + 1
      }
    }
  }
  
  # player 3
  else{
    lastchoice = p3_history[len_history]
    for (i in 1:len_history){
      if (p3_history[i] == lastchoice){
        my_regret = my_regret + payoff(role, p1_history[i], p2_history[i], action)
        period = period + 1
      }
    }
  }

  # calculate and return the regret
  return(my_regret/period)
}


# build the decision function based on the regret function under hm2000 revised
decision_hm2000r = function(mu, role, iteration, p1_history, p2_history, p3_history){
  
  len_history = iteration - 1
  
  # calculate decision conditional on role
  # player 1
  if (role == 1){
    lastchoice = p1_history[len_history]
    
    # compute regret for all possible decisions
    regret1 = regret_hm2000r(1, role, iteration, p1_history, p2_history, p3_history)
    regret2 = regret_hm2000r(2, role, iteration, p1_history, p2_history, p3_history)
    
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
  
  # player 2
  else if(role == 2){
    lastchoice = p2_history[len_history]
    
    # compute regret for all possible decisions
    regret1 = regret_hm2000r(1, role, iteration, p1_history, p2_history, p3_history)
    regret2 = regret_hm2000r(2, role, iteration, p1_history, p2_history, p3_history)
    
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
  
  # player 3
  else{
    lastchoice = p3_history[len_history]
    
    # compute regret for all possible decisions
    regret1 = regret_hm2000r(1, role, iteration, p1_history, p2_history, p3_history)
    regret2 = regret_hm2000r(2, role, iteration, p1_history, p2_history, p3_history)
    regret3 = regret_hm2000r(3, role, iteration, p1_history, p2_history, p3_history)
    
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
}


# build the decision function based on logit response and conditional regret
decision_hm2000r_logit = function(mu, role, beta, iteration, p1_history, p2_history, p3_history){
  
  len_history = iteration - 1
  
  # player 1
  if (role == 1){
    lastchoice = p1_history[len_history]
    
    # compute regret for all possible decisions
    regret1 = regret_hm2000r(1, role, iteration, p1_history, p2_history, p3_history)
    regret2 = regret_hm2000r(2, role, iteration, p1_history, p2_history, p3_history)
    
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
  
  # player 2
  else if (role == 2){
    lastchoice = p2_history[len_history]
    
    # compute regret for all possible decisions
    regret1 = regret_hm2000r(1, role, iteration, p1_history, p2_history, p3_history)
    regret2 = regret_hm2000r(2, role, iteration, p1_history, p2_history, p3_history)
    
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
  
  # player 3
  else{
    lastchoice = p3_history[len_history]
    
    # compute regret for all possible decisions
    regret1 = regret_hm2000r(1, role, iteration, p1_history, p2_history, p3_history)
    regret2 = regret_hm2000r(2, role, iteration, p1_history, p2_history, p3_history)
    regret3 = regret_hm2000r(3, role, iteration, p1_history, p2_history, p3_history)
    
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
}


##### Hist Avg (regret2) #####
# build the regret function for a given action (1,2,or 3) under hm2000 revised
regret_avgpay = function(action, role, iteration, p1_history, p2_history, p3_history){
  
  # calculate the length of history
  len_history = iteration - 1
  
  # create a regret payoff term
  my_regret = 0
  period = 0
  
  # calculate regret conditional on role
  # player 1
  if (role == 1){
    for (i in 1:len_history){
      if (p1_history[i] == action){
        my_regret = my_regret + payoff(role, p1_history[i], p2_history[i], p3_history[i])
        period = period + 1
      }
    }
  }
  
  # player 2
  else if (role == 2){
    for (i in 1:len_history){
      if (p2_history[i] == action){
        my_regret = my_regret + payoff(role, p1_history[i], p2_history[i], p3_history[i])
        period = period + 1
      }
    }
  }
  
  # player 3
  else{
    for (i in 1:len_history){
      if (p3_history[i] == action){
        my_regret = my_regret + payoff(role, p1_history[i], p2_history[i], p3_history[i])
        period = period + 1
      }
    }
  }
  
  # calculate and return the regret
  if (period == 0){return(0)}
  else{
    return(my_regret/period)
  }
}


# build the decision function based on the regret function under hm2000 revised
decision_avgpay = function(mu, role, iteration, p1_history, p2_history, p3_history){
  
  len_history = iteration - 1
  
  # calculate decision conditional on role
  # player 1
  if (role == 1){
    lastchoice = p1_history[len_history]
    
    # compute regret for all possible decisions
    regret1 = regret_avgpay(1, role, iteration, p1_history, p2_history, p3_history)
    regret2 = regret_avgpay(2, role, iteration, p1_history, p2_history, p3_history)
    
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
  
  # player 2
  else if(role == 2){
    lastchoice = p2_history[len_history]
    
    # compute regret for all possible decisions
    regret1 = regret_avgpay(1, role, iteration, p1_history, p2_history, p3_history)
    regret2 = regret_avgpay(2, role, iteration, p1_history, p2_history, p3_history)
    
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
  
  # player 3
  else{
    lastchoice = p3_history[len_history]
    
    # compute regret for all possible decisions
    regret1 = regret_avgpay(1, role, iteration, p1_history, p2_history, p3_history)
    regret2 = regret_avgpay(2, role, iteration, p1_history, p2_history, p3_history)
    regret3 = regret_avgpay(3, role, iteration, p1_history, p2_history, p3_history)
    
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
}


# build the decision function based on logit response and conditional regret
decision_avgpay_logit = function(mu, role, beta, iteration, p1_history, p2_history, p3_history){
  
  len_history = iteration - 1
  
  # player 1
  if (role == 1){
    lastchoice = p1_history[len_history]
    
    # compute regret for all possible decisions
    regret1 = regret_avgpay(1, role, iteration, p1_history, p2_history, p3_history)
    regret2 = regret_avgpay(2, role, iteration, p1_history, p2_history, p3_history)
    
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
  
  # player 2
  else if (role == 2){
    lastchoice = p2_history[len_history]
    
    # compute regret for all possible decisions
    regret1 = regret_avgpay(1, role, iteration, p1_history, p2_history, p3_history)
    regret2 = regret_avgpay(2, role, iteration, p1_history, p2_history, p3_history)
    
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
  
  # player 3
  else{
    lastchoice = p3_history[len_history]
    
    # compute regret for all possible decisions
    regret1 = regret_avgpay(1, role, iteration, p1_history, p2_history, p3_history)
    regret2 = regret_avgpay(2, role, iteration, p1_history, p2_history, p3_history)
    regret3 = regret_avgpay(3, role, iteration, p1_history, p2_history, p3_history)
    
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
}


##### Joint distribution #####
mu = 1500 # HM2000 probability parameter
n = 500 # number of periods in each simulation
sim = 500 # number of simulations
experiment = 100 # number of experimentation periods where players randomly make decisions
beta = 0.1

# set up the joint density matrix
joint_density_all = matrix(0,2,6)

# set up the joint density matrix for each simulation
joint_density = list()

# run the simulations
for (s in 1:sim){
  
  # set up the vectors for choices and game parameters
  history_p1 = rep(0, n)
  history_p2 = rep(0, n)
  history_p3 = rep(0, n)
  
  # calculate the experimentation periods with random starting decisions
  history_p1[1:experiment] = sample(1:2, experiment, replace = TRUE)
  history_p2[1:experiment] = sample(1:2, experiment, replace = TRUE)
  history_p3[1:experiment] = sample(1:3, experiment, replace = TRUE)
  
  # set up the joint density matrix for the current simulation
  joint_density[[s]] = matrix(0,2,6)
  
  # calculate the rest of the decisions to n periods
  for (i in (experiment+1):n){
    history_p1[i] = decision_hm2000r(mu, 1, i, history_p1, history_p2, history_p3)
    history_p2[i] = decision_hm2000r(mu, 2, i, history_p1, history_p2, history_p3)
    history_p3[i] = decision_hm2000r(mu, 3, i, history_p1, history_p2, history_p3)
    
    # update the joint density matrix
    if (history_p1[i]==1 & history_p2[i]==1 & history_p3[i]==1)
    {joint_density[[s]][1,1]=joint_density[[s]][1,1]+1}
    else if (history_p1[i]==1 & history_p2[i]==2 & history_p3[i]==1)
    {joint_density[[s]][1,2]=joint_density[[s]][1,2]+1}
    else if (history_p1[i]==2 & history_p2[i]==1 & history_p3[i]==1)
    {joint_density[[s]][2,1]=joint_density[[s]][2,1]+1}
    else if (history_p1[i]==2 & history_p2[i]==2 & history_p3[i]==1)
    {joint_density[[s]][2,2]=joint_density[[s]][2,2]+1}
    
    if (history_p1[i]==1 & history_p2[i]==1 & history_p3[i]==2)
    {joint_density[[s]][1,3]=joint_density[[s]][1,3]+1}
    else if (history_p1[i]==1 & history_p2[i]==2 & history_p3[i]==2)
    {joint_density[[s]][1,4]=joint_density[[s]][1,4]+1}
    else if (history_p1[i]==2 & history_p2[i]==1 & history_p3[i]==2)
    {joint_density[[s]][2,3]=joint_density[[s]][2,3]+1}
    else if (history_p1[i]==2 & history_p2[i]==2 & history_p3[i]==2)
    {joint_density[[s]][2,4]=joint_density[[s]][2,4]+1}
    
    if (history_p1[i]==1 & history_p2[i]==1 & history_p3[i]==3)
    {joint_density[[s]][1,5]=joint_density[[s]][1,5]+1}
    else if (history_p1[i]==1 & history_p2[i]==2 & history_p3[i]==3)
    {joint_density[[s]][1,6]=joint_density[[s]][1,6]+1}
    else if (history_p1[i]==2 & history_p2[i]==1 & history_p3[i]==3)
    {joint_density[[s]][2,5]=joint_density[[s]][2,5]+1}
    else if (history_p1[i]==2 & history_p2[i]==2 & history_p3[i]==3)
    {joint_density[[s]][2,6]=joint_density[[s]][2,6]+1}
  }
  
  # normalize the frequency to probability
  joint_density[[s]] = round(joint_density[[s]]/sum(joint_density[[s]]), 3)
  
  # create the dataset for figures
  df = data.frame(
    p1_choice = history_p1,
    p2_choice = history_p2,
    p3_choice = history_p3,
    period = 1:n
  )
}

for (a in 1:sim){
  joint_density_all = joint_density_all + joint_density[[a]]
}
joint_density_all = joint_density_all / sim
library(xtable)
xtable(joint_density_all)