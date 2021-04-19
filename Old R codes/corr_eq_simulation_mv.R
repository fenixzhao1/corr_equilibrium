##### Packages and Payoff #####
# add package
library(ggplot2)
library(xtable)

# set up the payoff matrix
pay_MV = matrix(c(0,200,100,100,0,200,200,100,0),3,3) # payoff matrix 3x3

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
  prob[1] = exp(beta[1]*regret1)/(exp(beta[1]*regret1) + exp(beta[2]*regret2) + exp(beta[3]*regret3))
  prob[2] = exp(beta[2]*regret2)/(exp(beta[1]*regret1) + exp(beta[2]*regret2) + exp(beta[3]*regret3))
  prob[3] = exp(beta[3]*regret3)/(exp(beta[1]*regret1) + exp(beta[2]*regret2) + exp(beta[3]*regret3))
  
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
  prob[1] = exp(beta[1]*regret1)/(exp(beta[1]*regret1) + exp(beta[2]*regret2) + exp(beta[3]*regret3))
  prob[2] = exp(beta[2]*regret2)/(exp(beta[1]*regret1) + exp(beta[2]*regret2) + exp(beta[3]*regret3))
  prob[3] = exp(beta[3]*regret3)/(exp(beta[1]*regret1) + exp(beta[2]*regret2) + exp(beta[3]*regret3))
  
  # randomly determine the decision
  seed = runif(1,0,1)
  if (seed <= prob[1]){return(c(1, prob[1], prob[2], prob[3]))}
  else if (seed > prob[1] & seed <= prob[1]+prob[2]){return(c(2, prob[1], prob[2], prob[3]))}
  else{return(c(3, prob[1], prob[2], prob[3]))}
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
  prob[1] = exp(beta[1]*regret1)/(exp(beta[1]*regret1) + exp(beta[2]*regret2) + exp(beta[3]*regret3))
  prob[2] = exp(beta[2]*regret2)/(exp(beta[1]*regret1) + exp(beta[2]*regret2) + exp(beta[3]*regret3))
  prob[3] = exp(beta[3]*regret3)/(exp(beta[1]*regret1) + exp(beta[2]*regret2) + exp(beta[3]*regret3))
  
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


##### Simulation pair level graph and joint density - HM2000 and avgpay #####
# set up the parameters for the simulation
mu = 1000 # HM2000 probability parameter
n = 500 # number of periods in each simulation
sim = 500 # number of simulations
experiment = 100 # number of experimentation periods where players randomly make decisions
beta = 1.635

# set up the joint density matrix
joint_density_all = matrix(c(0,0,0,0,0,0,0,0,0),3,3)

# set up the joint density matrix for each simulation
joint_density = list()

# run the simulations
for (s in 1:sim){
  
  # set up the vectors for choices and game parameters
  history_p1 = rep(0,n)
  history_p2 = rep(0,n)

  # set up the joint density matrix for the current simulation
  joint_density[[s]] = matrix(c(0,0,0,0,0,0,0,0,0),3,3)
  
  # calculate the experimentation periods with random starting decisions
  history_p1[1:experiment] = sample(1:3, experiment, replace = TRUE)
  history_p2[1:experiment] = sample(1:3, experiment, replace = TRUE)
  
  # for (h in 1:experiment){
  #   seed = runif(1,0,1)
  #   if (seed <= 0.17){
  #     history_p1[h] = 1
  #     history_p2[h] = 2
  #   }
  #   else if (seed <= 0.33){
  #     history_p1[h] = 2
  #     history_p2[h] = 1
  #   }
  #   else if (seed <= 0.5){
  #     history_p1[h] = 1
  #     history_p2[h] = 3
  #   }
  #   else if (seed <= 0.67){
  #     history_p1[h] = 3
  #     history_p2[h] = 1
  #   }
  #   else if (seed <= 0.83){
  #     history_p1[h] = 2
  #     history_p2[h] = 3
  #   }
  #   else{
  #     history_p1[h] = 3
  #     history_p2[h] = 2
  #   }
  # }
  
  # calculate the rest of the decisions to n periods
  for (i in (experiment+1):n){
    history_p1[i] = decision_hm2000r_logit(mu, c(beta,beta,beta), i, history_p1, history_p2)
    history_p2[i] = decision_hm2000r_logit(mu, c(beta,beta,beta), i, history_p2, history_p1)
    
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
  
  # # graph the decision making
  # title = paste('logit_hm2000r', 'MV', 'sim', as.character(s), sep = '_')
  # file = paste("D:/Dropbox/Working Papers/Correlated Equilibrium/data/simulations/", title, sep = "")
  # file = paste(file, ".png", sep = "")
  # png(file, width = 1500, height = 400)
  # 
  # pic = ggplot(data = df) +
  #   geom_line(aes(x=period, y=p1_choice, colour='blue')) +
  #   geom_line(aes(x=period, y=p2_choice, colour='red')) +
  #   scale_x_discrete(name='period', waiver()) +
  #   scale_y_continuous(name='decision', limits=c(1,3)) +
  #   ggtitle(title) + 
  #   theme_bw() + 
  #   scale_colour_manual(values=c('blue', 'red'), labels=c('p1', 'p2')) +
  #   theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
  #         axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
  #         axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))
  # 
  # print(pic)
  # dev.off()
}

# finalize the joint density matrix
#xtable(joint_density[[1]])

for (a in 1:sim){
  joint_density_all = joint_density_all + joint_density[[a]]
}
joint_density_all = joint_density_all / sim
xtable(joint_density_all)


##### Simulation pair level graph and joint density - HM2001 #####
# set up the parameters for the simulation
mu = 1000 # HM2000 probability parameter
delta = 0.01
gama = 0.0
n = 3000 # number of periods in each simulation
sim = 1 # number of simulations
experiment = 1 # number of experimentation periods where players randomly make decisions

# set up the joint density matrix
joint_density_all = matrix(c(0,0,0,0,0,0,0,0,0),3,3)

# set up the joint density matrix for each simulation
joint_density = list()

# run the simulations
for (s in 1:sim){
  # set up the probability choices for HM2001
  prob1_p1 = rep(0.33, n)
  prob2_p1 = rep(0.33, n)
  prob1_p2 = rep(0.33, n)
  prob2_p2 = rep(0.33, n)
  
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
    history_p1[i] = decision_hm2001(mu, delta, gama, i, history_p1, history_p2, prob1_p1, prob2_p1 )[1]
    history_p2[i] = decision_hm2001(mu, delta, gama, i, history_p2, history_p1, prob1_p2, prob2_p2 )[1]
    prob1_p1[i] = decision_hm2001(mu, delta, gama, i, history_p1, history_p2, prob1_p1, prob2_p1 )[2]
    prob2_p1[i] = decision_hm2001(mu, delta, gama, i, history_p1, history_p2, prob1_p1, prob2_p1 )[3]
    prob1_p2[i] = decision_hm2001(mu, delta, gama, i, history_p2, history_p1, prob1_p2, prob2_p2 )[2]
    prob2_p2[i] = decision_hm2001(mu, delta, gama, i, history_p2, history_p1, prob1_p2, prob2_p2 )[3]
    
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
  
  # graph the decision making
  title = paste('hm2001', 'MV', 'sim', as.character(s), sep = '_')
  file = paste("D:/Dropbox/Working Papers/Correlated Equilibrium/data/simulations/", title, sep = "")
  file = paste(file, ".png", sep = "")
  png(file, width = 1500, height = 400)
  
  pic = ggplot(data = df) +
    geom_line(aes(x=period, y=p1_choice, colour='blue')) +
    geom_line(aes(x=period, y=p2_choice, colour='red')) +
    scale_x_discrete(name='period', waiver()) +
    scale_y_continuous(name='decision', limits=c(1,3)) +
    ggtitle(title) + 
    theme_bw() + 
    scale_colour_manual(values=c('blue', 'red'), labels=c('p1', 'p2')) +
    theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
          axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
          axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))
  
  print(pic)
  dev.off()
}

# finalize the joint density matrix
xtable(joint_density[[1]])

for (a in 1:sim){
  joint_density_all = joint_density_all + joint_density[[a]]
}
joint_density_all = joint_density_all / sim
xtable(joint_density_all, caption = title)


##### Transition between cells #####
# set up the parameters for the simulation
mu = 1000 # HM2000 probability parameter
n = 1000 # number of periods in each simulation
sim = 100 # number of simulations
experiment = 100 # number of experimentation periods where players randomly make decisions
beta = 0.045

# set up the joint density matrix
joint_density_all = matrix(c(0,0,0,0,0,0,0,0,0),3,3)

# set up the joint density matrix for each simulation
joint_density = list()
history_all = list()

# run the simulations
for (s in 1:sim){
  
  # set up the vectors for choices and game parameters
  history_p1 = rep(0, n)
  history_p2 = rep(0, n)
  prob_p1_1 = rep(0, n)
  prob_p1_2 = rep(0, n)
  prob_p1_3 = rep(0, n)
  prob_p2_1 = rep(0, n)
  prob_p2_2 = rep(0, n)
  prob_p2_3 = rep(0, n)
  
  # calculate the experimentation periods with random starting decisions
  history_p1[1:experiment] = sample(1:3, experiment, replace = TRUE)
  history_p2[1:experiment] = sample(1:3, experiment, replace = TRUE)
  
  # set up the joint density matrix for the current simulation
  joint_density[[s]] = matrix(c(0,0,0,0,0,0,0,0,0),3,3)
  
  # calculate the rest of the decisions to n periods
  for (i in (experiment+1):n){
    history_p1[i] = decision_avgpay_logit(mu, c(beta,beta,beta), i, history_p1, history_p2)[1]
    history_p2[i] = decision_avgpay_logit(mu, c(beta,beta,beta), i, history_p2, history_p1)[1]
    prob_p1_1[i] = decision_avgpay_logit(mu, c(beta,beta,beta), i, history_p1, history_p2)[2]
    prob_p1_2[i] = decision_avgpay_logit(mu, c(beta,beta,beta), i, history_p1, history_p2)[3]
    prob_p1_3[i] = decision_avgpay_logit(mu, c(beta,beta,beta), i, history_p1, history_p2)[4]
    prob_p2_1[i] = decision_avgpay_logit(mu, c(beta,beta,beta), i, history_p2, history_p1)[2]
    prob_p2_2[i] = decision_avgpay_logit(mu, c(beta,beta,beta), i, history_p2, history_p1)[3]
    prob_p2_3[i] = decision_avgpay_logit(mu, c(beta,beta,beta), i, history_p2, history_p1)[4]
    
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
    p1_prob_1 = prob_p1_1,
    p1_prob_2 = prob_p1_2,
    p1_prob_3 = prob_p1_3,
    p2_prob_1 = prob_p2_1,
    p2_prob_2 = prob_p2_2,
    p2_prob_3 = prob_p2_3,
    type = rep(NA,n),
    period = 1:n
  )
  
  # update types for profiles
  for (j in 1:n){
    if (df$p1_choice[j] == 1 & df$p2_choice[j] == 1){df$type[j] = 1}
    else if (df$p1_choice[j] == 2 & df$p2_choice[j] == 2){df$type[j] = 2}
    else if (df$p1_choice[j] == 3 & df$p2_choice[j] == 3){df$type[j] = 3}
    else if (df$p1_choice[j] == 1 & df$p2_choice[j] == 3){df$type[j] = 4}
    else if (df$p1_choice[j] == 2 & df$p2_choice[j] == 1){df$type[j] = 5}
    else if (df$p1_choice[j] == 3 & df$p2_choice[j] == 2){df$type[j] = 6}
    else if (df$p1_choice[j] == 1 & df$p2_choice[j] == 2){df$type[j] = 7}
    else if (df$p1_choice[j] == 2 & df$p2_choice[j] == 3){df$type[j] = 8}
    else{df$type[j] = 9}
  }
  history_all[[s]] = df
  
}

# finalize the joint density matrix
for (a in 1:sim){
  joint_density_all = joint_density_all + joint_density[[a]]
}
joint_density_all = joint_density_all / sim
xtable(joint_density_all)

# set up the transition matrix
transition = matrix(0, nrow = 3, ncol = 5)
rownames(transition) = c('diagonal at t', 'p1adv at t', 'p2adv at t')
colnames(transition) = c('stay at t+1', 'diagonal at t+1', 'p1adv at t+1', 'p2adv at t+1', 'obs')

# loop over simulations
for (m in 1:sim){
  df_sim = history_all[[m]]
  
  # loop over periods
  for (k in (experiment+1):n){
    
    # transitions from diagonal
    if (df_sim$type[k-1] <= 3){
      if (df_sim$type[k] == df_sim$type[k-1]){transition[1,1] = transition[1,1] + 1}
      else if (df_sim$type[k] <= 3){transition[1,2] = transition[1,2] + 1}
      else if (df_sim$type[k] >= 7){transition[1,4] = transition[1,4] + 1}
      else{transition[1,3] = transition[1,3] + 1}
    }
    # transitions from p2adv
    else if (df_sim$type[k-1] >= 7){
      if (df_sim$type[k] == df_sim$type[k-1]){transition[3,1] = transition[3,1] + 1}
      else if (df_sim$type[k] <= 3){transition[3,2] = transition[3,2] + 1}
      else if (df_sim$type[k] >= 7){transition[3,4] = transition[3,4] + 1}
      else{transition[3,3] = transition[3,3] + 1}
    }
    # transitions from p1 adv
    else{
      if (df_sim$type[k] == df_sim$type[k-1]){transition[2,1] = transition[2,1] + 1}
      else if (df_sim$type[k] <= 3){transition[2,2] = transition[2,2] + 1}
      else if (df_sim$type[k] >= 7){transition[2,4] = transition[2,4] + 1}
      else{transition[2,3] = transition[2,3] + 1}
    }
  }
}
  
# calculation transition probability matrix
transition_prob = transition
for (b in 1:3){
  transition_prob[b,1] = round(transition[b,1] / sum(transition[b,1:4]), 2)
  transition_prob[b,2] = round(transition[b,2] / sum(transition[b,1:4]), 2)
  transition_prob[b,3] = round(transition[b,3] / sum(transition[b,1:4]), 2)
  transition_prob[b,4] = round(transition[b,4] / sum(transition[b,1:4]), 2)
  transition_prob[b,5] = sum(transition[b,1:4])
}
xtable(transition_prob)


  
  
  