##### Package and Payoff #####
# add package
library(ggplot2)
library(xtable)

# set up the payoff matrix
pay_chicken = matrix(c(100,200,600,500),2,2) # payoff matrix 2x2

# build the payoff function (chicken, can be switched to MV)
payoff = function(my_choice, your_choice){
  return(pay_chicken[my_choice, your_choice])
}


##### HM2000 (regret1) #####
# build the regret function for action m (m is 1,2,or 3) under hm2000
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


# build the decision function based on the regret function under hm2000
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


# build the decision function based on logit response
decision_hm2000_logit = function(mu, beta, iteration, my_history, your_history){
  
  # compute regret for all possible decisions
  regret1 = regret_hm2000(1, iteration, my_history, your_history)
  regret2 = regret_hm2000(2, iteration, my_history, your_history)
  
  # using logit response to calculate the probability of choosing each strategy
  prob = c(0,0)
  prob[1] = exp(beta[1]*regret1)/(exp(beta[1]*regret1) + exp(beta[2]*regret2))
  prob[2] = exp(beta[2]*regret2)/(exp(beta[1]*regret1) + exp(beta[2]*regret2))
  
  # randomly determine the decision
  seed = runif(1,0,1)
  if (seed <= prob[1]){return(1)}
  else{return(2)}
}


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


# build the decision function based on logit response
decision_avgpay_logit = function(mu, beta, iteration, my_history, your_history){
  
  # compute regret for all possible decisions
  regret1 = regret_avgpay(1, iteration, my_history, your_history)
  regret2 = regret_avgpay(2, iteration, my_history, your_history)
  
  # using logit response to calculate the probability of choosing each strategy
  prob = c(0,0)
  prob[1] = exp(beta[1]*regret1)/(exp(beta[1]*regret1) + exp(beta[2]*regret2))
  prob[2] = exp(beta[2]*regret2)/(exp(beta[1]*regret1) + exp(beta[2]*regret2))
  
  # randomly determine the decision
  seed = runif(1,0,1)
  if (seed <= prob[1]){return(c(1,prob[1],prob[2]))}
  else{return(c(2,prob[1], prob[2]))}
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


# build the decision function based on logit response
decision_hm2000r_logit = function(mu, beta, iteration, my_history, your_history){
  
  # compute regret for all possible decisions
  regret1 = regret_hm2000r(1, iteration, my_history, your_history)
  regret2 = regret_hm2000r(2, iteration, my_history, your_history)
  
  # using logit response to calculate the probability of choosing each strategy
  prob = c(0,0)
  prob[1] = exp(beta[1]*regret1)/(exp(beta[1]*regret1) + exp(beta[2]*regret2))
  prob[2] = exp(beta[2]*regret2)/(exp(beta[1]*regret1) + exp(beta[2]*regret2))
  
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


##### Pair level graph and joint density - HM2000 and avgpay #####
# set up the parameters for the simulation
mu = 1500 # HM2000 probability parameter
n = 500 # number of periods in each simulation
sim = 500 # number of simulations
experiment = 60 # number of experimentation periods where players randomly make decisions
beta = 0.05

# set up the joint density matrix
joint_density_all = matrix(c(0,0,0,0),2,2)

# set up the joint density matrix for each simulation
joint_density = list()

# run the simulations
for (s in 1:sim){
  
  # set up the vectors for choices and game parameters
  history_p1 = rep(0,n)
  history_p2 = rep(0,n)
  
  # set up the joint density matrix for the current simulation
  joint_density[[s]] = matrix(c(0,0,0,0),2,2)
  
  # calculate the experimentation periods with random starting decisions
  for (h in 1:experiment){
    seed = runif(1,0,1)
    if (seed <= 0.33){
      history_p1[h] = 1
      history_p2[h] = 2
    }
    else if (seed <= 0.66){
      history_p1[h] = 2
      history_p2[h] = 1
    }
    else{
      history_p1[h] = 2
      history_p2[h] = 2
    }
  }
  
  # calculate the rest of the decisions to n periods
  for (i in (experiment+1):n){
    history_p1[i] = decision_hm2000r_logit(mu, c(beta,beta), i, history_p1, history_p2)
    history_p2[i] = decision_hm2000r_logit(mu, c(beta,beta), i, history_p2, history_p1)
    
    # update the joint density matrix
    if (history_p1[i]==1 & history_p2[i]==1){joint_density[[s]][1,1]=joint_density[[s]][1,1]+1}
    else if (history_p1[i]==1 & history_p2[i]==2){joint_density[[s]][1,2]=joint_density[[s]][1,2]+1}
    else if (history_p1[i]==2 & history_p2[i]==1){joint_density[[s]][2,1]=joint_density[[s]][2,1]+1}
    else{joint_density[[s]][2,2]=joint_density[[s]][2,2]+1}
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
  # title = paste('hm2000r', 'BM', 'sim', as.character(s), sep = '_')
  # file = paste("D:/Dropbox/Working Papers/Correlated Equilibrium/data/simulations/", title, sep = "")
  # file = paste(file, ".png", sep = "")
  # png(file, width = 400, height = 200)
  # 
  # pic = ggplot(data = df) +
  #   geom_line(aes(x=period, y=p1_choice, colour='blue')) +
  #   geom_line(aes(x=period, y=p2_choice, colour='red')) +
  #   scale_x_discrete(name='period', waiver()) +
  #   scale_y_continuous(name='decision', limits=c(1,2)) +
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


##### Pair level graph and joint density - HM2001 #####
# set up the parameters for the simulation
mu = 1500 # HM2000 probability parameter
n = 10000 # number of periods in each simulation
sim = 1 # number of simulations
experiment = 1 # number of experimentation periods where players randomly make decisions
delta = 0.1
gamma = 0.0001

# set up the joint density matrix
joint_density_all = matrix(c(0,0,0,0),2,2)

# set up the joint density matrix for each simulation
joint_density = list()

# run the simulations
for (s in 1:sim){
  # set up the probability choices for HM2001
  prob_p1 = rep(0.5, n)
  prob_p2 = rep(0.5, n)
  
  # set up the vectors for choices and game parameters
  history_p1 = rep(0, n)
  history_p2 = rep(0, n)
  
  # calculate the experimentation periods with random starting decisions
  history_p1[1:experiment] = sample(1:2, experiment, replace = TRUE)
  history_p2[1:experiment] = sample(1:2, experiment, replace = TRUE)
  
  # set up the joint density matrix for the current simulation
  joint_density[[s]] = matrix(c(0,0,0,0),2,2)
  
  # calculate the rest of the decisions to n periods
  for (i in (experiment+1):n){
    history_p1[i] = decision_hm2001(mu, delta, gamma, i, history_p1, history_p2, prob_p1)[1]
    prob_p1[i] = decision_hm2001(mu, delta, gamma, i, history_p1, history_p2, prob_p1)[2]
    history_p2[i] = decision_hm2001(mu, delta, gamma, i, history_p2, history_p1, prob_p2)[1]
    prob_p2[i] = decision_hm2001(mu, delta, gamma, i, history_p2, history_p1, prob_p2)[2]
    
    # update the joint density matrix
    if (history_p1[i]==1 & history_p2[i]==1){joint_density[[s]][1,1]=joint_density[[s]][1,1]+1}
    else if (history_p1[i]==1 & history_p2[i]==2){joint_density[[s]][1,2]=joint_density[[s]][1,2]+1}
    else if (history_p1[i]==2 & history_p2[i]==1){joint_density[[s]][2,1]=joint_density[[s]][2,1]+1}
    else{joint_density[[s]][2,2]=joint_density[[s]][2,2]+1}
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
  title = paste('hm2001', 'BM', 'sim', as.character(s), sep = '_')
  file = paste("D:/Dropbox/Working Papers/Correlated Equilibrium/data/simulations/", title, sep = "")
  file = paste(file, ".png", sep = "")
  png(file, width = 400, height = 200)
  
  pic = ggplot(data = df) +
    geom_line(aes(x=period, y=p1_choice, colour='blue')) +
    geom_line(aes(x=period, y=p2_choice, colour='red')) +
    scale_x_discrete(name='period', waiver()) +
    scale_y_continuous(name='decision', limits=c(1,2)) +
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


##### Probability of converging to collusion #####
# set up the parameters for the simulation
mu = 1500 # HM2000 probability parameter
n = 1000 # number of periods in each simulation
sim = 100 # number of simulations
experiment = 100 # number of experimentation periods where players randomly make decisions
beta = 0.05

# set up the joint density matrix
joint_density_all = matrix(c(0,0,0,0),2,2)

# set up the joint density matrix and history data for each simulation
joint_density = list()
history_all = list()

# run the simulations
for (s in 1:sim){
  
  # set up the vectors for choices and game parameters
  history_p1 = rep(0, n)
  history_p2 = rep(0, n)
  prob_p1_1 = rep(0, n)
  prob_p1_2 = rep(0, n)
  prob_p2_1 = rep(0, n)
  prob_p2_2 = rep(0, n)
  
  # calculate the experimentation periods with random starting decisions
  # history_p1[1:experiment] = sample(1:2, experiment, replace = TRUE)
  # history_p2[1:experiment] = sample(2:2, experiment, replace = TRUE)
  history_p1[1:experiment] = rep(2, experiment)
  history_p2[1:experiment] = rep(2, experiment)
  
  # set up the joint density matrix for the current simulation
  joint_density[[s]] = matrix(c(0,0,0,0),2,2)
  
  # calculate the rest of the decisions to n periods
  for (i in (experiment+1):n){
    history_p1[i] = decision_avgpay_logit(mu, c(beta,beta), i, history_p1, history_p2)[1]
    history_p2[i] = decision_avgpay_logit(mu, c(beta,beta), i, history_p2, history_p1)[1]
    prob_p1_1[i] = decision_avgpay_logit(mu, c(beta,beta), i, history_p1, history_p2)[2]
    prob_p1_2[i] = decision_avgpay_logit(mu, c(beta,beta), i, history_p1, history_p2)[3]
    prob_p2_1[i] = decision_avgpay_logit(mu, c(beta,beta), i, history_p2, history_p1)[2]
    prob_p2_2[i] = decision_avgpay_logit(mu, c(beta,beta), i, history_p2, history_p1)[3]
    
    # update the joint density matrix
    if (history_p1[i]==1 & history_p2[i]==1){joint_density[[s]][1,1]=joint_density[[s]][1,1]+1}
    else if (history_p1[i]==1 & history_p2[i]==2){joint_density[[s]][1,2]=joint_density[[s]][1,2]+1}
    else if (history_p1[i]==2 & history_p2[i]==1){joint_density[[s]][2,1]=joint_density[[s]][2,1]+1}
    else{joint_density[[s]][2,2]=joint_density[[s]][2,2]+1}
  }
  
  # normalize the frequency to probability
  joint_density[[s]] = round(joint_density[[s]]/sum(joint_density[[s]]), 3)
  
  # create the dataset
  df = data.frame(
    p1_choice = history_p1,
    p2_choice = history_p2,
    p1_prob_1 = prob_p1_1,
    p1_prob_2 = prob_p1_2,
    p2_prob_1 = prob_p2_1,
    p2_prob_2 = prob_p2_2,
    period = 1:n
  )
  history_all[[s]] = df
}

# finalize the joint density matrix
for (a in 1:sim){
  joint_density_all = joint_density_all + joint_density[[a]]
}
joint_density_all = joint_density_all / sim
xtable(joint_density_all)

# check the probability of playing collusion
length = rep(0, sim)
df_m = data.frame(nash1 = length, nash2 = length, collude = length)
for (j in 1:sim){
  
  df_m$nash1[j] = joint_density[[j]][1,2]
  df_m$nash2[j] = joint_density[[j]][2,1]
  df_m$collude[j] = joint_density[[j]][2,2]
}

# graph the cdf
title = paste('cdf of playing NE and collusion', 'b', as.character(beta), sep = ' ')
file = paste("D:/Dropbox/Working Papers/Correlated Equilibrium/data/simulations/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 700, height = 400)

pic = ggplot(data = df_m) +
  stat_ecdf(geom="step", aes(x=collude, colour='blue')) +
  stat_ecdf(geom="step", aes(x=nash1, colour='red')) +
  stat_ecdf(geom="step", aes(x=nash2, colour='green')) +
  scale_x_continuous(name='prob of play', waiver(), limits=c(0,1)) +
  scale_y_continuous(name='density') +
  ggtitle(title) +
  theme_bw() + 
  scale_colour_manual(values=c('blue','red', 'green'), label=c('collude', 'nash2', 'nash1')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()

write.csv(history_all[[3]], file = 'D:/Dropbox/Working Papers/Correlated Equilibrium/data/simulations/sample.csv')