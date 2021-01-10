# add package
library(ggplot2)

# set up the payoff matrix
pay_chicken = matrix(c(100,200,600,500),2,2) # payoff matrix 2x2
pay_MV = matrix(c(0,200,100,100,0,200,200,100,0),3,3) # payoff matrix 3x3


# build the payoff function (chicken, can be switched to MV)
payoff = function(my_choice, your_choice){
  return(pay_chicken[my_choice, your_choice])
}


# build the regret function for action m (m is 1,2,or 3)
regret = function(m, iteration, my_history, your_history){
  
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


# build the decision function based on the regret function
decision = function(mu, iteration, my_history, your_history){
  
  # get my most recent decision
  lastchoice = my_history[iteration-1]
  
  # compute regret for all possible decisions
  regret1 = regret(1, iteration, my_history, your_history)
  regret2 = regret(2, iteration, my_history, your_history)
  
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


# set up the parameters for the simulation
mu = 1000 # HM2000 probability parameter
n = 500 # number of periods in each simulation
sim = 50 # number of simulations
experiment = 100 # number of experimentation periods where players randomly make decisions

# run the simulations
for (s in 1:sim){
  
  # set up the vectors for choices and game parameters
  history_p1 = rep(0, n)
  history_p2 = rep(0, n)
  payoff_p1 = rep(0, n)
  payoff_p2 = rep(0, n)
  
  # calculate the experimentation periods with random starting decisions
  history_p1[1:experiment] = sample(1:2, experiment, replace = TRUE)
  history_p2[1:experiment] = sample(1:2, experiment, replace = TRUE)
  #payoff_p1[1] = payoff(history_p1[1], history_p2[1])
  #payoff_p2[1] = payoff(history_p2[1], history_p1[1])
  
  # calculate the rest of the decisions to n periods
  for (i in (experiment+1):n){
    history_p1[i] = decision(mu, i, history_p1, history_p2)
    history_p2[i] = decision(mu, i, history_p2, history_p1)
  }
  df = data.frame(
    p1_choice = history_p1,
    p2_choice = history_p2,
    period = 1:n
  )
  
  # graph the decision making
  title = paste('Simulation BM games', as.character(s), sep = ' ')
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








