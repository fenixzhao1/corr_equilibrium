##### Package and Payoff #####
# add package
# rm(list = ls())
library(ggplot2)
library(xtable)
library(dplyr)
library(here)
source(here("simulations/MVregrets.R"))

##### Simulation pair level graph and joint density - HM2000 and avgpay #####
# set up the parameters for the simulation
mu = 1000 # HM2000 probability parameter
n = 500 # number of periods in each simulation
sim = 500 # number of simulations
experiment = 100 # number of experimentation periods where players randomly make decisions
beta = c(0.97,-0.18)
Delta = 0.99
pay_MV = matrix(c(0,200,100,100,0,200,200,100,0),3,3) # payoff matrix 3x3

# set up the joint density matrix
joint_density_all = matrix(c(0,0,0,0,0,0,0,0,0),3,3)

# set up the joint density matrix for each simulation
joint_density = list()

# set up the overall dataset
length = rep(0, sim)
df_sim = data.frame(sim = length, jd_11 = length, jd_12 = length, jd_13 = length, jd_21 = length,
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
  
  # set up the joint density matrix for the current simulation
  joint_density[[s]] = matrix(c(0,0,0,0,0,0,0,0,0),3,3)
  
  # calculate the rest of the decisions to n periods
  for (i in (experiment+1):n){
    #history_p1[i] = decision_avgpay_InertiaLogit_truncate(mu, beta, Delta, i, history_p1, history_p2)
    #history_p2[i] = decision_avgpay_InertiaLogit_truncate(mu, beta, Delta, i, history_p2, history_p1)
    #history_p1[i] = decision_avgpay_InertiaLogit(mu, beta, Delta, i, history_p1, history_p2)
    #history_p2[i] = decision_avgpay_InertiaLogit(mu, beta, Delta, i, history_p2, history_p1)
    #history_p1[i] = decision_avgpay_logit(mu, beta, i, history_p1, history_p2)
    #history_p2[i] = decision_avgpay_logit(mu, beta, i, history_p2, history_p1)
    history_p1[i] = decision_hm2000(mu, i, history_p1, history_p2)
    history_p2[i] = decision_hm2000(mu, i, history_p2, history_p1)
    
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
  
  # create the dataset for this simulation
  df = data.frame(
    p1_choice = history_p1[(experiment+1):n],
    p2_choice = history_p2[(experiment+1):n],
    period = (experiment+1):n)
  df = df %>% mutate(
    is_11 = ifelse(p1_choice==1 & p2_choice==1, 1, 0),
    is_12 = ifelse(p1_choice==1 & p2_choice==2, 1, 0),
    is_13 = ifelse(p1_choice==1 & p2_choice==3, 1, 0),
    is_21 = ifelse(p1_choice==2 & p2_choice==1, 1, 0),
    is_22 = ifelse(p1_choice==2 & p2_choice==2, 1, 0),
    is_23 = ifelse(p1_choice==2 & p2_choice==3, 1, 0),
    is_31 = ifelse(p1_choice==3 & p2_choice==1, 1, 0),
    is_32 = ifelse(p1_choice==3 & p2_choice==2, 1, 0),
    is_33 = ifelse(p1_choice==3 & p2_choice==3, 1, 0))
  for (j in 1:length(df$period)){
    df$p1_payoff[j] = pay_MV[df$p1_choice[j], df$p2_choice[j]]
    df$p2_payoff[j] = pay_MV[df$p2_choice[j], df$p1_choice[j]]
  }
  
  # record the sim ID to the overall data
  df_sim$sim[s] = s
  
  # record the joint density
  df_sim$jd_11[s] = mean(df$is_11)
  df_sim$jd_12[s] = mean(df$is_12)
  df_sim$jd_13[s] = mean(df$is_13)
  df_sim$jd_21[s] = mean(df$is_21)
  df_sim$jd_22[s] = mean(df$is_22)
  df_sim$jd_23[s] = mean(df$is_23)
  df_sim$jd_31[s] = mean(df$is_31)
  df_sim$jd_32[s] = mean(df$is_32)
  df_sim$jd_33[s] = mean(df$is_33)
  df_sim$p1_payoff[s] = mean(df$p1_payoff)
  df_sim$p2_payoff[s] = mean(df$p2_payoff)
  
  # # graph the decision making
  # title = paste('hm2000r', 'MV', 'sim', as.character(s), sep = '_')
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

# calculate the joint distribution
for (a in 1:sim){
  joint_density_all = joint_density_all + joint_density[[a]]
}
joint_density_all = joint_density_all / sim
xtable(joint_density_all)

# calculate the fraction in CE
df_sim$in_ce <- 0
df_sim <- df_sim %>%
  mutate(in_ce = ifelse(p1_payoff>100 & abs((p2_payoff-100)/(p1_payoff-100))>=1/2 & abs((p2_payoff-100)/(p1_payoff-100))<=2,1,in_ce))
df_sim<- df_sim %>%
  mutate(in_ce = ifelse(p1_payoff==100 & p2_payoff==100,1,in_ce))

# calculate the distance
df_sim = df_sim %>%
  mutate(
    d_mne = sqrt((p1_payoff-100)^2+(p2_payoff-100)^2),
    d_tce = sqrt((p1_payoff-150)^2+(p2_payoff-150)^2))

# display the results
mean(df_sim$in_ce)
mean(df_sim$d_mne)
mean(df_sim$d_tce)
t.test(df_sim$d_mne, df_sim$d_tce, mu=0, paired = TRUE)$p.value

#rm(df, df_sim, joint_density, joint_density_all, history_p1, history_p2)


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

