##### Run CH simulations and build dataset #####
rm(list = ls())
library(ggplot2)
library(xtable)
library(dplyr)
library(here)
source(here("simulations/BMregrets.R"))

# set up the parameters for the simulations
mu = 1800 # HM2000 probability parameter
n = 500 # number of periods in each simulation
sim = 500 # number of simulations
experiment = 100 # number of experimentation periods where players randomly make decisions
pay_chicken = matrix(c(100,200,600,500),2,2) # payoff matrix 2x2


## counterfactual simulation
# set up parameters
beta = 2.9
Delta = 0.48

# set up the joint density matrix for each simulation
joint_density = list()

# set up the overall dataset
length = rep(0, sim)
df_sim_c = data.frame(sim = length, jd_11 = length, jd_12 = length, jd_21 = length, jd_22 = length,
                      p1_payoff = length, p2_payoff = length, regret = length)

# run the simulations
for (s in 1:sim){
  
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
    history_p1[i] = decision_hm2000r_InertiaLogit(mu, beta, Delta, i, history_p1, history_p2)
    history_p2[i] = decision_hm2000r_InertiaLogit(mu, beta, Delta, i, history_p2, history_p1)
 
    # update the joint density matrix
    if (history_p1[i]==1 & history_p2[i]==1){joint_density[[s]][1,1]=joint_density[[s]][1,1]+1}
    else if (history_p1[i]==1 & history_p2[i]==2){joint_density[[s]][1,2]=joint_density[[s]][1,2]+1}
    else if (history_p1[i]==2 & history_p2[i]==1){joint_density[[s]][2,1]=joint_density[[s]][2,1]+1}
    else{joint_density[[s]][2,2]=joint_density[[s]][2,2]+1}
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
    is_21 = ifelse(p1_choice==2 & p2_choice==1, 1, 0),
    is_22 = ifelse(p1_choice==2 & p2_choice==2, 1, 0))
  for (j in 1:length(df$period)){
    df$p1_payoff[j] = pay_chicken[df$p1_choice[j], df$p2_choice[j]]
    df$p2_payoff[j] = pay_chicken[df$p2_choice[j], df$p1_choice[j]]
  }
  
  # update the simulation dataset
  df_sim_c$sim[s] = s
  df_sim_c$jd_11[s] = mean(df$is_11)
  df_sim_c$jd_12[s] = mean(df$is_12)
  df_sim_c$jd_21[s] = mean(df$is_21)
  df_sim_c$jd_22[s] = mean(df$is_22)
  df_sim_c$p1_payoff[s] = mean(df$p1_payoff)
  df_sim_c$p2_payoff[s] = mean(df$p2_payoff)
  df_sim_c$regret[s] = 'counterfactual'
}


## average simulation
# set up parameters
beta = 0.51
Delta = 0.48

# set up the joint density matrix for each simulation
joint_density = list()

# set up the overall dataset
length = rep(0, sim)
df_sim_a = data.frame(sim = length, jd_11 = length, jd_12 = length, jd_21 = length, jd_22 = length,
                      p1_payoff = length, p2_payoff = length, regret = length)

# run the simulations
for (s in 1:sim){
  
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
    history_p1[i] = decision_avgpay_InertiaLogit(mu, beta, Delta, i, history_p1, history_p2)
    history_p2[i] = decision_avgpay_InertiaLogit(mu, beta, Delta, i, history_p2, history_p1)
    
    # update the joint density matrix
    if (history_p1[i]==1 & history_p2[i]==1){joint_density[[s]][1,1]=joint_density[[s]][1,1]+1}
    else if (history_p1[i]==1 & history_p2[i]==2){joint_density[[s]][1,2]=joint_density[[s]][1,2]+1}
    else if (history_p1[i]==2 & history_p2[i]==1){joint_density[[s]][2,1]=joint_density[[s]][2,1]+1}
    else{joint_density[[s]][2,2]=joint_density[[s]][2,2]+1}
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
    is_21 = ifelse(p1_choice==2 & p2_choice==1, 1, 0),
    is_22 = ifelse(p1_choice==2 & p2_choice==2, 1, 0))
  for (j in 1:length(df$period)){
    df$p1_payoff[j] = pay_chicken[df$p1_choice[j], df$p2_choice[j]]
    df$p2_payoff[j] = pay_chicken[df$p2_choice[j], df$p1_choice[j]]
  }
  
  # update the simulation dataset
  df_sim_a$sim[s] = s
  df_sim_a$jd_11[s] = mean(df$is_11)
  df_sim_a$jd_12[s] = mean(df$is_12)
  df_sim_a$jd_21[s] = mean(df$is_21)
  df_sim_a$jd_22[s] = mean(df$is_22)
  df_sim_a$p1_payoff[s] = mean(df$p1_payoff)
  df_sim_a$p2_payoff[s] = mean(df$p2_payoff)
  df_sim_a$regret[s] = 'average'
}

# combine datasets
full_data = rbind(df_sim_c, df_sim_a)
write.csv(full_data,here("Data", "sim_joint_ch.csv"))


##### Run MV simulations and build dataset #####
rm(list = ls())
source(here("simulations/MVregrets.R"))

# set up the parameters for the simulations
mu = 1000 # HM2000 probability parameter
n = 500 # number of periods in each simulation
sim = 500 # number of simulations
experiment = 100 # number of experimentation periods where players randomly make decisions
pay_MV = matrix(c(0,200,100,100,0,200,200,100,0),3,3) # payoff matrix 3x3


## counterfactual simulation
# set up parameters
beta = 1.71
Delta = 0.98

# set up the joint density matrix for each simulation
joint_density = list()

# set up the overall dataset
length = rep(0, sim)
df_sim_c = data.frame(sim = length, jd_11 = length, jd_12 = length, jd_13 = length, jd_21 = length,
                      jd_22 = length, jd_23 = length, jd_31 = length, jd_32 = length, jd_33 = length,
                      p1_payoff = length, p2_payoff = length, regret = length)

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
    history_p1[i] = decision_hm2000r_InertiaLogit(mu, beta, Delta, i, history_p1, history_p2)
    history_p2[i] = decision_hm2000r_InertiaLogit(mu, beta, Delta, i, history_p2, history_p1)
    
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
  df_sim_c$sim[s] = s
  df_sim_c$jd_11[s] = mean(df$is_11)
  df_sim_c$jd_12[s] = mean(df$is_12)
  df_sim_c$jd_13[s] = mean(df$is_13)
  df_sim_c$jd_21[s] = mean(df$is_21)
  df_sim_c$jd_22[s] = mean(df$is_22)
  df_sim_c$jd_23[s] = mean(df$is_23)
  df_sim_c$jd_31[s] = mean(df$is_31)
  df_sim_c$jd_32[s] = mean(df$is_32)
  df_sim_c$jd_33[s] = mean(df$is_33)
  df_sim_c$p1_payoff[s] = mean(df$p1_payoff)
  df_sim_c$p2_payoff[s] = mean(df$p2_payoff)
  df_sim_c$regret[s] = 'counterfactual'
}


## counterfactual simulation
# set up parameters
beta = 0.88
Delta = 0.98

# set up the joint density matrix for each simulation
joint_density = list()

# set up the overall dataset
length = rep(0, sim)
df_sim_a = data.frame(sim = length, jd_11 = length, jd_12 = length, jd_13 = length, jd_21 = length,
                      jd_22 = length, jd_23 = length, jd_31 = length, jd_32 = length, jd_33 = length,
                      p1_payoff = length, p2_payoff = length, regret = length)

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
    history_p1[i] = decision_avgpay_InertiaLogit(mu, beta, Delta, i, history_p1, history_p2)
    history_p2[i] = decision_avgpay_InertiaLogit(mu, beta, Delta, i, history_p2, history_p1)
    
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
  df_sim_a$sim[s] = s
  df_sim_a$jd_11[s] = mean(df$is_11)
  df_sim_a$jd_12[s] = mean(df$is_12)
  df_sim_a$jd_13[s] = mean(df$is_13)
  df_sim_a$jd_21[s] = mean(df$is_21)
  df_sim_a$jd_22[s] = mean(df$is_22)
  df_sim_a$jd_23[s] = mean(df$is_23)
  df_sim_a$jd_31[s] = mean(df$is_31)
  df_sim_a$jd_32[s] = mean(df$is_32)
  df_sim_a$jd_33[s] = mean(df$is_33)
  df_sim_a$p1_payoff[s] = mean(df$p1_payoff)
  df_sim_a$p2_payoff[s] = mean(df$p2_payoff)
  df_sim_a$regret[s] = 'average'
}

# combine datasets
full_data = rbind(df_sim_c, df_sim_a)
write.csv(full_data,here("Data", "sim_joint_mv.csv"))


##### Plot the payoff vectors by simulation run #####
# read joint density data
rm(list = ls())
library(ggplot2)
library(xtable)
library(dplyr)
library(here)

d_ch<-read.csv(here("Data/sim_joint_ch.csv"), header=T, stringsAsFactors = FALSE)
d_ch_c = filter(d_ch, regret == 'counterfactual')
d_ch_a = filter(d_ch, regret == 'average')
d_mv<-read.csv(here("Data/sim_joint_mv.csv"), header=T, stringsAsFactors = FALSE)
d_mv_c = filter(d_mv, regret == 'counterfactual')
d_mv_a = filter(d_mv, regret == 'average')

# # add payoff information
# d_ch = d_ch %>% mutate(
#   p1_payoff = 100*jd_11 + 600*jd_12 + 200*jd_21 + 500*jd_22,
#   p2_payoff = 100*jd_11 + 200*jd_12 + 600*jd_21 + 500*jd_22)
# 
# d_mv = d_mv %>% mutate(
#   p1_payoff = 100*jd_12 + 200*jd_13 + 200*jd_21 + 100*jd_23 + 100*jd_31 + 200*jd_32,
#   p2_payoff = 200*jd_12 + 100*jd_13 + 100*jd_21 + 200*jd_23 + 200*jd_31 + 100*jd_32)

## draw figure
# set up CH parameters
ne_pure1<-c(2,6)
ne_pure2<-c(6,2)
best_ce<-c(13/3,13/3)
worst_ce<-c(3,3)

puntos_bm<-as.data.frame(rbind(worst_ce,ne_pure1,best_ce,ne_pure2,worst_ce)*100)
puntos_bm_matrix<-as.data.frame(rbind(c(1,1),ne_pure1,c(5,5),ne_pure2,c(1,1))*100)

x<-c(2,3,13/3,6)
y<-c(12-3*x[1:2],4-(1/3)*x[3:4])
y_u<-c(5/7*2+6-5/7*x[1:2],7/5*13/3+13/3-7/5*x[3:4])

puntos_shade<-as.data.frame(cbind(x,y,y_u)*100)
names(puntos_shade)<-c("V1","V2","V3")

# set up MV parameters
ne_mv<-c(1,1)
ce_mv_s<-c(3/2,3/2)
ce_mv_1<-c(5/3,4/3)

puntos_mv<-as.data.frame(rbind(ne_mv,rev(ce_mv_1),ce_mv_1,ne_mv)*100)
puntos_mv_matrix<-as.data.frame(rbind(c(0,0),c(1,2),c(2,1),c(0,0))*100)

x_mv<-c(1,1.33,1.6667)
y_mv<-1/2*x_mv+1/2
y_u_mv<-c(2*x_mv-1)
y_u_mv_2<-3-x_mv+0.0005

puntos_shade_mv<-as.data.frame(cbind(x_mv,y_mv,y_u_mv,y_u_mv_2)*100)
names(puntos_shade_mv)<-c("V1","V2","V3","V4")

# CH figure
ggplot(data=d_ch_c,aes(x=p1_payoff,y=p2_payoff)) + geom_point() +  geom_count() +
  geom_path(data=puntos_bm_matrix,aes(x=V1,y=V2),linetype = "dashed") +
  geom_path(data=puntos_bm,aes(x=V1,y=V2)) +
  geom_ribbon(data=puntos_shade, aes(x=V1,y=V2,ymin=V2,ymax=V3), fill="blue", alpha=0.5) +
  scale_x_continuous(name='Payoff P1', waiver(), limits=c(100,600)) +
  scale_y_continuous(name='Payoff P2', waiver(), limits=c(100,600)) +
  annotate(geom="text", x=190, y=600, label="NE") +
  annotate(geom="text", x=600, y=190, label="NE") +
  annotate(geom="text", x=360, y=360, label="MNE") +
  annotate("point", x = 350, y = 350, colour = "blue") +
  annotate(geom="text", x=454, y=440, label="Target CE") +
  annotate("point", x = 433, y = 433, colour = "blue") 
ggsave(here("Figures", 'sims_CH_c.png'))

ggplot(data=d_ch_a,aes(x=p1_payoff,y=p2_payoff)) + geom_point() +  geom_count() +
  geom_path(data=puntos_bm_matrix,aes(x=V1,y=V2),linetype = "dashed") +
  geom_path(data=puntos_bm,aes(x=V1,y=V2)) +
  geom_ribbon(data=puntos_shade, aes(x=V1,y=V2,ymin=V2,ymax=V3), fill="blue", alpha=0.5) +
  scale_x_continuous(name='Payoff P1', waiver(), limits=c(100,600)) +
  scale_y_continuous(name='Payoff P2', waiver(), limits=c(100,600)) +
  annotate(geom="text", x=190, y=600, label="NE") +
  annotate(geom="text", x=600, y=190, label="NE") +
  annotate(geom="text", x=360, y=360, label="MNE") +
  annotate("point", x = 350, y = 350, colour = "blue") +
  annotate(geom="text", x=454, y=440, label="Target CE") +
  annotate("point", x = 433, y = 433, colour = "blue") 
ggsave(here("Figures", 'sims_CH_a.png'))

# MV figure
ggplot(data=d_mv_c ,aes(x=p1_payoff,y=p2_payoff)) + geom_point() + geom_count() +
  geom_path(data=puntos_mv_matrix,aes(x=V1,y=V2),linetype = "dashed")+
  geom_path(data=puntos_mv,aes(x=V1,y=V2)) +
  geom_ribbon(data=puntos_shade_mv,aes(x=V1,y=V2,ymin=V2,ymax=V3), fill="blue", alpha=0.5) +
  geom_ribbon(data=puntos_shade_mv,aes(x=V1,y=V2,ymin=V2,ymax=V4), fill="blue", alpha=0.5) +
  scale_x_continuous(name='Payoff P1', waiver(), limits=c(0,200)) +
  scale_y_continuous(name='Payoff P2', waiver(), limits=c(0,200)) +
  annotate(geom="text", x=100, y=95, label="MNE") +
  annotate("point", x = 100, y = 100, colour = "blue") +
  annotate(geom="text", x=160, y=155, label="Target CE") +
  annotate("point", x = 150, y = 150, colour = "blue", shape=10) 
ggsave(here("Figures", 'sims_MV_c.png'))

ggplot(data=d_mv_a ,aes(x=p1_payoff,y=p2_payoff)) + geom_point() + geom_count() +
  geom_path(data=puntos_mv_matrix,aes(x=V1,y=V2),linetype = "dashed")+
  geom_path(data=puntos_mv,aes(x=V1,y=V2)) +
  geom_ribbon(data=puntos_shade_mv,aes(x=V1,y=V2,ymin=V2,ymax=V3), fill="blue", alpha=0.5) +
  geom_ribbon(data=puntos_shade_mv,aes(x=V1,y=V2,ymin=V2,ymax=V4), fill="blue", alpha=0.5) +
  scale_x_continuous(name='Payoff P1', waiver(), limits=c(0,200)) +
  scale_y_continuous(name='Payoff P2', waiver(), limits=c(0,200)) +
  annotate(geom="text", x=100, y=95, label="MNE") +
  annotate("point", x = 100, y = 100, colour = "blue") +
  annotate(geom="text", x=160, y=155, label="Target CE") +
  annotate("point", x = 150, y = 150, colour = "blue", shape=10) 
ggsave(here("Figures", 'sims_MV_a.png'))
 

