##### Data preparation #####
# load packages
rm(list = ls())
library(here)
library(ggplot2)
library(dplyr)
library(haven)
your_path<-"~/Desktop/jotarepos/correq/corr_equilibrium/Data/no_bars/"
your_path_1<-"~/Desktop/jotarepos/correq/corr_equilibrium/Data/new_data/"

full_data<-NULL
d<-read.csv(paste(your_path_1,"BML_3_07gjoche.csv",sep=""),header=T, stringsAsFactors = FALSE)
d = select(d, 1:36)
full_data<-rbind(full_data,d)
rm(d)

# sort data and add period variable
full_data = arrange(full_data, full_data$session_code, full_data$subsession_id, full_data$id_in_subsession, full_data$tick)
full_data = full_data %>% filter(round>2) %>% mutate(period = tick + 1)

# unify matrices
full_data$p1_strategy[full_data$game == 'BM2']<-1-full_data$p1_strategy[full_data$game == 'BM2']
full_data$p2_strategy[full_data$game == 'BM2']<-1-full_data$p2_strategy[full_data$game == 'BM2']

full_data$p1_strategy[full_data$game == 'MV2' & full_data$p1_strategy>0]<-2/full_data$p1_strategy[full_data$game == 'MV2' & full_data$p1_strategy>0]
full_data$p2_strategy[full_data$game == 'MV2' & full_data$p2_strategy!=1]<-2-full_data$p2_strategy[full_data$game == 'MV2' & full_data$p2_strategy!=1]

full_data$game[full_data$game=='BM1']<-'BM'
full_data$game[full_data$game == 'BM2']<-'BM'
full_data$game[full_data$game=='MV1']<-'MV'
full_data$game[full_data$game == 'MV2']<-'MV'


# create round/pair id
full_data$pair_id = paste(full_data$p1_code, full_data$p2_code, sep = "_")
full_data$round_pair_id = paste(full_data$round, full_data$pair_id,  sep = "_")
full_data$session_round_pair_id = paste(full_data$session_code, full_data$round_pair_id, sep = "_")
full_data$session_round_id = paste(full_data$session_code, full_data$round, sep = "_")

# create treatment variable
full_data = full_data %>% mutate(information = ifelse(max_info == 'TRUE' |  max_info == 'True', 'H', 'L'))
full_data = full_data %>% mutate(regret_info = ifelse(regret == 2, 'A', 'C'))

full_data$treatment = paste(full_data$game, full_data$information, full_data$regret_info, sep = '_')
full_data$game_info = paste(full_data$game, full_data$information)

# create strategy indicators
full_data = full_data %>% mutate(p1_strategy_0 = ifelse(p1_strategy == 0, 1, 0))
full_data = full_data %>% mutate(p1_strategy_1 = ifelse(p1_strategy == 1, 1, 0))
full_data = full_data %>% mutate(p1_strategy_2 = ifelse(p1_strategy == 2, 1, 0))
full_data = full_data %>% mutate(p2_strategy_0 = ifelse(p2_strategy == 0, 1, 0))
full_data = full_data %>% mutate(p2_strategy_1 = ifelse(p2_strategy == 1, 1, 0))
full_data = full_data %>% mutate(p2_strategy_2 = ifelse(p2_strategy == 2, 1, 0))
full_data = full_data %>% mutate(p3_strategy_0 = ifelse(p3_strategy == 0, 1, 0))
full_data = full_data %>% mutate(p3_strategy_1 = ifelse(p3_strategy == 1, 1, 0))
full_data = full_data %>% mutate(p3_strategy_2 = ifelse(p3_strategy == 2, 1, 0))

# check number of subjects
uniquePlayer = union(unique(full_data$p1_code), unique(full_data$p2_code))

# create and update type variables
full_data$type = NA

for (i in 1:length(full_data$tick)){
  # update BM types
  if (full_data$game[i] == 'BM'){
    if (full_data$p1_strategy[i] == 1 & full_data$p2_strategy[i] == 1){full_data$type[i] = 'collude'}
    else if (full_data$p1_strategy[i] != full_data$p2_strategy[i]){full_data$type[i] = 'Nash'}
    else{full_data$type[i] = 'UL'}
  }
  # update MV types
  else if (full_data$game[i] == 'MV'){
    if (full_data$p1_strategy[i] == full_data$p2_strategy[i]){full_data$type[i] = 'diagonal'}
    else if (full_data$p1_strategy[i] == 1 & full_data$p2_strategy[i] == 0){full_data$type[i] = 'p1 advantage'}
    else if (full_data$p1_strategy[i] == 2 & full_data$p2_strategy[i] == 1){full_data$type[i] = 'p1 advantage'}
    else if (full_data$p1_strategy[i] == 0 & full_data$p2_strategy[i] == 2){full_data$type[i] = 'p1 advantage'}
    else{{full_data$type[i] = 'p2 advantage'}}
  }
  # update FT types
  else{
    if (full_data$p3_strategy[i] == 1){full_data$type[i] = 'middle matrix'}
    else if (full_data$p1_strategy[i] == 1 & full_data$p2_strategy[i] == 0){full_data$type[i] = 'DL'}
    else{full_data$type[i] = 'others'}
  }
}

# create type dummies
full_data = full_data %>% mutate(
  type_collude = ifelse(full_data$type=='collude', 1, 0),
  type_diagonal = ifelse(full_data$type=='diagonal', 1, 0),
  type_DL = ifelse(full_data$type=='DL', 1, 0),
  type_middle = ifelse(full_data$type=='middle matrix', 1, 0),
  type_Nash = ifelse(full_data$type=='Nash', 1, 0),
  type_others = ifelse(full_data$type=='others', 1, 0),
  type_p1adv = ifelse(full_data$type=='p1 advantage', 1, 0),
  type_p2adv = ifelse(full_data$type=='p2 advantage', 1, 0),
  type_UL = ifelse(full_data$type=='UL', 1, 0)
)

# create and update type variables
full_data$type2 = NA

for (i in 1:length(full_data$tick)){
  # update BM types
  if (full_data$game[i] == 'BM'){
    if (full_data$p1_strategy[i] == 1 & full_data$p2_strategy[i] == 1){full_data$type2[i] = 4}
    else if (full_data$p1_strategy[i] == 1 & full_data$p2_strategy[i] == 0){full_data$type2[i] = 3}
    else if (full_data$p1_strategy[i] == 0 & full_data$p2_strategy[i] == 1){full_data$type2[i] = 2}
    else{full_data$type2[i] = 1}
  }
  # update MV types
  else{
    if (full_data$p1_strategy[i] == 0 & full_data$p2_strategy[i] == 0){full_data$type2[i] = 1}
    else if (full_data$p1_strategy[i] == 1 & full_data$p2_strategy[i] == 1){full_data$type2[i] = 2}
    else if (full_data$p1_strategy[i] == 2 & full_data$p2_strategy[i] == 2){full_data$type2[i] = 3}
    else if (full_data$p1_strategy[i] == 0 & full_data$p2_strategy[i] == 2){full_data$type2[i] = 4}
    else if (full_data$p1_strategy[i] == 1 & full_data$p2_strategy[i] == 0){full_data$type2[i] = 5}
    else if (full_data$p1_strategy[i] == 2 & full_data$p2_strategy[i] == 1){full_data$type2[i] = 6}
    else if (full_data$p1_strategy[i] == 0 & full_data$p2_strategy[i] == 1){full_data$type2[i] = 7}
    else if (full_data$p1_strategy[i] == 1 & full_data$p2_strategy[i] == 2){full_data$type2[i] = 8}
    else{full_data$type2[i] = 9}
  }
}

# create payoff variables
full_data$p1_payoff<-0
full_data$p2_payoff<-0
full_data$p3_payoff<-0

# create payoff matrices
pay_chicken<-matrix(c(100,200,600,500),2,2)
pay_MV<-matrix(c(0,200,100,100,0,200,200,100,0),3,3)
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

for(row in seq(full_data$tick[full_data$game=="BM"])){
  full_data$p1_payoff[full_data$game=="BM"][row]<- pay_chicken[full_data$p1_strategy[full_data$game=="BM"][row]+1,full_data$p2_strategy[full_data$game=="BM"][row]+1]
  full_data$p2_payoff[full_data$game=="BM"][row]<- pay_chicken[full_data$p2_strategy[full_data$game=="BM"][row]+1,full_data$p1_strategy[full_data$game=="BM"][row]+1]
}

for(row in seq(full_data$tick[full_data$game=="MV"])){
  full_data$p1_payoff[full_data$game=="MV"][row]<- pay_MV[full_data$p1_strategy[full_data$game=="MV"][row]+1,full_data$p2_strategy[full_data$game=="MV"][row]+1]
  full_data$p2_payoff[full_data$game=="MV"][row]<- pay_MV[full_data$p2_strategy[full_data$game=="MV"][row]+1,full_data$p1_strategy[full_data$game=="MV"][row]+1]
}

for(row in seq(full_data$tick[full_data$game=="FP"])){
  full_data$p1_payoff[full_data$game=="FP"][row]<- pay_FT1[[full_data$p3_strategy[full_data$game=="FP"][row]+1]][full_data$p1_strategy[full_data$game=="FP"][row]+1,full_data$p2_strategy[full_data$game=="FP"][row]+1]
  full_data$p2_payoff[full_data$game=="FP"][row]<- pay_FT2[[full_data$p3_strategy[full_data$game=="FP"][row]+1]][full_data$p1_strategy[full_data$game=="FP"][row]+1,full_data$p2_strategy[full_data$game=="FP"][row]+1]
  full_data$p3_payoff[full_data$game=="FP"][row]<- pay_FT3[[full_data$p3_strategy[full_data$game=="FP"][row]+1]][full_data$p1_strategy[full_data$game=="FP"][row]+1,full_data$p2_strategy[full_data$game=="FP"][row]+1]
}

rm(pay_chicken, pay_FT1, pay_FT2, pay_FT3, pay_MV)

pay_player_1= filter(full_data, p1_code=="uqo4i6gc")
pay_player_2= filter(full_data, p2_code=="uqo4i6gc")

a1<-sum(pay_player_1$p1_payoff==100)*100+sum(pay_player_1$p1_payoff==200)*200+sum(pay_player_1$p1_payoff==600)*600+sum(pay_player_1$p1_payoff==500)*500
a2<-sum(pay_player_2$p2_payoff==100)*100+sum(pay_player_2$p2_payoff==200)*200+sum(pay_player_2$p2_payoff==600)*600+sum(pay_player_2$p2_payoff==500)*500
(a1+a2)/(50)*.0063

pay<-sum(full_data$p1_payoff[full_data$p1_code=="uqo4i6gc"])+sum(full_data$p2_payoff[full_data$p2_code=="uqo4i6gc"])
pay*0.0063/50
