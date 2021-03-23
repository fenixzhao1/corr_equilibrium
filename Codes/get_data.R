##### Data preparation #####
# load packages
rm(list = ls())
library(here)
library(ggplot2)
library(dplyr)
library(haven)

# add paths to files
your_path<-here("Data/no_bars/")
your_path_1<-here("Data/new_data/")
path = list.files(path=your_path, pattern="*.csv")
path_1 = list.files(path=your_path_1, pattern="*.csv")

# import data with no regret columns
full_data<-NULL
for(i in seq(along=path)){
  d<-read.csv(paste(your_path, '/', path[i],sep=""),header=T, stringsAsFactors = FALSE)
  d = select(d, 1:27)
  full_data<-rbind(full_data,d)
}

full_data = full_data %>% mutate(
  p1_regret0 = 0,
  p1_regret1 = 0,
  p1_regret2 = 0,
  p2_regret0 = 0,
  p2_regret1 = 0,
  p2_regret2 = 0,
  p3_regret0 = 0,
  p3_regret1 = 0,
  p3_regret2 = 0,
  adjust_regret = TRUE
)

# import the rest of the data with regret columns
for(i in seq(along=path_1)){
  d<-read.csv(paste(your_path_1, '/', path_1[i],sep=""),header=T, stringsAsFactors = FALSE)
  d = select(d, 1:36)
  d = d %>% mutate(adjust_regret = FALSE)
  full_data<-rbind(full_data,d)
}
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
# pay_FT1<-list()
# pay_FT1[[1]]<-matrix(c(0,100,0,100),2,2)
# pay_FT1[[2]]<-matrix(c(200,200,0,200),2,2)
# pay_FT1[[3]]<-matrix(c(0,100,0,100),2,2)
# pay_FT2<-list()
# pay_FT2[[1]]<-matrix(c(100,100,0,0),2,2)
# pay_FT2[[2]]<-matrix(c(200,200,0,200),2,2)
# pay_FT2[[3]]<-matrix(c(100,100,0,0),2,2)
# pay_FT3<-list()
# pay_FT3[[1]]<-matrix(c(300,100,0,0),2,2)
# pay_FT3[[2]]<-matrix(c(200,0,0,200),2,2)
# pay_FT3[[3]]<-matrix(c(0,0,0,300),2,2)

for(row in seq(full_data$tick[full_data$game=="BM"])){
  full_data$p1_payoff[full_data$game=="BM"][row]<- pay_chicken[full_data$p1_strategy[full_data$game=="BM"][row]+1,full_data$p2_strategy[full_data$game=="BM"][row]+1]
  full_data$p2_payoff[full_data$game=="BM"][row]<- pay_chicken[full_data$p2_strategy[full_data$game=="BM"][row]+1,full_data$p1_strategy[full_data$game=="BM"][row]+1]
}
for(row in seq(full_data$tick[full_data$game=="MV"])){
  full_data$p1_payoff[full_data$game=="MV"][row]<- pay_MV[full_data$p1_strategy[full_data$game=="MV"][row]+1,full_data$p2_strategy[full_data$game=="MV"][row]+1]
  full_data$p2_payoff[full_data$game=="MV"][row]<- pay_MV[full_data$p2_strategy[full_data$game=="MV"][row]+1,full_data$p1_strategy[full_data$game=="MV"][row]+1]
}
# for(row in seq(full_data$tick[full_data$game=="FP"])){
#   full_data$p1_payoff[full_data$game=="FP"][row]<- pay_FT1[[full_data$p3_strategy[full_data$game=="FP"][row]+1]][full_data$p1_strategy[full_data$game=="FP"][row]+1,full_data$p2_strategy[full_data$game=="FP"][row]+1]
#   full_data$p2_payoff[full_data$game=="FP"][row]<- pay_FT2[[full_data$p3_strategy[full_data$game=="FP"][row]+1]][full_data$p1_strategy[full_data$game=="FP"][row]+1,full_data$p2_strategy[full_data$game=="FP"][row]+1]
#   full_data$p3_payoff[full_data$game=="FP"][row]<- pay_FT3[[full_data$p3_strategy[full_data$game=="FP"][row]+1]][full_data$p1_strategy[full_data$game=="FP"][row]+1,full_data$p2_strategy[full_data$game=="FP"][row]+1]
# }

# manually calculate regret
full_data = full_data %>% group_by(session_round_pair_id) %>%
  mutate(p1_strategy_0_regret= cumsum(coalesce(lag(p1_payoff), 0)*coalesce(lag(p1_strategy_0), 0))/cumsum(coalesce(lag(p1_strategy_0),0)),
         p1_strategy_1_regret= cumsum(coalesce(lag(p1_payoff), 0)*coalesce(lag(p1_strategy_1), 0))/cumsum(coalesce(lag(p1_strategy_1),0)),
         p1_strategy_2_regret= cumsum(coalesce(lag(p1_payoff), 0)*coalesce(lag(p1_strategy_2), 0))/cumsum(coalesce(lag(p1_strategy_2),0)),
         p2_strategy_0_regret= cumsum(coalesce(lag(p2_payoff), 0)*coalesce(lag(p2_strategy_0), 0))/cumsum(coalesce(lag(p2_strategy_0),0)),
         p2_strategy_1_regret= cumsum(coalesce(lag(p2_payoff), 0)*coalesce(lag(p2_strategy_1), 0))/cumsum(coalesce(lag(p2_strategy_1),0)),
         p2_strategy_2_regret= cumsum(coalesce(lag(p2_payoff), 0)*coalesce(lag(p2_strategy_2), 0))/cumsum(coalesce(lag(p2_strategy_2),0)),
         p1_switch = ifelse(abs(p1_strategy-lag(p1_strategy))>0, 1, 0),
         p2_switch = ifelse(abs(p2_strategy-lag(p2_strategy))>0, 1, 0))

full_data$p1_strategy_0_regret[is.na(full_data$p1_strategy_0_regret)]<-0
full_data$p1_strategy_1_regret[is.na(full_data$p1_strategy_1_regret)]<-0
full_data$p1_strategy_2_regret[is.na(full_data$p1_strategy_2_regret)]<-0
full_data$p2_strategy_0_regret[is.na(full_data$p2_strategy_0_regret)]<-0
full_data$p2_strategy_1_regret[is.na(full_data$p2_strategy_1_regret)]<-0
full_data$p2_strategy_2_regret[is.na(full_data$p2_strategy_2_regret)]<-0

# remove observations when regret is not correctly recorded
full_data = filter(full_data, p1_regret0 != 'error' & p2_regret0 != 'error')

# update regret for those sessions with no regret columns
full_data = full_data %>% mutate(
  p1_strategy_0_regret = ifelse(adjust_regret==TRUE, p1_strategy_0_regret, as.numeric(p1_regret0)),
  p1_strategy_1_regret = ifelse(adjust_regret==TRUE, p1_strategy_1_regret, as.numeric(p1_regret1)),
  p1_strategy_2_regret = ifelse(adjust_regret==TRUE, p1_strategy_2_regret, as.numeric(p1_regret2)),
  p2_strategy_0_regret = ifelse(adjust_regret==TRUE, p2_strategy_0_regret, as.numeric(p2_regret0)),
  p2_strategy_1_regret = ifelse(adjust_regret==TRUE, p2_strategy_1_regret, as.numeric(p2_regret1)),
  p2_strategy_2_regret = ifelse(adjust_regret==TRUE, p2_strategy_2_regret, as.numeric(p2_regret2)),
)

# update dta file
write_dta(full_data,here("Data", "stata.dta"))
write.csv(full_data,here("Data", "data_all.csv"))


##### Pool p1 and p2 data #####
#p1 dataset
df_p1 = full_data
df_p1 = df_p1 %>% select(-c(p2_code, p2_role, p2_strategy, p2_target,
                            p2_regret0, p2_regret1, p2_regret2, p2_payoff,
                            p2_strategy_0, p2_strategy_1, p2_strategy_2, p2_switch,
                            p2_strategy_0_regret, p2_strategy_1_regret, p2_strategy_2_regret))
df_p1 = df_p1 %>% select(-c(p3_code, p3_role, p3_strategy, p3_target,
                            p3_regret0, p3_regret1, p3_regret2, p3_payoff,
                            p3_strategy_0, p3_strategy_1, p3_strategy_2))
df_p1 = df_p1 %>% select(-c(p1_regret0, p1_regret1, p1_regret2, p1_target))
names(df_p1)[names(df_p1)=="p1_code"]="player_code"
names(df_p1)[names(df_p1)=="p1_role"]="player_role"
names(df_p1)[names(df_p1)=="p1_strategy"]="player_strategy"
names(df_p1)[names(df_p1)=="p1_strategy_0"]="player_strategy0"
names(df_p1)[names(df_p1)=="p1_strategy_1"]="player_strategy1"
names(df_p1)[names(df_p1)=="p1_strategy_2"]="player_strategy2"
names(df_p1)[names(df_p1)=="p1_payoff"]="player_payoff"
names(df_p1)[names(df_p1)=="p1_strategy_0_regret"]="player_avgpay0"
names(df_p1)[names(df_p1)=="p1_strategy_1_regret"]="player_avgpay1"
names(df_p1)[names(df_p1)=="p1_strategy_2_regret"]="player_avgpay2"
names(df_p1)[names(df_p1)=="p1_switch"]="player_switch"

#p2 dataset
df_p2 = full_data
df_p2 = df_p2 %>% select(-c(p1_code, p1_role, p1_strategy, p1_target,
                            p1_regret0, p1_regret1, p1_regret2, p1_payoff,
                            p1_strategy_0, p1_strategy_1, p1_strategy_2, p1_switch,
                            p1_strategy_0_regret, p1_strategy_1_regret, p1_strategy_2_regret))
df_p2 = df_p2 %>% select(-c(p3_code, p3_role, p3_strategy, p3_target,
                            p3_regret0, p3_regret1, p3_regret2, p3_payoff,
                            p3_strategy_0, p3_strategy_1, p3_strategy_2))
df_p2 = df_p2 %>% select(-c(p2_regret0, p2_regret1, p2_regret2, p2_target))
names(df_p2)[names(df_p2)=="p2_code"]="player_code"
names(df_p2)[names(df_p2)=="p2_role"]="player_role"
names(df_p2)[names(df_p2)=="p2_strategy"]="player_strategy"
names(df_p2)[names(df_p2)=="p2_strategy_0"]="player_strategy0"
names(df_p2)[names(df_p2)=="p2_strategy_1"]="player_strategy1"
names(df_p2)[names(df_p2)=="p2_strategy_2"]="player_strategy2"
names(df_p2)[names(df_p2)=="p2_payoff"]="player_payoff"
names(df_p2)[names(df_p2)=="p2_strategy_0_regret"]="player_avgpay0"
names(df_p2)[names(df_p2)=="p2_strategy_1_regret"]="player_avgpay1"
names(df_p2)[names(df_p2)=="p2_strategy_2_regret"]="player_avgpay2"
names(df_p2)[names(df_p2)=="p2_switch"]="player_switch"

# combine two datasets
df = rbind(df_p1, df_p2)

# standarize avgpay terms
uniquetreatment = unique(df$treatment)
df_list = list()
for (i in 1:length(uniquetreatment)){
  
  df_list[[i]] = filter(df, treatment == uniquetreatment[i])
  mean0 = mean(df$player_avgpay0)
  mean1 = mean(df$player_avgpay1)
  mean2 = mean(df$player_avgpay2)
  sd0 = sd(df$player_avgpay0)
  sd1 = sd(df$player_avgpay1)
  sd2 = sd(df$player_avgpay2)
  df_list[[i]] = df_list[[i]] %>% mutate(player_avgpay0_standard = (player_avgpay0-mean0)/sd0)
  df_list[[i]] = df_list[[i]] %>% mutate(player_avgpay1_standard = (player_avgpay1-mean1)/sd1)
  df_list[[i]] = df_list[[i]] %>% mutate(player_avgpay2_standard = (player_avgpay2-mean2)/sd2)
}

df_new = rbind(df_list[[1]], df_list[[2]], df_list[[3]], df_list[[4]])

# add new group id
df$cluster_id = paste(df$session_round_id, df$player_code)

# update dta file
write_dta(df,here("Data", "stata_pool.dta"))
rm(df_list, df_new, df_p1, df_p2, df)

##### Pool p1 and p2 data and reconstrcut the dataset to study switch and avgpaydiff #####
#p1 dataset
df_p1 = full_data
df_p1 = df_p1 %>% select(-c(p2_code, p2_role, p2_strategy, p2_target,
                            p2_regret0, p2_regret1, p2_regret2, p2_payoff,
                            p2_strategy_0, p2_strategy_1, p2_strategy_2, p2_switch,
                            p2_strategy_0_regret, p2_strategy_1_regret, p2_strategy_2_regret))
df_p1 = df_p1 %>% select(-c(p3_code, p3_role, p3_strategy, p3_target,
                            p3_regret0, p3_regret1, p3_regret2, p3_payoff,
                            p3_strategy_0, p3_strategy_1, p3_strategy_2))
df_p1 = df_p1 %>% select(-c(p1_regret0, p1_regret1, p1_regret2, p1_target))
names(df_p1)[names(df_p1)=="p1_code"]="player_code"
names(df_p1)[names(df_p1)=="p1_role"]="player_role"
names(df_p1)[names(df_p1)=="p1_strategy"]="player_strategy"
names(df_p1)[names(df_p1)=="p1_strategy_0"]="player_strategy0"
names(df_p1)[names(df_p1)=="p1_strategy_1"]="player_strategy1"
names(df_p1)[names(df_p1)=="p1_strategy_2"]="player_strategy2"
names(df_p1)[names(df_p1)=="p1_payoff"]="player_payoff"
names(df_p1)[names(df_p1)=="p1_strategy_0_regret"]="player_avgpay0"
names(df_p1)[names(df_p1)=="p1_strategy_1_regret"]="player_avgpay1"
names(df_p1)[names(df_p1)=="p1_strategy_2_regret"]="player_avgpay2"
names(df_p1)[names(df_p1)=="p1_switch"]="player_switch"

#p2 dataset
df_p2 = full_data
df_p2 = df_p2 %>% select(-c(p1_code, p1_role, p1_strategy, p1_target,
                            p1_regret0, p1_regret1, p1_regret2, p1_payoff,
                            p1_strategy_0, p1_strategy_1, p1_strategy_2, p1_switch,
                            p1_strategy_0_regret, p1_strategy_1_regret, p1_strategy_2_regret))
df_p2 = df_p2 %>% select(-c(p3_code, p3_role, p3_strategy, p3_target,
                            p3_regret0, p3_regret1, p3_regret2, p3_payoff,
                            p3_strategy_0, p3_strategy_1, p3_strategy_2))
df_p2 = df_p2 %>% select(-c(p2_regret0, p2_regret1, p2_regret2, p2_target))
names(df_p2)[names(df_p2)=="p2_code"]="player_code"
names(df_p2)[names(df_p2)=="p2_role"]="player_role"
names(df_p2)[names(df_p2)=="p2_strategy"]="player_strategy"
names(df_p2)[names(df_p2)=="p2_strategy_0"]="player_strategy0"
names(df_p2)[names(df_p2)=="p2_strategy_1"]="player_strategy1"
names(df_p2)[names(df_p2)=="p2_strategy_2"]="player_strategy2"
names(df_p2)[names(df_p2)=="p2_payoff"]="player_payoff"
names(df_p2)[names(df_p2)=="p2_strategy_0_regret"]="player_avgpay0"
names(df_p2)[names(df_p2)=="p2_strategy_1_regret"]="player_avgpay1"
names(df_p2)[names(df_p2)=="p2_strategy_2_regret"]="player_avgpay2"
names(df_p2)[names(df_p2)=="p2_switch"]="player_switch"

# combine two datasets
df = rbind(df_p1, df_p2)
rm(df_p1, df_p2)

# add new group id
df$cluster_id = paste(df$session_round_id, df$player_code)

# further separate the dataset to construct switch vs avgpaydiff
# BM 0 to 1
df_bm01 = df %>% group_by(cluster_id) %>% filter(
  game == 'BM' & lag(player_strategy == 0))
df_bm01 = df_bm01 %>% group_by(cluster_id) %>% mutate(
  player_switch_new = ifelse(player_strategy==1, 1, 0),
  player_avgpaydiff = player_avgpay1 - player_avgpay0,
  direction = 'bm01')

# BM 1 to 0
df_bm10 = df %>% group_by(cluster_id) %>% filter(
  game == 'BM' & lag(player_strategy == 1))
df_bm10 = df_bm10 %>% group_by(cluster_id) %>% mutate(
  player_switch_new = ifelse(player_strategy==0, 1, 0),
  player_avgpaydiff = player_avgpay0 - player_avgpay1,
  direction = 'bm10')

# MV 0 to 1
df_mv01 = df %>% group_by(cluster_id) %>% filter(
  game == 'MV' & lag(player_strategy == 0))
df_mv01 = df_mv01 %>% group_by(cluster_id) %>% mutate(
  player_switch_new = ifelse(player_strategy==1, 1, 0),
  player_avgpaydiff = player_avgpay1 - player_avgpay0,
  direction = 'mv01')

# MV 0 to 2
df_mv02 = df %>% group_by(cluster_id) %>% filter(
  game == 'MV' & lag(player_strategy == 0))
df_mv02 = df_mv02 %>% group_by(cluster_id) %>% mutate(
  player_switch_new = ifelse(player_strategy==2, 1, 0),
  player_avgpaydiff = player_avgpay2 - player_avgpay0,
  direction = 'mv02')

# MV 1 to 0
df_mv10 = df %>% group_by(cluster_id) %>% filter(
  game == 'MV' & lag(player_strategy == 1))
df_mv10 = df_mv10 %>% group_by(cluster_id) %>% mutate(
  player_switch_new = ifelse(player_strategy==0, 1, 0),
  player_avgpaydiff = player_avgpay0 - player_avgpay1,
  direction = 'mv10')

# MV 1 to 2
df_mv12 = df %>% group_by(cluster_id) %>% filter(
  game == 'MV' & lag(player_strategy == 1))
df_mv12 = df_mv12 %>% group_by(cluster_id) %>% mutate(
  player_switch_new = ifelse(player_strategy==2, 1, 0),
  player_avgpaydiff = player_avgpay2 - player_avgpay1,
  direction = 'mv12')

# MV 2 to 0
df_mv20 = df %>% group_by(cluster_id) %>% filter(
  game == 'MV' & lag(player_strategy == 2))
df_mv20 = df_mv20 %>% group_by(cluster_id) %>% mutate(
  player_switch_new = ifelse(player_strategy==0, 1, 0),
  player_avgpaydiff = player_avgpay0 - player_avgpay2,
  direction = 'mv20')

# MV 2 to 1
df_mv21 = df %>% group_by(cluster_id) %>% filter(
  game == 'MV' & lag(player_strategy == 2))
df_mv21 = df_mv21 %>% group_by(cluster_id) %>% mutate(
  player_switch_new = ifelse(player_strategy==1, 1, 0),
  player_avgpaydiff = player_avgpay1 - player_avgpay2,
  direction = 'mv21')

# combine all dataset
df_new = rbind(df_bm01, df_bm10, df_mv01, df_mv02, df_mv10, df_mv12, df_mv20, df_mv21)
rm(df_bm01, df_bm10, df_mv01, df_mv02, df_mv10, df_mv12, df_mv20, df_mv21)

# construct a new cluster variable
df_new$cluster_id_dir = paste(df_new$cluster_id, df_new$direction)

# update dta file
write_dta(df_new,here("Data", "stata_pool_dir.dta"))



