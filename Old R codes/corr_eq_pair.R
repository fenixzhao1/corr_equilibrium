##### Data preparation #####
# load packages
library(ggplot2)
library(dplyr)
library(xtable)
library(haven)

# add paths
path1<-"D:/Dropbox/Working Papers/Correlated Equilibrium/data/produce/MVL_8pxwav0s.csv" 
path2<-"D:/Dropbox/Working Papers/Correlated Equilibrium/data/produce/MVH_4d93o8o2.csv" 
path3<-"D:/Dropbox/Working Papers/Correlated Equilibrium/data/produce/BMH_p1cc2ryg.csv" 
path4<-"D:/Dropbox/Working Papers/Correlated Equilibrium/data/produce/BML_pn1grluj.csv"
path5<-"D:/Dropbox/Working Papers/Correlated Equilibrium/data/produce/MVL_4pk5wq8n.csv" 
path6<-"D:/Dropbox/Working Papers/Correlated Equilibrium/data/produce/MVH_dpmkufvg.csv" 
path7<-"D:/Dropbox/Working Papers/Correlated Equilibrium/data/produce/BML_gcsgh2se.csv"
path8<-"D:/Dropbox/Working Papers/Correlated Equilibrium/data/produce/BMH_ju0cgjd9.csv"
path9<-"D:/Dropbox/Working Papers/Correlated Equilibrium/data/produce/BML_wyrupym7.csv"
path10<-"D:/Dropbox/Working Papers/Correlated Equilibrium/data/produce/BML_zepokgc0.csv"
path11<-"D:/Dropbox/Working Papers/Correlated Equilibrium/data/produce/MVL_rs7rl6lx.csv"

path12<-"D:/Dropbox/Working Papers/Correlated Equilibrium/data/produce/BML_3_dlpeimt2.csv"
path13<-"D:/Dropbox/Working Papers/Correlated Equilibrium/data/produce/BML_3_m1ix0n73.csv"
path14<-"D:/Dropbox/Working Papers/Correlated Equilibrium/data/produce/BML_3_s59xbery.csv"
path15<-"D:/Dropbox/Working Papers/Correlated Equilibrium/data/produce/BMH_3_hen9kyv7.csv"
path16<-"D:/Dropbox/Working Papers/Correlated Equilibrium/data/produce/BMH_3_3vfdysx7.csv"
path17<-"D:/Dropbox/Working Papers/Correlated Equilibrium/data/produce/MVL_3_tlde6vfl.csv"
path18<-"D:/Dropbox/Working Papers/Correlated Equilibrium/data/produce/MVL_3_zt8he0n9.csv"
path19<-"D:/Dropbox/Working Papers/Correlated Equilibrium/data/produce/MVH_3_glly017c.csv"
path20<-"D:/Dropbox/Working Papers/Correlated Equilibrium/data/produce/MVH_3_jb5b2zsg.csv"
path21<-"D:/Dropbox/Working Papers/Correlated Equilibrium/data/produce/BML_3_07gjoche.csv"

figures<-"D:/Dropbox/Working Papers/Correlated Equilibrium/data/figures/"

# load data from the first four sessions and add regret columns
df_1 <- read.csv(path1, header = T, stringsAsFactors = FALSE)
df_1 = select(df_1, 1:27)
df_2 <- read.csv(path2, header = T, stringsAsFactors = FALSE)
df_2 = select(df_2, 1:27)
df_3 <- read.csv(path3, header = T, stringsAsFactors = FALSE)
df_3 = select(df_3, 1:27)
df_4 <- read.csv(path4, header = T, stringsAsFactors = FALSE)
df_4 = select(df_4, 1:27)

full_data = rbind(df_1, df_2, df_3, df_4)
rm(df_1, df_2, df_3, df_4)
full_data = full_data %>% mutate(
  p1_regret0 = 0,
  p1_regret1 = 0,
  p1_regret2 = 0,
  p2_regret0 = 0,
  p2_regret1 = 0,
  p2_regret2 = 0,
  p3_regret0 = 0,
  p3_regret1 = 0,
  p3_regret2 = 0
)

# load data from the rest of the sessions
df_5 <- read.csv(path5, header = T, stringsAsFactors = FALSE)
df_5 = select(df_5, 1:36)
df_6 <- read.csv(path6, header = T, stringsAsFactors = FALSE)
df_6 = select(df_6, 1:36)
df_7 <- read.csv(path7, header = T, stringsAsFactors = FALSE)
df_7 = select(df_7, 1:36)
df_8 <- read.csv(path8, header = T, stringsAsFactors = FALSE)
df_8 = select(df_8, 1:36)
df_9 <- read.csv(path9, header = T, stringsAsFactors = FALSE)
df_9 = select(df_9, 1:36)
df_10 <- read.csv(path10, header = T, stringsAsFactors = FALSE)
df_10 = select(df_10, 1:36)
df_11 <- read.csv(path11, header = T, stringsAsFactors = FALSE)
df_11 = select(df_11, 1:36)
df_12 <- read.csv(path12, header = T, stringsAsFactors = FALSE)
df_12 = select(df_12, 1:36)
df_13 <- read.csv(path13, header = T, stringsAsFactors = FALSE)
df_13 = select(df_13, 1:36)
df_14 <- read.csv(path14, header = T, stringsAsFactors = FALSE)
df_14 = select(df_14, 1:36)
df_15 <- read.csv(path15, header = T, stringsAsFactors = FALSE)
df_15 = select(df_15, 1:36)
df_16 <- read.csv(path16, header = T, stringsAsFactors = FALSE)
df_16 = select(df_16, 1:36)
df_17 <- read.csv(path17, header = T, stringsAsFactors = FALSE)
df_17 = select(df_17, 1:36)
df_18 <- read.csv(path18, header = T, stringsAsFactors = FALSE)
df_18 = select(df_18, 1:36)
df_19 <- read.csv(path19, header = T, stringsAsFactors = FALSE)
df_19 = select(df_19, 1:36)
df_20 <- read.csv(path20, header = T, stringsAsFactors = FALSE)
df_20 = select(df_20, 1:36)
df_21 <- read.csv(path21, header = T, stringsAsFactors = FALSE)
df_21 = select(df_21, 1:36)

full_data = rbind(full_data, df_5, df_6, df_7, df_8, df_9, df_10, df_11, df_12, df_13,
                  df_14, df_15, df_16, df_17, df_18, df_19, df_20, df_21)
rm(df_5, df_6, df_7, df_8, df_9, df_10, df_11, df_12, df_13, df_14, df_15, 
   df_16, df_17, df_18, df_19, df_20, df_21)
rm(path1, path2, path3, path4, path5, path6, path7, path8, path9, path10, path11,
   path12, path13, path14, path15, path16, path17, path18, path19, path20, path21)

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
#full_data = full_data %>% mutate(matching = ifelse(mean_matching == TRUE, 'Meanmatch', 'Pairwise'))
#full_data = full_data %>% mutate(time = ifelse(num_subperiods == 0, 'Continuous', 'Discrete'))
full_data = full_data %>% mutate(game = ifelse(game == 'BM', 'CH', 'MV'))
full_data = full_data %>% mutate(information = ifelse(max_info == TRUE | max_info == 'True', 'H', 'L'))
full_data = full_data %>% mutate(regret_mode = ifelse(regret==2, 'A', ifelse(regret==3, 'C', 'Origin')))
full_data$treatment = paste(full_data$game, full_data$information, full_data$regret_mode, sep = '_')

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
  if (full_data$game[i] == 'CH'){
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
  if (full_data$game[i] == 'CH'){
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

for(row in seq(full_data$tick[full_data$game=="CH"])){
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

# create regret
full_data = 
  full_data %>%
  group_by(session_round_pair_id) %>%
  mutate(p1_strategy_0_regret= cumsum(coalesce(lag(p1_payoff), 0)*coalesce(lag(p1_strategy_0), 0))/cumsum(coalesce(lag(p1_strategy_0),0)),
         p1_strategy_1_regret= cumsum(coalesce(lag(p1_payoff), 0)*coalesce(lag(p1_strategy_1), 0))/cumsum(coalesce(lag(p1_strategy_1),0)),
         p1_strategy_2_regret= cumsum(coalesce(lag(p1_payoff), 0)*coalesce(lag(p1_strategy_2), 0))/cumsum(coalesce(lag(p1_strategy_2),0)),
         p2_strategy_0_regret= cumsum(coalesce(lag(p2_payoff), 0)*coalesce(lag(p2_strategy_0), 0))/cumsum(coalesce(lag(p2_strategy_0),0)),
         p2_strategy_1_regret= cumsum(coalesce(lag(p2_payoff), 0)*coalesce(lag(p2_strategy_1), 0))/cumsum(coalesce(lag(p2_strategy_1),0)),
         p2_strategy_2_regret= cumsum(coalesce(lag(p2_payoff), 0)*coalesce(lag(p2_strategy_2), 0))/cumsum(coalesce(lag(p2_strategy_2),0)),
         #p3_strategy_0_regret= cumsum(coalesce(lag(p2_payoff), 0)*coalesce(lag(p3_strategy_0), 0))/cumsum(coalesce(lag(p3_strategy_0),0)),
         #p3_strategy_1_regret= cumsum(coalesce(lag(p2_payoff), 0)*coalesce(lag(p3_strategy_1), 0))/cumsum(coalesce(lag(p3_strategy_1),0)),
         #p3_strategy_2_regret= cumsum(coalesce(lag(p2_payoff), 0)*coalesce(lag(p3_strategy_2), 0))/cumsum(coalesce(lag(p3_strategy_2),0)),
         p1_switch = ifelse(abs(p1_strategy-lag(p1_strategy))>0, 1, 0),
         p2_switch = ifelse(abs(p2_strategy-lag(p2_strategy))>0, 1, 0),
         #p3_switch = ifelse(abs(p3_strategy-lag(p3_strategy))>0, 1, 0)
         )

full_data$p1_strategy_0_regret[is.na(full_data$p1_strategy_0_regret)]<-0
full_data$p1_strategy_1_regret[is.na(full_data$p1_strategy_1_regret)]<-0
full_data$p1_strategy_2_regret[is.na(full_data$p1_strategy_2_regret)]<-0
full_data$p2_strategy_0_regret[is.na(full_data$p2_strategy_0_regret)]<-0
full_data$p2_strategy_1_regret[is.na(full_data$p2_strategy_1_regret)]<-0
full_data$p2_strategy_2_regret[is.na(full_data$p2_strategy_2_regret)]<-0
#full_data$p3_strategy_0_regret[is.na(full_data$p3_strategy_0_regret)]<-0
#full_data$p3_strategy_1_regret[is.na(full_data$p3_strategy_1_regret)]<-0
#full_data$p3_strategy_2_regret[is.na(full_data$p3_strategy_2_regret)]<-0

# update dta file
write_dta(full_data, "D:/Dropbox/Working Papers/Correlated Equilibrium/data/produce/stata.dta")

# # check the calculated regret and recorded regret
# regret_data = filter(full_data, session_code == '4pk5wq8n' | session_code == 'dpmkufvg')
# regret_data = regret_data %>% mutate(
#   p1_0diff = p1_strategy_0_regret - p1_regret0,
#   p1_1diff = p1_strategy_1_regret - p1_regret1,
#   p1_2diff = p1_strategy_2_regret - p1_regret2,
#   p2_0diff = p2_strategy_0_regret - p2_regret0,
#   p2_1diff = p2_strategy_1_regret - p2_regret1,
#   p2_2diff = p2_strategy_2_regret - p2_regret2
# )
# summary(regret_data$p1_0diff)
# summary(regret_data$p1_1diff)
# summary(regret_data$p1_2diff)
# summary(regret_data$p2_0diff)
# summary(regret_data$p2_1diff)
# summary(regret_data$p2_2diff)
# 
# uniquepair = unique(regret_data$session_round_pair_id)
# df_round = filter(regret_data, session_round_pair_id == uniquepair[1])
# df_round = select(df_round, c(session_round_pair_id, period,
#                               p1_strategy, p2_strategy, p1_regret0, p1_regret1, p1_regret2,
#                               p1_strategy_0_regret, p1_strategy_1_regret, p1_strategy_2_regret))
# write.csv(df_round, "D:/Dropbox/sample.csv")


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
df_new$cluster_id = paste(df_new$session_round_id, df_new$player_code)

# update dta file
write_dta(df_new, "D:/Dropbox/Working Papers/Correlated Equilibrium/data/produce/stata_pool.dta")
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
  game == 'CH' & lag(player_strategy == 0))
df_bm01 = df_bm01 %>% group_by(cluster_id) %>% mutate(
  player_switch_new = ifelse(player_strategy==1, 1, 0),
  player_avgpaydiff = player_avgpay1 - player_avgpay0,
  direction = 'bm01')

# BM 1 to 0
df_bm10 = df %>% group_by(cluster_id) %>% filter(
  game == 'CH' & lag(player_strategy == 1))
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
write_dta(df_new, "D:/Dropbox/Working Papers/Correlated Equilibrium/data/produce/stata_pool_dir.dta")
rm(df, df_new)


##### Pool p1 and p2 data and reconstruct for multinomial logit in MV #####
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

# onyl keep MV data
df = filter(df, game == 'MV')

# further separate the dataset to construct switch vs avgpaydiff
# MV start with 0
df_mv0 = df %>% group_by(cluster_id) %>% filter(lag(player_strategy == 0))
df_mv0 = df_mv0 %>% group_by(cluster_id) %>% mutate(
  player_switch = ifelse(player_strategy==0, 0, ifelse(player_strategy==1, 1, 2)),
  player_avgpaydiff1 = player_avgpay1 - player_avgpay0,
  player_avgpaydiff2 = player_avgpay2 - player_avgpay0,
  direction = 'mv0')

# MV start with 1
df_mv1 = df %>% group_by(cluster_id) %>% filter(lag(player_strategy == 1))
df_mv1 = df_mv1 %>% group_by(cluster_id) %>% mutate(
  player_switch = ifelse(player_strategy==1, 0, ifelse(player_strategy==2, 1, 2)),
  player_avgpaydiff1 = player_avgpay2 - player_avgpay1,
  player_avgpaydiff2 = player_avgpay0 - player_avgpay1,
  direction = 'mv1')

# MV start with 2
df_mv2 = df %>% group_by(cluster_id) %>% filter(lag(player_strategy == 2))
df_mv2 = df_mv2 %>% group_by(cluster_id) %>% mutate(
  player_switch = ifelse(player_strategy==2, 0, ifelse(player_strategy==0, 1, 2)),
  player_avgpaydiff1 = player_avgpay0 - player_avgpay2,
  player_avgpaydiff2 = player_avgpay1 - player_avgpay2,
  direction = 'mv2')

# combine all dataset
df_new = rbind(df_mv0, df_mv1, df_mv2)
rm(df_mv0, df_mv1, df_mv2)

# construct a new cluster variable
df_new$cluster_id_dir = paste(df_new$cluster_id, df_new$direction)

# update dta file
write_dta(df_new, "D:/Dropbox/Working Papers/Correlated Equilibrium/data/produce/stata_pool_dir_mv.dta")
rm(df, df_new)


##### Pair-level data (not used) #####
# pair level dynamics
full_data = full_data %>% mutate(p2_strategy_jitter = p2_strategy + 0.01)
full_data = full_data %>% mutate(p3_strategy_jitter = ifelse(is.na(p3_strategy), NA, p3_strategy + 0.02))
uniquepairs = unique(full_data$session_round_pair_id)

# loop over pairs
for (i in 1:length(uniquepairs)){
  pairdata = subset(full_data, session_round_pair_id == uniquepairs[i])
  
  title = paste(as.character(pairdata$game[1]), as.character(pairdata$information[1]),
                as.character(uniquepairs[i]), sep = '_')
  file = paste(figures, title, sep = "")
  file = paste(file, ".png", sep = "")
  
  png(file, width = 700, height = 400)
  par(mai=c(1.5, 1, 0.5, 0.5))
  xy=par("usr")
  plot(pairdata$period, pairdata$p1_strategy, type='l', col="blue",
       xlab="time", ylab="strategy mixture", ylim = c(0,2), main=title)
  lines(pairdata$period, pairdata$p2_strategy_jitter, type='l', col="red")
  if (pairdata$game[1] == 'FP'){
    lines(pairdata$period, pairdata$p3_strategy_jitter, type='l', col="black")
  }
  legend(x=xy[2]-xinch(0.2), y=xy[3]-yinch(0.8), lty=1,
         c("p1", "p2", "p3"), col=c("blue", "red", "black"), xpd=TRUE)
  
  dev.off()
}


##### Pair-level data by type #####
# pair level dynamics
uniquepairs = unique(full_data$session_round_pair_id)

# loop over pairs
for (i in 1:length(uniquepairs)){
  pairdata = filter(full_data, session_round_pair_id == uniquepairs[i])
  pairdata_second = filter(pairdata, period > 20)
  # skip those incomplete pairs
  if (length(pairdata$tick) <= 20){next}
  
  # calculate profile index and draw graph for BM
  if (pairdata$game[1] == 'CH'){
    
    collude = round(mean(pairdata_second$type_collude), digits = 2)
    nash = round(mean(pairdata_second$type_Nash), digits = 2)
    
    title1 = paste(as.character(pairdata$treatment[1]),
                   as.character(pairdata$session_round_pair_id[1]), sep = '_')
    title2 = paste(as.character(pairdata$treatment[1]),
                   'Collude', as.character(collude), 'Nash', as.character(nash), sep = ' ')
    file = paste(figures, title1, sep = "")
    file = paste(file, ".png", sep = "")
    
    png(file, width = 700, height = 400)
    plot(pairdata$period, pairdata$type2, type='b', col="blue",
         xlab="time", ylab="strategy profile", ylim = c(0,4), main=title2)
    abline(h=1.5, col='red')
    abline(h=3.5, col='red')
    
    dev.off()
  }
  # same for MV
  else{
    
    p1adv = round(mean(pairdata_second$type_p1adv), digits = 2)
    p2adv = round(mean(pairdata_second$type_p2adv), digits = 2)
    
    title1 = paste(as.character(pairdata$treatment[1]),
                   as.character(pairdata$session_round_pair_id[1]), sep = '_')
    title2 = paste(as.character(pairdata$treatment[1]),
                   'p1 adv', as.character(p1adv), 'p2 adv', as.character(p2adv), sep = ' ')
    file = paste(figures, title1, sep = "")
    file = paste(file, ".png", sep = "")
    
    png(file, width = 700, height = 400)
    xy=par("usr")
    plot(pairdata$period, pairdata$type2, type='b', col="blue",
         xlab="time", ylab="strategy profile", ylim = c(0,9), main=title2)
    abline(h=3.5, col='red')
    abline(h=6.5, col='red')
    
    dev.off()
  }
}


##### Joint density #####
uniquetreatment = unique(full_data$treatment)
density_matrix = list()

df_second = filter(full_data, period > 20)

for (i in 1:length(uniquetreatment)){
  df_treatment = filter(df_second, treatment == uniquetreatment[i])
  
  # BM game
  if (df_treatment$game[1] == 'BM'){
    density_matrix[[i]] = matrix(NA, nrow = 2, ncol = 2)
    rownames(density_matrix[[i]]) = c('U', 'D')
    colnames(density_matrix[[i]]) = c('L', 'R')
    
    df_ul = filter(df_treatment, p1_strategy == 0 & p2_strategy == 0)
    df_ur = filter(df_treatment, p1_strategy == 0 & p2_strategy == 1)
    df_dl = filter(df_treatment, p1_strategy == 1 & p2_strategy == 0)
    df_dr = filter(df_treatment, p1_strategy == 1 & p2_strategy == 1)
    
    density_matrix[[i]][1,1] = length(df_ul$tick) / length(df_treatment$tick)
    density_matrix[[i]][1,2] = length(df_ur$tick) / length(df_treatment$tick)
    density_matrix[[i]][2,1] = length(df_dl$tick) / length(df_treatment$tick)
    density_matrix[[i]][2,2] = length(df_dr$tick) / length(df_treatment$tick)
    
    rm(df_ul, df_ur, df_dl, df_dr)
  }
  # MV game
  else if (df_treatment$game[1] == 'MV'){
    density_matrix[[i]] = matrix(NA, nrow = 3, ncol = 3)
    rownames(density_matrix[[i]]) = c('T', 'M', 'D')
    colnames(density_matrix[[i]]) = c('L', 'C', 'R')
    
    df_tl = filter(df_treatment, p1_strategy == 0 & p2_strategy == 0)
    df_tc = filter(df_treatment, p1_strategy == 0 & p2_strategy == 1)
    df_tr = filter(df_treatment, p1_strategy == 0 & p2_strategy == 2)
    df_ml = filter(df_treatment, p1_strategy == 1 & p2_strategy == 0)
    df_mc = filter(df_treatment, p1_strategy == 1 & p2_strategy == 1)
    df_mr = filter(df_treatment, p1_strategy == 1 & p2_strategy == 2)
    df_dl = filter(df_treatment, p1_strategy == 2 & p2_strategy == 0)
    df_dc = filter(df_treatment, p1_strategy == 2 & p2_strategy == 1)
    df_dr = filter(df_treatment, p1_strategy == 2 & p2_strategy == 2)
    
    density_matrix[[i]][1,1] = length(df_tl$tick) / length(df_treatment$tick)
    density_matrix[[i]][1,2] = length(df_tc$tick) / length(df_treatment$tick)
    density_matrix[[i]][1,3] = length(df_tr$tick) / length(df_treatment$tick)
    density_matrix[[i]][2,1] = length(df_ml$tick) / length(df_treatment$tick)
    density_matrix[[i]][2,2] = length(df_mc$tick) / length(df_treatment$tick)
    density_matrix[[i]][2,3] = length(df_mr$tick) / length(df_treatment$tick)
    density_matrix[[i]][3,1] = length(df_dl$tick) / length(df_treatment$tick)
    density_matrix[[i]][3,2] = length(df_dc$tick) / length(df_treatment$tick)
    density_matrix[[i]][3,3] = length(df_dr$tick) / length(df_treatment$tick)
    
    rm(df_tl, df_tc, df_tr, df_ml, df_mc, df_mr, df_dl, df_dc, df_dr)
  }
  # FT game
  else{
    density_matrix[[i]] = matrix(NA, nrow = 2, ncol = 6)
    rownames(density_matrix[[i]]) = c('U', 'D')
    colnames(density_matrix[[i]]) = c('LA', 'RA', 'LB', 'RB', 'LC', 'RC')
    
    df_ula = filter(df_treatment, p1_strategy == 0 & p2_strategy == 0 & p3_strategy == 0)
    df_ura = filter(df_treatment, p1_strategy == 0 & p2_strategy == 1 & p3_strategy == 0)
    df_dla = filter(df_treatment, p1_strategy == 1 & p2_strategy == 0 & p3_strategy == 0)
    df_dra = filter(df_treatment, p1_strategy == 1 & p2_strategy == 1 & p3_strategy == 0)
    df_ulb = filter(df_treatment, p1_strategy == 0 & p2_strategy == 0 & p3_strategy == 1)
    df_urb = filter(df_treatment, p1_strategy == 0 & p2_strategy == 1 & p3_strategy == 1)
    df_dlb = filter(df_treatment, p1_strategy == 1 & p2_strategy == 0 & p3_strategy == 1)
    df_drb = filter(df_treatment, p1_strategy == 1 & p2_strategy == 1 & p3_strategy == 1)
    df_ulc = filter(df_treatment, p1_strategy == 0 & p2_strategy == 0 & p3_strategy == 2)
    df_urc = filter(df_treatment, p1_strategy == 0 & p2_strategy == 1 & p3_strategy == 2)
    df_dlc = filter(df_treatment, p1_strategy == 1 & p2_strategy == 0 & p3_strategy == 2)
    df_drc = filter(df_treatment, p1_strategy == 1 & p2_strategy == 1 & p3_strategy == 2)

    density_matrix[[i]][1,1] = length(df_ula$tick) / length(df_treatment$tick)
    density_matrix[[i]][1,2] = length(df_ura$tick) / length(df_treatment$tick)
    density_matrix[[i]][2,1] = length(df_dla$tick) / length(df_treatment$tick)
    density_matrix[[i]][2,2] = length(df_dra$tick) / length(df_treatment$tick)
    density_matrix[[i]][1,3] = length(df_ulb$tick) / length(df_treatment$tick)
    density_matrix[[i]][1,4] = length(df_urb$tick) / length(df_treatment$tick)
    density_matrix[[i]][2,3] = length(df_dlb$tick) / length(df_treatment$tick)
    density_matrix[[i]][2,4] = length(df_drb$tick) / length(df_treatment$tick)
    density_matrix[[i]][1,5] = length(df_ulc$tick) / length(df_treatment$tick)
    density_matrix[[i]][1,6] = length(df_urc$tick) / length(df_treatment$tick)
    density_matrix[[i]][2,5] = length(df_dlc$tick) / length(df_treatment$tick)
    density_matrix[[i]][2,6] = length(df_drc$tick) / length(df_treatment$tick)
    
    rm(df_ula, df_ura, df_dla, df_dra, df_ulb, df_urb, df_dlb, df_drb, df_ulc, df_urc, df_dlc, df_drc)
  }
}

# table output
xtable(density_matrix[[1]], digits = 2, caption = uniquetreatment[1])
xtable(density_matrix[[2]], digits = 2, caption = uniquetreatment[2])
xtable(density_matrix[[3]], digits = 2, caption = uniquetreatment[3])
xtable(density_matrix[[4]], digits = 2, caption = uniquetreatment[4])
xtable(density_matrix[[5]], digits = 2, caption = uniquetreatment[5])
xtable(density_matrix[[6]], digits = 2, caption = uniquetreatment[6])
xtable(density_matrix[[7]], digits = 2, caption = uniquetreatment[7])
xtable(density_matrix[[8]], digits = 2, caption = uniquetreatment[8])


##### Aggregate over time (not used) #####
uniquetreatment = unique(full_data$treatment)

# generate aggregate data over period by treatments
aggregate_plot = list()
for (i in 1:length(uniquetreatment)){
  df_treatment = filter(full_data, treatment == uniquetreatment[i])
  # BM and MV data
  if (df_treatment$game[1] != 'FP'){
    aggregate_plot[[i]] = data.frame(
      p1_average_0 = tapply(df_treatment$p1_strategy_0, df_treatment$period, mean),
      p1_average_1 = tapply(df_treatment$p1_strategy_1, df_treatment$period, mean),
      p1_average_2 = tapply(df_treatment$p1_strategy_2, df_treatment$period, mean),
      p2_average_0 = tapply(df_treatment$p2_strategy_0, df_treatment$period, mean),
      p2_average_1 = tapply(df_treatment$p2_strategy_1, df_treatment$period, mean),
      p2_average_2 = tapply(df_treatment$p2_strategy_2, df_treatment$period, mean),
      period = tapply(df_treatment$period, df_treatment$period, mean)
    )
  }
  # FT data
  else{
    aggregate_plot[[i]] = data.frame(
      p1_average_0 = tapply(df_treatment$p1_strategy_0, df_treatment$period, mean),
      p1_average_1 = tapply(df_treatment$p1_strategy_1, df_treatment$period, mean),
      p1_average_2 = tapply(df_treatment$p1_strategy_2, df_treatment$period, mean),
      p2_average_0 = tapply(df_treatment$p2_strategy_0, df_treatment$period, mean),
      p2_average_1 = tapply(df_treatment$p2_strategy_1, df_treatment$period, mean),
      p2_average_2 = tapply(df_treatment$p2_strategy_2, df_treatment$period, mean),
      p3_average_0 = tapply(df_treatment$p3_strategy_0, df_treatment$period, mean),
      p3_average_1 = tapply(df_treatment$p3_strategy_1, df_treatment$period, mean),
      p3_average_2 = tapply(df_treatment$p3_strategy_2, df_treatment$period, mean),
      period = tapply(df_treatment$period, df_treatment$period, mean)
    )
  }
  
  # start to draw plot
  title = as.character(df_treatment$treatment[1])
  file = paste("D:/Dropbox/Working Papers/Correlated Equilibrium/data/figures/pairwise_aggregate/", title, sep = "")
  file = paste(file, ".png", sep = "")
  png(file, width = 800, height = 500)
  
  # BM plot
  if (df_treatment$game[1] == 'BM'){
    pic = ggplot(data = aggregate_plot[[i]]) +
      geom_line(aes(x=period, y=p1_average_1, colour='blue', linetype='solid')) +
      geom_line(aes(x=period, y=p2_average_1, colour='red', linetype='solid')) +
      scale_x_discrete(name='period', waiver(), limits=c(1,10,20,30,40)) +
      scale_y_continuous(name='average mixture', limits=c(0,1)) +
      ggtitle(title) + 
      theme_bw() + 
      scale_colour_manual(values=c('blue','red'), labels=c('p1-D', 'p2-R')) +
      theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
            axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
            axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))
  }
  # MV plot
  else if (df_treatment$game[1] == 'MV'){
    pic = ggplot(data = aggregate_plot[[i]]) +
      geom_line(aes(x=period, y=p1_average_1, colour='blue', linetype='solid')) +
      geom_line(aes(x=period, y=p1_average_2, colour='blue', linetype='dashed')) +
      geom_line(aes(x=period, y=p2_average_1, colour='red', linetype='solid')) +
      geom_line(aes(x=period, y=p2_average_2, colour='red', linetype='dashed')) +
      scale_x_discrete(name='period', waiver(), limits=c(1,10,20,30,40)) +
      scale_y_continuous(name='average mixture', limits=c(0,1)) +
      ggtitle(title) + 
      theme_bw() + 
      scale_colour_manual(values=c('blue','red'), labels=c('p1', 'p2')) +
      scale_linetype_manual(values=c('dashed', 'solid'), labels=c('B/R', 'M/C')) +
      theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
            axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
            axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))
  }
  # FT plot
  else if (df_treatment$game[1] == 'FP'){ 
    pic = ggplot(data = aggregate_plot[[i]]) +
      geom_line(aes(x=period, y=p1_average_1, colour='blue', linetype='solid')) +
      geom_line(aes(x=period, y=p2_average_1, colour='red', linetype='solid')) +
      geom_line(aes(x=period, y=p3_average_1, colour='black', linetype='solid')) +
      geom_line(aes(x=period, y=p3_average_2, colour='black', linetype='dashed')) +
      scale_x_discrete(name='period', waiver(), limits=c(1,10,20,30,40)) +
      scale_y_continuous(name='average mixture', limits=c(0,1)) +
      ggtitle(title) + 
      theme_bw() + 
      scale_colour_manual(values=c('black', 'blue', 'red'), labels=c('p3', 'p1', 'p2')) +
      scale_linetype_manual(values=c('dashed', 'solid'), labels=c('C', 'D/R/B')) +
      theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
            axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
            axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))
  }
  
  print(pic)
  dev.off()
}


##### Type over time #####
uniquetreatment = unique(full_data$treatment)

# generate aggregate data over period by treatments
aggregate_plot = list()
for (i in 1:length(uniquetreatment)){
  df_treatment = filter(full_data, treatment == uniquetreatment[i])
  # create dataset
  aggregate_plot[[i]] = data.frame(
    type_collude = tapply(df_treatment$type_collude, df_treatment$period, mean),
    type_diagonal = tapply(df_treatment$type_diagonal, df_treatment$period, mean),
    type_DL = tapply(df_treatment$type_DL, df_treatment$period, mean),
    type_middle = tapply(df_treatment$type_middle, df_treatment$period, mean),
    type_Nash = tapply(df_treatment$type_Nash, df_treatment$period, mean),
    type_others = tapply(df_treatment$type_others, df_treatment$period, mean),
    type_p1adv = tapply(df_treatment$type_p1adv, df_treatment$period, mean),
    type_p2adv = tapply(df_treatment$type_p2adv, df_treatment$period, mean),
    type_UL = tapply(df_treatment$type_UL, df_treatment$period, mean),
    period = tapply(df_treatment$period, df_treatment$period, mean)
  )

  # start to draw plot
  title = paste('typeovertime', as.character(df_treatment$treatment[1]), sep = '_')
  file = paste("D:/Dropbox/Working Papers/Correlated Equilibrium/writeup/figs/", title, sep = "")
  file = paste(file, ".png", sep = "")
  png(file, width = 1000, height = 500)
  
  # BM plot
  if (df_treatment$game[1] == 'CH'){
    pic = ggplot(data = aggregate_plot[[i]]) +
      geom_line(aes(x=period, y=type_collude, colour='blue')) +
      geom_line(aes(x=period, y=type_Nash, colour='red')) +
      geom_line(aes(x=period, y=type_UL, colour='black')) +
      scale_x_discrete(name='period', waiver(), limits=c(1,10,20,30,40)) +
      scale_y_continuous(name='average mixture', limits=c(0,1)) +
      ggtitle(title) + 
      theme_bw() + 
      scale_colour_manual(values=c('blue', 'red', 'black'), labels=c('UL', 'collude', 'either Nash')) +
      theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
            axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
            axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))
  }
  # MV plot
  else if (df_treatment$game[1] == 'MV'){
    pic = ggplot(data = aggregate_plot[[i]]) +
      geom_line(aes(x=period, y=type_p1adv, colour='blue')) +
      geom_line(aes(x=period, y=type_p2adv, colour='red')) +
      geom_line(aes(x=period, y=type_diagonal, colour='black')) +
      scale_x_discrete(name='period', waiver(), limits=c(1,10,20,30,40)) +
      scale_y_continuous(name='average mixture', limits=c(0,1)) +
      ggtitle(title) + 
      theme_bw() + 
      scale_colour_manual(values=c('blue', 'red', 'black'), labels=c('diagonal', 'p1_adv', 'p2_adv')) +
      theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
            axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
            axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))
  }
  # FT plot
  else if (df_treatment$game[1] == 'FP'){ 
    pic = ggplot(data = aggregate_plot[[i]]) +
      geom_line(aes(x=period, y=type_DL, colour='blue')) +
      geom_line(aes(x=period, y=type_middle, colour='red')) +
      geom_line(aes(x=period, y=type_others, colour='black')) +
      scale_x_discrete(name='period', waiver(), limits=c(1,10,20,30,40)) +
      scale_y_continuous(name='average mixture', limits=c(0,1)) +
      ggtitle(title) + 
      theme_bw() + 
      scale_colour_manual(values=c('blue', 'red', 'black'), labels=c('others','DL', 'middle')) +
      theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
            axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
            axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))
  }
  
  print(pic)
  dev.off()
}


##### Type over supergames #####
uniquetreatment = unique(full_data$treatment)

# generate aggregate data over period by treatments
aggregate_plot = list()
for (i in 1:length(uniquetreatment)){
  df_treatment = filter(full_data, treatment == uniquetreatment[i])
  df_treatment = filter(df_treatment, period > 20)
  # create dataset
  aggregate_plot[[i]] = data.frame(
    type_collude = tapply(df_treatment$type_collude, df_treatment$round, mean),
    type_diagonal = tapply(df_treatment$type_diagonal, df_treatment$round, mean),
    type_DL = tapply(df_treatment$type_DL, df_treatment$round, mean),
    type_middle = tapply(df_treatment$type_middle, df_treatment$round, mean),
    type_Nash = tapply(df_treatment$type_Nash, df_treatment$round, mean),
    type_others = tapply(df_treatment$type_others, df_treatment$round, mean),
    type_p1adv = tapply(df_treatment$type_p1adv, df_treatment$round, mean),
    type_p2adv = tapply(df_treatment$type_p2adv, df_treatment$round, mean),
    type_UL = tapply(df_treatment$type_UL, df_treatment$round, mean),
    round = tapply(df_treatment$round, df_treatment$round, mean)
  )
  
  # start to draw plot
  title = paste('typeovergame', as.character(df_treatment$treatment[1]), sep = '_')
  file = paste("D:/Dropbox/Working Papers/Correlated Equilibrium/writeup/figs/", title, sep = "")
  file = paste(file, ".png", sep = "")
  png(file, width = 1000, height = 500)
  
  # BM plot
  if (df_treatment$game[1] == 'CH'){
    pic = ggplot(data = aggregate_plot[[i]]) +
      geom_line(aes(x=round, y=type_collude, colour='blue')) +
      geom_line(aes(x=round, y=type_Nash, colour='red')) +
      geom_line(aes(x=round, y=type_UL, colour='black')) +
      scale_x_discrete(name='supergame', waiver(), limits=c(3,4,5,6,7,8,9,10)) +
      scale_y_continuous(name='average mixture', limits=c(0,1)) +
      ggtitle(title) + 
      theme_bw() + 
      scale_colour_manual(values=c('blue', 'red', 'black'), labels=c('UL', 'collude', 'either Nash')) +
      theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
            axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
            axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))
  }
  # MV plot
  else{
    pic = ggplot(data = aggregate_plot[[i]]) +
      geom_line(aes(x=round, y=type_p1adv, colour='blue')) +
      geom_line(aes(x=round, y=type_p2adv, colour='red')) +
      geom_line(aes(x=round, y=type_diagonal, colour='black')) +
      scale_x_discrete(name='supergame', waiver(), limits=c(3,4,5,6,7,8,9,10)) +
      scale_y_continuous(name='average mixture', limits=c(0,1)) +
      ggtitle(title) + 
      theme_bw() + 
      scale_colour_manual(values=c('blue', 'red', 'black'), labels=c('diagonal', 'p1_adv', 'p2_adv')) +
      theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
            axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
            axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))
  }
  
  print(pic)
  dev.off()
}


##### Type by pair #####
# select the data after period 20
uniquetreatment = unique(full_data$treatment)
df_second = filter(full_data, period > 20)
uniquepair = unique(df_second$session_round_pair_id)

# set up data container 
table = as.data.frame(matrix(nrow=3*length(uniquepair), ncol=6))
colnames(table)=c("ID", "treatment", "game", "info", "type", "freq")

for (i in 1:length(uniquepair)){
  
  df_pair = filter(df_second, session_round_pair_id == uniquepair[i])
  # each pair has 3 rows
  # update ID, game and information treatment info
  table$ID[3*i-2] = df_pair$session_round_pair_id[1]
  table$ID[3*i-1] = df_pair$session_round_pair_id[1]
  table$ID[3*i] = df_pair$session_round_pair_id[1]
  
  table$treatment[3*i-2] = df_pair$treatment[1]
  table$treatment[3*i-1] = df_pair$treatment[1]
  table$treatment[3*i] = df_pair$treatment[1]
  
  table$game[3*i-2] = df_pair$game[1]
  table$game[3*i-1] = df_pair$game[1]
  table$game[3*i] = df_pair$game[1]
  
  table$info[3*i-2] = df_pair$information[1]
  table$info[3*i-1] = df_pair$information[1]
  table$info[3*i] = df_pair$information[1]
  
  # update type and frequency info for BM
  if (df_pair$game[1] == 'CH'){
    table$type[3*i-2] = 'collude'
    table$freq[3*i-2] = sum(df_pair$type_collude)
    table$type[3*i-1] = 'Nash'
    table$freq[3*i-1] = sum(df_pair$type_Nash)
    table$type[3*i] = 'UL'
    table$freq[3*i] = sum(df_pair$type_UL)
  }
  # update type and frequency info for MV
  else{
    table$type[3*i-2] = 'p1 advantage'
    table$freq[3*i-2] = sum(df_pair$type_p1adv)
    table$type[3*i-1] = 'p2 advantage'
    table$freq[3*i-1] = sum(df_pair$type_p2adv)
    table$type[3*i] = 'diagonal'
    table$freq[3*i] = sum(df_pair$type_diagonal)
  }
}

# loop over treatments to create figure
for (i in 1:length(uniquetreatment)){
  
  table_game = filter(table, treatment == uniquetreatment[i])
  
  # type pair for BM barplot
  if (table_game$game[1] == 'CH'){
    
    table_game$te<-0
    table_game$te[table_game$type=="collude"]<-1
    table_game$te<-table_game$te*table_game$freq
    table_game$ID= with(table_game, reorder(ID, te, max))
    
    title = paste('pairtype', as.character(table_game$treatment[1]), sep = '_')
    file = paste("D:/Dropbox/Working Papers/Correlated Equilibrium/writeup/figs/", title, sep = "")
    file = paste(file, ".png", sep = "")
    png(file, width = 1000, height = 500)
    
    pic = ggplot(table_game, aes(x= reorder(ID, freq), y=freq, fill=type)) +
      geom_bar(stat="identity", position='fill', width=1, colour='white') +
      ggtitle(title) +
      scale_x_discrete(name='Pair ID', waiver()) +
      scale_y_continuous(name='strategy profile type') +
      scale_fill_manual(values=c("collude"="#D72B13", "Nash"="#4B54AF", "UL"="grey")) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = 30),
            axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25),
            legend.text = element_text(size = 15))
    
    print(pic)
    dev.off()
  }
  # type pair for MV barplot
  else{
    table_game$te<-0
    table_game$te[table_game$type=="p1 advantage"]<-1
    table_game$te<-table_game$te*table_game$freq
    table_game$ID= with(table_game, reorder(ID, te, max))
  
    title = paste('pairtype', as.character(table_game$treatment[1]), sep = '_')
    file = paste("D:/Dropbox/Working Papers/Correlated Equilibrium/writeup/figs/", title, sep = "")
    file = paste(file, ".png", sep = "")
    png(file, width = 1000, height = 500)
  
    pic = ggplot(table_game, aes(x=ID, y=freq, fill=type)) +
      geom_bar(stat="identity", position='fill', width=1, colour='white') +
      ggtitle(title) +
      scale_x_discrete(name='Pair ID', waiver()) +
      scale_y_continuous(name='strategy profile type') +
      scale_fill_manual(values=c("p1 advantage"="#ECCF05", "p2 advantage"="#7BBE56", 'diagonal'='grey')) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = 30),
            axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 25),
            legend.text = element_text(size = 15))
  
    print(pic)
    dev.off()
  }
}


##### duration at strategy profiles - corr eq (not used) #####
## stay at corr eq positions
# create dataset
full_data = arrange(full_data, full_data$session_code, full_data$subsession_id, full_data$id_in_subsession, full_data$tick)
uniquepairs = unique(full_data$session_round_pair_id)

df_length = data.frame(matrix(0, nrow = 1, ncol = 1))
df_char = data.frame(matrix(NA, nrow = 1, ncol = 4))
colnames(df_length) = c('length')
colnames(df_char) = c('session_round_pair_id', 'treatment', 'game', 'info')

# loop over pairs to update the dataset
for (i in 1:length(uniquepairs)){
  df_pair = filter(full_data, session_round_pair_id == uniquepairs[i])
  # initialize the event observation and parameters
  pair_id = df_pair$session_round_pair_id[1]
  treatment = df_pair$treatment[1]
  game = df_pair$game[1]
  info = df_pair$information[1]
  time = 0
  
  # loop over observations in BM
  if (game == 'BM'){
    for (j in 1:length(df_pair$period)){
      # j > 1
      if (j > 1){
        if (df_pair$type2[j] == df_pair$type2[j-1] & df_pair$type_UL[j] == 0){
          time = time + 1
        }
        else{
          if (time > 0){
            event = c(pair_id, treatment, game, info)
            df_length = rbind(df_length, time)
            df_char = rbind(df_char, event)
            time = 0
          }
          if (df_pair$type_UL[j] == 0){
            time = time + 1
          }
        }
      }
      # j = 1
      else{
        if (df_pair$type_UL[j] == 0){
          time = time + 1
        }
      }
      
      # record the last event
      if (j == length(df_pair$period) & time > 0){
        event = c(pair_id, treatment, game, info)
        df_length = rbind(df_length, time)
        df_char = rbind(df_char, event)
      }
    }
  }
  # loop over observations in MV
  else{
    for (j in 1:length(df_pair$period)){
      # j > 1
      if (j > 1){
        if (df_pair$type2[j] == df_pair$type2[j-1] & df_pair$type_diagonal[j] == 0){
          time = time + 1
        }
        else{
          if (time > 0){
            event = c(pair_id, treatment, game, info)
            df_length = rbind(df_length, time)
            df_char = rbind(df_char, event)
            time = 0
          }
          if (df_pair$type_diagonal[j] == 0){
            time = time + 1
          }
        }
      }
      # j = 1
      else{
        if (df_pair$type_diagonal[j] == 0){
          time = time + 1
        }
      }
      
      # record the last event
      if (j == length(df_pair$period) & time > 0){
        event = c(pair_id, treatment, game, info)
        df_length = rbind(df_length, time)
        df_char = rbind(df_char, event)
      }
    }
  }
}

# organize dataset
df_stay = cbind(df_length, df_char)
df_stay = filter(df_stay, is.na(treatment) == FALSE)

df_bm_max = filter(df_stay, game == 'BM' & info == 'MaxInfo')
df_bm_min = filter(df_stay, game == 'BM' & info == 'MinInfo')
df_mv_max = filter(df_stay, game == 'MV' & info == 'MaxInfo')
df_mv_min = filter(df_stay, game == 'MV' & info == 'MinInfo')

# set up plot for BM
title = 'distribution_duration_BM_correq'
file = paste("D:/Dropbox/Working Papers/Correlated Equilibrium/writeup/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 700, height = 300)
pic = ggplot() +
  stat_ecdf(geom="step", data=df_bm_max, aes(x=length, colour='blue')) +
  stat_ecdf(geom="step", data=df_bm_min, aes(x=length, colour='red')) +
  scale_x_continuous(name='duration(period)', waiver(), limits=c(0,10), breaks = c(0,2,4,6,8,10)) +
  scale_y_continuous(name='cdf') +
  theme_bw() + 
  scale_colour_manual(values=c('blue','red'), labels=c('MaxInfo','MinInfo')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()

# set up plot for MV
title = 'distribution_duration_MV_correq'
file = paste("D:/Dropbox/Working Papers/Correlated Equilibrium/writeup/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 700, height = 300)
pic = ggplot() +
  stat_ecdf(geom="step", data=df_mv_max, aes(x=length, colour='blue')) +
  stat_ecdf(geom="step", data=df_mv_min, aes(x=length, colour='red')) +
  scale_x_continuous(name='duration(period)', waiver(), limits=c(0,10), breaks = c(0,2,4,6,8,10)) +
  scale_y_continuous(name='cdf') +
  theme_bw() + 
  scale_colour_manual(values=c('blue','red'), labels=c('MaxInfo','MinInfo')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()


##### duration at strategy profiles - UL or diagonal (not used) #####
## stay at mimsmatch positions
# create dataset
full_data = arrange(full_data, full_data$session_code, full_data$subsession_id, full_data$id_in_subsession, full_data$tick)
uniquepairs = unique(full_data$session_round_pair_id)

df_length = data.frame(matrix(0, nrow = 1, ncol = 1))
df_char = data.frame(matrix(NA, nrow = 1, ncol = 4))
colnames(df_length) = c('length')
colnames(df_char) = c('session_round_pair_id', 'treatment', 'game', 'info')

# loop over pairs to update the dataset
for (i in 1:length(uniquepairs)){
  df_pair = filter(full_data, session_round_pair_id == uniquepairs[i])
  # initialize the event observation and parameters
  pair_id = df_pair$session_round_pair_id[1]
  treatment = df_pair$treatment[1]
  game = df_pair$game[1]
  info = df_pair$information[1]
  time = 0
  
  # loop over observations in BM
  if (game == 'BM'){
    for (j in 1:length(df_pair$period)){
      # j > 1
      if (j > 1){
        if (df_pair$type2[j] == df_pair$type2[j-1] & df_pair$type_UL[j] == 1){
          time = time + 1
        }
        else{
          if (time > 0){
            event = c(pair_id, treatment, game, info)
            df_length = rbind(df_length, time)
            df_char = rbind(df_char, event)
            time = 0
          }
          if (df_pair$type_UL[j] == 1){
            time = time + 1
          }
        }
      }
      # j = 1
      else{
        if (df_pair$type_UL[j] == 1){
          time = time + 1
        }
      }
      
      # record the last event
      if (j == length(df_pair$period) & time > 0){
        event = c(pair_id, treatment, game, info)
        df_length = rbind(df_length, time)
        df_char = rbind(df_char, event)
      }
    }
  }
  # loop over observations in MV
  else{
    for (j in 1:length(df_pair$period)){
      # j > 1
      if (j > 1){
        if (df_pair$type2[j] == df_pair$type2[j-1] & df_pair$type_diagonal[j] == 1){
          time = time + 1
        }
        else{
          if (time > 0){
            event = c(pair_id, treatment, game, info)
            df_length = rbind(df_length, time)
            df_char = rbind(df_char, event)
            time = 0
          }
          if (df_pair$type_diagonal[j] == 1){
            time = time + 1
          }
        }
      }
      # j = 1
      else{
        if (df_pair$type_diagonal[j] == 1){
          time = time + 1
        }
      }
      
      # record the last event
      if (j == length(df_pair$period) & time > 0){
        event = c(pair_id, treatment, game, info)
        df_length = rbind(df_length, time)
        df_char = rbind(df_char, event)
      }
    }
  }
}

# organize dataset
df_stay = cbind(df_length, df_char)
df_stay = filter(df_stay, is.na(treatment) == FALSE)

df_bm_max = filter(df_stay, game == 'BM' & info == 'MaxInfo')
df_bm_min = filter(df_stay, game == 'BM' & info == 'MinInfo')
df_mv_max = filter(df_stay, game == 'MV' & info == 'MaxInfo')
df_mv_min = filter(df_stay, game == 'MV' & info == 'MinInfo')

# set up plot for BM
title = 'distribution_duration_BM_mismatch'
file = paste("D:/Dropbox/Working Papers/Correlated Equilibrium/writeup/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 700, height = 300)
pic = ggplot() +
  stat_ecdf(geom="step", data=df_bm_max, aes(x=length, colour='blue')) +
  stat_ecdf(geom="step", data=df_bm_min, aes(x=length, colour='red')) +
  scale_x_continuous(name='duration(period)', waiver(), limits=c(0,10), breaks = c(0,2,4,6,8,10)) +
  scale_y_continuous(name='cdf') +
  theme_bw() + 
  scale_colour_manual(values=c('blue','red'), labels=c('MaxInfo','MinInfo')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()

# set up plot for MV
title = 'distribution_duration_MV_mismatch'
file = paste("D:/Dropbox/Working Papers/Correlated Equilibrium/writeup/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 700, height = 300)
pic = ggplot() +
  stat_ecdf(geom="step", data=df_mv_max, aes(x=length, colour='blue')) +
  stat_ecdf(geom="step", data=df_mv_min, aes(x=length, colour='red')) +
  scale_x_continuous(name='duration(period)', waiver(), limits=c(0,10), breaks = c(0,2,4,6,8,10)) +
  scale_y_continuous(name='cdf') +
  theme_bw() + 
  scale_colour_manual(values=c('blue','red'), labels=c('MaxInfo','MinInfo')) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))

print(pic)
dev.off()


##### Transition probability matrix #####
# set up matrix
uniquetreatment = unique(full_data$treatment)
transition_matrix = list()

# loop over treatments
for (i in 1:length(uniquetreatment)){
  
  df = filter(full_data, treatment == uniquetreatment[i])
  pairs = unique(df$session_round_pair_id)
  
  # transitions for BM
  if (df$game[1] == 'CH'){
    
    # generate the empty matrix
    transition = matrix(0, nrow = 4, ncol = 5)
    rownames(transition) = c('UL at t', 'Nash1 at t', 'Nash2 at t', 'Collude at t')
    colnames(transition) = c('UL at t+1', 'Nash1 at t+1', 'Nash2 at t+1', 'Collude at t+1', 'obs')
    
    # loop over pairs
    for (j in 1:length(pairs)){
      
      df_pair = filter(df, session_round_pair_id == pairs[j])
      # loop over observations
      for (k in 2:length(df_pair$tick)){
        
        # add transitions
        if (df_pair$type2[k-1] == 1){
          if (df_pair$type2[k] == 1){transition[1,1] = transition[1,1] + 1}
          if (df_pair$type2[k] == 2){transition[1,2] = transition[1,2] + 1}
          if (df_pair$type2[k] == 3){transition[1,3] = transition[1,3] + 1}
          if (df_pair$type2[k] == 4){transition[1,4] = transition[1,4] + 1}
        }
        if (df_pair$type2[k-1] == 2){
          if (df_pair$type2[k] == 1){transition[2,1] = transition[2,1] + 1}
          if (df_pair$type2[k] == 2){transition[2,2] = transition[2,2] + 1}
          if (df_pair$type2[k] == 3){transition[2,3] = transition[2,3] + 1}
          if (df_pair$type2[k] == 4){transition[2,4] = transition[2,4] + 1}
        }
        if (df_pair$type2[k-1] == 3){
          if (df_pair$type2[k] == 1){transition[3,1] = transition[3,1] + 1}
          if (df_pair$type2[k] == 2){transition[3,2] = transition[3,2] + 1}
          if (df_pair$type2[k] == 3){transition[3,3] = transition[3,3] + 1}
          if (df_pair$type2[k] == 4){transition[3,4] = transition[3,4] + 1}
        }
        if (df_pair$type2[k-1] == 4){
          if (df_pair$type2[k] == 1){transition[4,1] = transition[4,1] + 1}
          if (df_pair$type2[k] == 2){transition[4,2] = transition[4,2] + 1}
          if (df_pair$type2[k] == 3){transition[4,3] = transition[4,3] + 1}
          if (df_pair$type2[k] == 4){transition[4,4] = transition[4,4] + 1}
        }
      }
    }
    
    # calculation transition probability matrix
    transition_prob = transition
    for (m in 1:4){
      transition_prob[m,1] = round(transition[m,1] / sum(transition[m,1:4]), 2)
      transition_prob[m,2] = round(transition[m,2] / sum(transition[m,1:4]), 2)
      transition_prob[m,3] = round(transition[m,3] / sum(transition[m,1:4]), 2)
      transition_prob[m,4] = round(transition[m,4] / sum(transition[m,1:4]), 2)
      transition_prob[m,5] = sum(transition[m,1:4])
    }
    
    # update matrix in the list
    transition_matrix[[i]] = transition_prob
  }
  
  # transitions for MV
  else{
    
    # generate the empty matrix
    transition = matrix(0, nrow = 3, ncol = 5)
    rownames(transition) = c('diagonal at t', 'p1adv at t', 'p2adv at t')
    colnames(transition) = c('stay at t+1', 'diagonal at t+1', 'p1adv at t+1', 'p2adv at t+1', 'obs')
    
    # loop over pairs
    for (j in 1:length(pairs)){
      
      df_pair = filter(df, session_round_pair_id == pairs[j])
      # loop over observations
      for (k in 2:length(df_pair$tick)){
        
        # transitions from diagonal
        if (df_pair$type2[k-1] <= 3){
          if (df_pair$type2[k] == df_pair$type2[k-1]){transition[1,1] = transition[1,1] + 1}
          else if (df_pair$type2[k] <= 3){transition[1,2] = transition[1,2] + 1}
          else if (df_pair$type2[k] >= 7){transition[1,4] = transition[1,4] + 1}
          else{transition[1,3] = transition[1,3] + 1}
        }
        # transitions from p2adv
        else if (df_pair$type2[k-1] >= 7){
          if (df_pair$type2[k] == df_pair$type2[k-1]){transition[3,1] = transition[3,1] + 1}
          else if (df_pair$type2[k] <= 3){transition[3,2] = transition[3,2] + 1}
          else if (df_pair$type2[k] >= 7){transition[3,4] = transition[3,4] + 1}
          else{transition[3,3] = transition[3,3] + 1}
        }
        # transitions from p1 adv
        else{
          if (df_pair$type2[k] == df_pair$type2[k-1]){transition[2,1] = transition[2,1] + 1}
          else if (df_pair$type2[k] <= 3){transition[2,2] = transition[2,2] + 1}
          else if (df_pair$type2[k] >= 7){transition[2,4] = transition[2,4] + 1}
          else{transition[2,3] = transition[2,3] + 1}
        }
      }
    }
    
    # calculation transition probability matrix
    transition_prob = transition
    for (m in 1:3){
      transition_prob[m,1] = round(transition[m,1] / sum(transition[m,1:4]), 2)
      transition_prob[m,2] = round(transition[m,2] / sum(transition[m,1:4]), 2)
      transition_prob[m,3] = round(transition[m,3] / sum(transition[m,1:4]), 2)
      transition_prob[m,4] = round(transition[m,4] / sum(transition[m,1:4]), 2)
      transition_prob[m,5] = sum(transition[m,1:4])
    }
    
    # update matrix in the list
    transition_matrix[[i]] = transition_prob
  }
}

# data export
xtable(transition_matrix[[1]], digits = 2, caption = uniquetreatment[1])
xtable(transition_matrix[[2]], digits = 2, caption = uniquetreatment[2])
xtable(transition_matrix[[3]], digits = 2, caption = uniquetreatment[3])
xtable(transition_matrix[[4]], digits = 2, caption = uniquetreatment[4])
xtable(transition_matrix[[5]], digits = 2, caption = uniquetreatment[5])
xtable(transition_matrix[[6]], digits = 2, caption = uniquetreatment[6])
xtable(transition_matrix[[7]], digits = 2, caption = uniquetreatment[7])
xtable(transition_matrix[[8]], digits = 2, caption = uniquetreatment[8])


##### Stay but positive regret & Switch to negative regret (not used) #####
full_data = full_data %>% mutate(p1_current_regret = p1_strategy_0*p1_strategy_0_regret + p1_strategy_1*p1_strategy_1_regret + p1_strategy_2*p1_strategy_2_regret)
full_data = full_data %>% mutate(p2_current_regret = p2_strategy_0*p2_strategy_0_regret + p2_strategy_1*p2_strategy_1_regret + p2_strategy_2*p2_strategy_2_regret)
full_data = full_data %>% mutate(
  p1_current_regret_frac = ifelse(game=='BM', p1_current_regret/600, p1_current_regret/200),
  p1_current_regret_0 = p1_strategy_0_regret - p1_current_regret,
  p1_current_regret_1 = p1_strategy_1_regret - p1_current_regret,
  p1_current_regret_2 = p1_strategy_2_regret - p1_current_regret,
  p1_regret_max = max(p1_current_regret_0, p1_current_regret_1, p1_current_regret_2),
  p1_regret_max_frac = ifelse(game=='BM', p1_regret_max/600, p1_regret_max/200),
  p2_current_regret_frac = ifelse(game=='BM', p2_current_regret/600, p2_current_regret/200),
  p2_current_regret_0 = p2_strategy_0_regret - p2_current_regret,
  p2_current_regret_1 = p2_strategy_1_regret - p2_current_regret,
  p2_current_regret_2 = p2_strategy_2_regret - p2_current_regret,
  p2_regret_max = max(p2_current_regret_0, p2_current_regret_1, p2_current_regret_2),
  p2_regret_max_frac = ifelse(game=='BM', p2_regret_max/600, p2_regret_max/200),
)

# Stay but positive regret
stay_p1 = filter(full_data, p1_switch == 0 & p1_regret_max > 0)
stay_p2 = filter(full_data, p2_switch == 0 & p2_regret_max > 0)

title = 'density of current choice avgpay when stay but positive regret'
file = paste("D:/Dropbox/Working Papers/Correlated Equilibrium/writeup/figs/", title, sep = "")
file = paste(file, ".png", sep = "")
png(file, width = 1200, height = 500)
par(mfrow=c(1,2))

plot(density(stay_p1$p1_current_regret_frac), main = 'player 1: stay but exist positive regret')
plot(density(stay_p2$p2_current_regret_frac), main = 'player 2: stay but exist positive regret')

dev.off()

# # Switch to negative regret
# switch_p1 = filter(full_data, p1_switch == 1 & p1_regret_max == 0)
# switch_p2 = filter(full_data, p2_switch == 1 & p2_regret_max == 0)
# 
# title = 'density of current choice avgpay when switch but with negative regret'
# file = paste("D:/Dropbox/Working Papers/Correlated Equilibrium/writeup/figs/", title, sep = "")
# file = paste(file, ".png", sep = "")
# png(file, width = 1200, height = 500)
# par(mfrow=c(1,2))
# 
# plot(density(switch_p1$p1_current_regret_frac), main = 'player 1: switch but with negative regret')
# plot(density(switch_p2$p2_current_regret_frac), main = 'player 2: switch but with negative regret')
# 
# dev.off()
