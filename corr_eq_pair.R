##### Data preparation #####
# load packages
#rm(list = ls())
library(ggplot2)
library(dplyr)
library(xtable)
library(haven)
#patho<-"~/Desktop/jotarepos/corr_equilibrium/data/pilot_pair_9_13.csv" #here enter your data path
#figures1<-"~/Desktop/jotarepos/corr_equilibrium/data/pair/"  #here enter your directory to store data.
patho<-"D:/Dropbox/Working Papers/Correlated Equilibrium/data/corr_equilibrium/data/pilot_pair_9_13.csv" #here enter your data path
figures<-"D:/Dropbox/Working Papers/Correlated Equilibrium/corr_equilibrium/figures/"  #here enter your directory to store data. 

# load data 
bimatrix_choice <- read.csv(patho, header = T)
bimatrix_choice<-bimatrix_choice[,c(1:26)]

# create round variable in choice data and create full dataset
bimatrix_choice$round = as.double(substring(bimatrix_choice$subsession_id, 2, 3)) - 50
full_data = bimatrix_choice
full_data = arrange(full_data, full_data$session_code, full_data$subsession_id, full_data$id_in_subsession, full_data$tick)
#full_data = full_data %>% filter(practice=="FALSE") %>% mutate(period = tick + 1)
full_data = full_data %>% mutate(period = tick + 1)

# create round/pair id
full_data$pair_id = paste(full_data$p1_code, full_data$p2_code, sep = "_")
full_data$round_pair_id = paste(full_data$round, full_data$pair_id,  sep = "_")
full_data$session_round_pair_id = paste(full_data$session_code, full_data$round_pair_id, sep = "_")
full_data$session_round_id = paste(full_data$session_code, full_data$round, sep = "_")

# create treatment variable
full_data = full_data %>% mutate(matching = ifelse(mean_matching == TRUE, 'Meanmatch', 'Pairwise'))
full_data = full_data %>% mutate(time = ifelse(num_subperiods == 0, 'Continuous', 'Discrete'))
full_data = full_data %>% mutate(information = ifelse(max_info == TRUE, 'MaxInfo', 'MinInfo'))
full_data$treatment = paste(full_data$game, full_data$information, full_data$matching, full_data$time, sep = '_')

# create mixed strategies
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
         p3_strategy_0_regret= cumsum(coalesce(lag(p2_payoff), 0)*coalesce(lag(p3_strategy_0), 0))/cumsum(coalesce(lag(p3_strategy_0),0)),
         p3_strategy_1_regret= cumsum(coalesce(lag(p2_payoff), 0)*coalesce(lag(p3_strategy_1), 0))/cumsum(coalesce(lag(p3_strategy_1),0)),
         p3_strategy_2_regret= cumsum(coalesce(lag(p2_payoff), 0)*coalesce(lag(p3_strategy_2), 0))/cumsum(coalesce(lag(p3_strategy_2),0)),
         p1_switch = ifelse(abs(p1_strategy-lag(p1_strategy))>0, 1, 0),
         p2_switch = ifelse(abs(p2_strategy-lag(p2_strategy))>0, 1, 0),
         p3_switch = ifelse(abs(p3_strategy-lag(p3_strategy))>0, 1, 0)
         )

full_data$p1_strategy_0_regret[is.na(full_data$p1_strategy_0_regret)]<-0
full_data$p1_strategy_1_regret[is.na(full_data$p1_strategy_1_regret)]<-0
full_data$p1_strategy_2_regret[is.na(full_data$p1_strategy_2_regret)]<-0
full_data$p2_strategy_0_regret[is.na(full_data$p2_strategy_0_regret)]<-0
full_data$p2_strategy_1_regret[is.na(full_data$p2_strategy_1_regret)]<-0
full_data$p2_strategy_2_regret[is.na(full_data$p2_strategy_2_regret)]<-0
full_data$p3_strategy_0_regret[is.na(full_data$p3_strategy_0_regret)]<-0
full_data$p3_strategy_1_regret[is.na(full_data$p3_strategy_1_regret)]<-0
full_data$p3_strategy_2_regret[is.na(full_data$p3_strategy_2_regret)]<-0

# table(full_data$p2_switch,full_data$treatment)
# 
# #how many DD are not colluding? 
# sum(full_data$treatment[full_data$p1_payoff+full_data$p2_payoff==1000]=="BM_MinInfo_Pairwise_Discrete")/sum(full_data$treatment=="BM_MinInfo_Pairwise_Discrete")
# sum(full_data$treatment[full_data$p1_payoff+full_data$p2_payoff==1000 & full_data$p1_regret>0 & full_data$p2_regret>0]=="BM_MinInfo_Pairwise_Discrete")/sum(full_data$treatment=="BM_MinInfo_Pairwise_Discrete")

# update dta file
write_dta(full_data, "D:/Dropbox/Working Papers/Correlated Equilibrium/data/corr_equilibrium/data/pilot_9_13.dta")


##### Pair-level data #####
# pair level dynamics
full_data = full_data %>% mutate(p2_strategy_jitter = p2_strategy + 0.01)
full_data = full_data %>% mutate(p3_strategy_jitter = ifelse(is.na(p3_strategy), NA, p3_strategy + 0.02))
uniquepairs = unique(full_data$session_round_pair_id)

# loop over pairs
for (i in 1:length(uniquepairs)){
  pairdata = subset(full_data, session_round_pair_id == uniquepairs[i])
  
  title = paste(as.character(pairdata$game[1]), as.character(pairdata$information[1]),
                as.character(uniquepairs[i]), sep = '_')
  file = paste(figures1, title, sep = "")
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


##### Aggregate over time #####
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
  title = as.character(df_treatment$treatment[1])
  file = paste("D:/Dropbox/Working Papers/Correlated Equilibrium/data/figures/", title, sep = "")
  file = paste(file, ".png", sep = "")
  png(file, width = 1000, height = 500)
  
  # BM plot
  if (df_treatment$game[1] == 'BM'){
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


##### Regret vs strategy #####
