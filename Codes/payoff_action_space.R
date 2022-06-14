##### Data preparation #####
# load packages
rm(list = ls())
library(here)
library(ggplot2)
library(dplyr)
library(xtable)
library(haven)

full_data = read.csv(here('Data','data_all.csv'))
full_data = filter(full_data, game == 'MV' & period > 20)

full_data = full_data %>% mutate(
  is_11 = p1_strategy_0 * p2_strategy_0, 
  is_12 = p1_strategy_0 * p2_strategy_1,
  is_13 = p1_strategy_0 * p2_strategy_2, 
  is_21 = p1_strategy_1 * p2_strategy_0, 
  is_22 = p1_strategy_1 * p2_strategy_1, 
  is_23 = p1_strategy_1 * p2_strategy_2, 
  is_31 = p1_strategy_2 * p2_strategy_0, 
  is_32 = p1_strategy_2 * p2_strategy_1,
  is_33 = p1_strategy_2 * p2_strategy_2,
  is_all = is_11+is_12+is_13+is_21+is_22+is_23+is_31+is_32+is_33)

# generate data at pair level
uniquepairs = unique(full_data$session_round_pair_id)
uniquetreatments = unique(full_data$treatment)
len = length(uniquepairs)
df = data.frame(
  pair_id = rep(NA,len), information = rep(NA,len), 
  regret_info = rep(NA,len), treatment = rep(NA,len),
  is_11 = rep(NA,len), is_12 = rep(NA,len), is_13 = rep(NA,len), 
  is_21 = rep(NA,len), is_22 = rep(NA,len), is_23 = rep(NA,len),
  is_31 = rep(NA,len), is_32 = rep(NA,len), is_33 = rep(NA,len), 
  p1_payoff = rep(NA,len), p2_payoff = rep(NA,len))

for (i in 1:length(uniquepairs)){
  df_temp = filter(full_data, session_round_pair_id == uniquepairs[i])
  df$pair_id[i] = df_temp$session_round_pair_id[1]
  df$information[i] = df_temp$information[1]
  df$regret_info[i] = df_temp$regret_info[1]
  df$treatment[i] = df_temp$treatment[1]
  df$is_11[i] = mean(df_temp$is_11)
  df$is_12[i] = mean(df_temp$is_12)
  df$is_13[i] = mean(df_temp$is_13)
  df$is_21[i] = mean(df_temp$is_21)
  df$is_22[i] = mean(df_temp$is_22)
  df$is_23[i] = mean(df_temp$is_23)
  df$is_31[i] = mean(df_temp$is_31)
  df$is_32[i] = mean(df_temp$is_32)
  df$is_33[i] = mean(df_temp$is_33)
  df$p1_payoff[i] = mean(df_temp$p1_payoff)
  df$p2_payoff[i] = mean(df_temp$p2_payoff)
}

rm(df_temp)

# generate fraction table
table = matrix(0, nrow = 4, ncol = 4)
rownames(table) = uniquetreatments
colnames(table) = c('payoff space', 'action space', 
                    'action space row', 'action space col')


##### pairs in payoff space #####
# calculate the fraction in CE
df$in_ce = 0
df = df %>% mutate(
  in_ce = ifelse(p1_payoff>100 & abs((p2_payoff-100)/(p1_payoff-100))>=1/2 & abs((p2_payoff-100)/(p1_payoff-100))<=2,1,in_ce))
df = df %>% mutate(
  in_ce = ifelse(p1_payoff==100 & p2_payoff==100,1,in_ce))

# calculate the distance
df = df %>%
  mutate(d_mne = sqrt((p1_payoff-100)^2+(p2_payoff-100)^2),
         d_tce = sqrt((p1_payoff-150)^2+(p2_payoff-150)^2))

# loop over treatments to fill out the table
for (i in 1:length(uniquetreatments)){
  df_temp = filter(df, treatment == uniquetreatments[i])
  table[i,1] = mean(df_temp$in_ce)
}

rm(df_temp)


##### pairs in action space #####
# check whether a pair is in the CE action space
df = df %>% mutate(
  eq8 = is_11*(2-0) + is_12*(0-1) + is_13*(1-2),
  eq9 = is_11*(1-0) + is_12*(2-1) + is_13*(0-2),
  eq10 = is_21*(0-2) + is_22*(1-0) + is_23*(2-1),
  eq11 = is_21*(1-2) + is_22*(2-0) + is_23*(0-1),
  eq12 = is_31*(0-1) + is_32*(1-2) + is_33*(2-0),
  eq13 = is_31*(2-1) + is_32*(0-2) + is_33*(1-0),
  eq14 = is_11*(2-0) + is_21*(0-1) + is_31*(1-2),
  eq15 = is_11*(1-0) + is_21*(2-1) + is_31*(0-2),
  eq16 = is_12*(0-2) + is_22*(1-0) + is_32*(2-1),
  eq17 = is_12*(1-2) + is_22*(2-0) + is_32*(0-1),
  eq18 = is_13*(0-1) + is_23*(1-2) + is_33*(2-0),
  eq19 = is_13*(2-1) + is_23*(0-2) + is_33*(1-0),
  #in_ce_row = ifelse(eq8<=0&eq9<=0&eq10<=0&eq11<=0&eq12<=0&eq13<=0, 1, 0),
  #in_ce_col = ifelse(eq14<=0&eq15<=0&eq16<=0&eq17<=0&eq18<=0&eq19<=0, 1, 0),
  #in_ce_row = ifelse(eq8<=0.1&eq9<=0.1&eq10<=0.1&eq11<=0.1&eq12<=0.1&eq13<=0.1, 1, 0),
  #in_ce_col = ifelse(eq14<=0.1&eq15<=0.1&eq16<=0.1&eq17<=0.1&eq18<=0.1&eq19<=0.1, 1, 0),
  in_ce_row = ifelse(eq8<=0.2&eq9<=0.2&eq10<=0.2&eq11<=0.2&eq12<=0.2&eq13<=0.2, 1, 0),
  in_ce_col = ifelse(eq14<=0.2&eq15<=0.2&eq16<=0.2&eq17<=0.2&eq18<=0.2&eq19<=0.2, 1, 0),
  in_ce_all = ifelse(in_ce_row==1&in_ce_col==1, 1, 0)
)

# loop over treatments to fill out the table
for (i in 1:length(uniquetreatments)){
  df_temp = filter(df, treatment == uniquetreatments[i])
  table[i,2] = mean(df_temp$in_ce_all)
  table[i,3] = mean(df_temp$in_ce_row)
  table[i,4] = mean(df_temp$in_ce_col)
}

xtable(table, digits = 2)