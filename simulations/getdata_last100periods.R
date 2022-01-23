##### Load packages #####
# add package
rm(list = ls())
library(ggplot2)
library(xtable)
library(dplyr)
library(here)

##### generate dataset for ch games #####
# import CH action dataset
dfch<-read.csv(here("Data/sim_ch.csv"), header=T, stringsAsFactors = FALSE)

# only keep last 100 periods
dfch=filter(dfch, period>400)

# generate unique treatment_simid
dfch$treatment_sim = paste(dfch$regret, dfch$response, dfch$sim, sep = '_')
uniqueid = unique(dfch$treatment_sim)

# build dataset
length = rep(0, length(uniqueid))
df_jd = data.frame(sim = length, jd_11 = length, jd_12 = length, jd_21 = length, jd_22 = length, 
                   p1_payoff = length, p2_payoff = length, regret = length, response = length, 
                   mu = length, beta = length, Delta = length)

# loop over id to fill in the dataset
for (i in 1:length(uniqueid)){
  df = filter(dfch, treatment_sim == uniqueid[i])
  df_jd$sim[i] = df$sim[1]
  df_jd$jd_11[i] = mean(df$is_11)
  df_jd$jd_12[i] = mean(df$is_12)
  df_jd$jd_21[i] = mean(df$is_21)
  df_jd$jd_22[i] = mean(df$is_22)
  df_jd$p1_payoff[i] = mean(df$p1_payoff)
  df_jd$p2_payoff[i] = mean(df$p2_payoff)
  df_jd$regret[i] = df$regret[1]
  df_jd$response[i] = df$response[1]
  df_jd$mu[i] = df$mu[1]
  df_jd$beta[i] = df$beta[1]
  df_jd$Delta[i] = df$Delta[1]
}

# calculate the fraction in CE
df_jd$in_ce <- 0
df_jd <- df_jd %>%
  mutate(in_ce = ifelse(p1_payoff/p2_payoff>=1 & abs((p2_payoff-200)/(p1_payoff-600))>=1/3 & abs((p2_payoff-200)/(p1_payoff-600))<=7/5,1,in_ce), 
         in_ce = ifelse(p1_payoff/p2_payoff<1 & abs((p2_payoff-600)/(p1_payoff-200))>=5/7 & abs((p2_payoff-600)/(p1_payoff-200))<=3,1,in_ce))
df_jd <- df_jd %>%
  mutate(in_ce = ifelse(p1_payoff/p2_payoff>1 & p1_payoff==600,1,in_ce), 
         in_ce = ifelse(p1_payoff/p2_payoff<1 & p1_payoff==200,1,in_ce))

# calculate the distance
df_jd = df_jd %>%
  mutate(d_mne = sqrt((p1_payoff-350)^2+(p2_payoff-350)^2),
         d_tce = sqrt((p1_payoff-1300/3)^2+(p2_payoff-1300/3)^2))

# export data
write.csv(df_jd, here("Data", "sim_pair100_ch.csv"))


##### generate dataset for mv games #####
# import MV action dataset
dfmv<-read.csv(here("Data/sim_mv.csv"), header=T, stringsAsFactors = FALSE)

# only keep last 100 periods
dfmv=filter(dfmv, period>400)

# generate unique treatment_simid
dfmv$treatment_sim = paste(dfmv$regret, dfmv$response, dfmv$sim, sep = '_')
uniqueid = unique(dfmv$treatment_sim)

# build dataset
length = rep(0, length(uniqueid))
df_jd = data.frame(sim = length, jd_11 = length, jd_12 = length, jd_13 = length, jd_21 = length,
                   jd_22 = length, jd_23 = length, jd_31 = length, jd_32 = length, jd_33 = length,
                   p1_payoff = length, p2_payoff = length, regret = length, response = length, 
                   mu = length, beta = length, Delta = length)

# loop over id to fill in the dataset
for (i in 1:length(uniqueid)){
  df = filter(dfmv, treatment_sim == uniqueid[i])
  df_jd$sim[i] = df$sim[1]
  df_jd$jd_11[i] = mean(df$is_11)
  df_jd$jd_12[i] = mean(df$is_12)
  df_jd$jd_13[i] = mean(df$is_13)
  df_jd$jd_21[i] = mean(df$is_21)
  df_jd$jd_22[i] = mean(df$is_22)
  df_jd$jd_23[i] = mean(df$is_23)
  df_jd$jd_31[i] = mean(df$is_31)
  df_jd$jd_32[i] = mean(df$is_32)
  df_jd$jd_33[i] = mean(df$is_33)
  df_jd$p1_payoff[i] = mean(df$p1_payoff)
  df_jd$p2_payoff[i] = mean(df$p2_payoff)
  df_jd$regret[i] = df$regret[1]
  df_jd$response[i] = df$response[1]
  df_jd$mu[i] = df$mu[1]
  df_jd$beta[i] = df$beta[1]
  df_jd$Delta[i] = df$Delta[1]
}

# calculate the fraction in CE
df_jd$in_ce <- 0
df_jd <- df_jd %>%
  mutate(in_ce = ifelse(p1_payoff>100 & abs((p2_payoff-100)/(p1_payoff-100))>=1/2 & abs((p2_payoff-100)/(p1_payoff-100))<=2,1,in_ce))
df_jd <- df_jd %>%
  mutate(in_ce = ifelse(p1_payoff==100 & p2_payoff==100,1,in_ce))

# calculate the distance
df_jd = df_jd %>%
  mutate(d_mne = sqrt((p1_payoff-100)^2+(p2_payoff-100)^2),
         d_tce = sqrt((p1_payoff-150)^2+(p2_payoff-150)^2))

# export data
write.csv(df_jd, here("Data", "sim_pair100_mv.csv"))
