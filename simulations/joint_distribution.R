##### Data preparation #####
# add package
rm(list = ls())
library(ggplot2)
library(xtable)
library(dplyr)
library(here)

# import pair dataset
dfch<-read.csv(here("Data/sim_pair_ch.csv"), header=T, stringsAsFactors = FALSE)
dfch$treatment = paste(dfch$regret, dfch$response, sep = ' ')

dfmv<-read.csv(here("Data/sim_pair_mv.csv"), header=T, stringsAsFactors = FALSE)
dfmv$treatment = paste(dfmv$regret, dfmv$response, sep = ' ')

# set up treatment list
uniquetreatment = unique(dfch$treatment)


##### CH game joint distribution #####
# set up data container
jd = list()

# loop over treatments
for (i in 1:length(uniquetreatment)){
  
  df = filter(dfch, treatment == uniquetreatment[i])
  jd[[i]] = matrix(c(0,0,0,0),2,2)
  
  jd[[i]][1,1] = mean(df$jd_11)
  jd[[i]][1,2] = mean(df$jd_12)
  jd[[i]][2,1] = mean(df$jd_21)
  jd[[i]][2,2] = mean(df$jd_22)
  
  # table output
  print(xtable(jd[[i]], digits = 3, caption = uniquetreatment[i]))
}

rm(df, jd)


##### MV game joint distribution #####
# set up data container
jd = list()

# loop over treatments
for (i in 1:length(uniquetreatment)){
  
  df = filter(dfmv, treatment == uniquetreatment[i])
  jd[[i]] = matrix(c(0,0,0,0,0,0,0,0,0),3,3)
  
  jd[[i]][1,1] = mean(df$jd_11)
  jd[[i]][1,2] = mean(df$jd_12)
  jd[[i]][1,3] = mean(df$jd_13)
  jd[[i]][2,1] = mean(df$jd_21)
  jd[[i]][2,2] = mean(df$jd_22)
  jd[[i]][2,3] = mean(df$jd_23)
  jd[[i]][3,1] = mean(df$jd_31)
  jd[[i]][3,2] = mean(df$jd_32)
  jd[[i]][3,3] = mean(df$jd_33)
  
  # table output
  print(xtable(jd[[i]], digits = 3, caption = uniquetreatment[i]))
}

rm(df, jd)
