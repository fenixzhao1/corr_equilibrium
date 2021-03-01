##### Data preparation #####
# load packages
rm(list = ls())
library(here)
library(ggplot2)
library(dplyr)
library(xtable)
library(haven)

load('~/Desktop/jotarepos/correq/corr_equilibrium/Data/data_all.Rda')

##### Joint density #####
uniquetreatment = unique(full_data$treatment)
density_matrix = list()
total_obs = list()

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
    total_obs[[i]] = length(df_treatment$tick)
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
    total_obs[[i]] = length(df_treatment$tick)
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

de<-as.data.frame(cbind(c(density_matrix[[4]])*total_obs[[4]],c(density_matrix[[5]])*total_obs[[5]]))
chisq.test(de)

de<-as.data.frame(cbind(c(density_matrix[[6]])*total_obs[[6]],c(density_matrix[[5]])*total_obs[[5]]))
chisq.test(de)

de<-as.data.frame(cbind(c(density_matrix[[7]])*total_obs[[7]],c(density_matrix[[3]])*total_obs[[3]]))
chisq.test(de)

