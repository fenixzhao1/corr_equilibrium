##### Data preparation #####
# load packages
rm(list = ls())
library(here)
library(ggplot2)
library(dplyr)
library(xtable)
library(haven)

load('~/Desktop/jotarepos/correq/corr_equilibrium/Data/data_all.Rda')

df <- filter(full_data, period > 20)

uniquepairs = unique(df$session_round_pair_id)

df_co <- df %>%
  group_by(session_round_pair_id, treatment)  %>%
  summarize(p1_strategy_0 =  mean(p1_strategy_0),
            p1_strategy_1 =  mean(p1_strategy_1),
            p1_strategy_2 =  mean(p1_strategy_2),
            p2_strategy_0 =  mean(p2_strategy_0),
            p2_strategy_1 =  mean(p2_strategy_1),
            p2_strategy_2 =  mean(p2_strategy_2),
            p1_payoff  = mean(p1_payoff),
            p2_payoff  = mean(p2_payoff))

df_bm_l_c <- filter(df_co, treatment == 'BM_L_C')

ne_pure1<-c(2,6)
ne_pure2<-c(6,2)
best_ce<-c(13/3,13/3)
worst_ce<-c(3,3)
puntos_bm<-as.data.frame(rbind(worst_ce,ne_pure1,best_ce,ne_pure2,worst_ce)*100)

ggplot(data=df_bm_l_c ,aes(x=p1_payoff,y=p2_payoff)) + geom_point() + geom_count()  +
  geom_path(data=puntos_bm,aes(x=V1,y=V2)) +
  scale_x_continuous(name='Payoff P1', waiver(), limits=c(100,600)) +
  scale_y_continuous(name='Payoff P2', waiver(), limits=c(100,600)) 

df_bm_l_a<- filter(df_co, treatment == 'BM_L_A')
ggplot(data=df_bm_l_a ,aes(x=p1_payoff,y=p2_payoff)) + geom_point() + geom_count()  +
  geom_path(data=puntos_bm,aes(x=V1,y=V2)) +
  scale_x_continuous(name='Payoff P1', waiver(), limits=c(100,600)) +
  scale_y_continuous(name='Payoff P2', waiver(), limits=c(100,600)) 

df_bm_h_a<- filter(df_co, treatment == 'BM_H_A')

ggplot(df_bm_h_a ,aes(x=p1_payoff,y=p2_payoff)) + geom_point() + geom_count()  +
  geom_path(data=puntos_bm,aes(x=V1,y=V2)) +
  scale_x_continuous(name='Payoff P1', waiver(), limits=c(100,600)) +
  scale_y_continuous(name='Payoff P2', waiver(), limits=c(100,600)) 

df_bm_h_c<- filter(df_co, treatment == 'BM_H_C')
ggplot(df_bm_h_c ,aes(x=p1_payoff,y=p2_payoff)) + geom_point() + geom_count()  +
  geom_path(data=puntos_bm,aes(x=V1,y=V2)) +
  scale_x_continuous(name='Payoff P1', waiver(), limits=c(100,600)) +
  scale_y_continuous(name='Payoff P2', waiver(), limits=c(100,600)) 

