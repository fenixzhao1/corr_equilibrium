##### Data preparation #####
# load packages
#rm(list = ls())
library(here)
library(ggplot2)
library(dplyr)
library(xtable)
library(haven)

load(here('Data','data_all.Rda'))

df <- filter(full_data, period > 20)


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

ne_pure1<-c(2,6)
ne_pure2<-c(6,2)
best_ce<-c(13/3,13/3)
worst_ce<-c(3,3)
puntos_bm<-as.data.frame(rbind(worst_ce,ne_pure1,best_ce,ne_pure2,worst_ce)*100)

ne_mv<-c(1,1)
ce_mv_s<-c(3/2,3/2)
ce_mv_1<-c(5/3,4/3)
puntos_mv<-as.data.frame(rbind(ne_mv,rev(ce_mv_1),ce_mv_1,ne_mv)*100)

label_tre = unique(df_co$treatment)

for(tre in label_tre){
  data_tre <-filter(df_co, treatment == tre)
  pepito<- paste('pi_space_',tre,'.png',sep='')
  
  if(grepl("BM",tre, fixed = TRUE)==TRUE){
    ggplot(data=data_tre ,aes(x=p1_payoff,y=p2_payoff)) + geom_point() + geom_count()  +
      geom_path(data=puntos_bm,aes(x=V1,y=V2)) +
      scale_x_continuous(name='Payoff P1', waiver(), limits=c(100,600)) +
      scale_y_continuous(name='Payoff P2', waiver(), limits=c(100,600))
  }else{
    ggplot(data=data_tre ,aes(x=p1_payoff,y=p2_payoff)) + geom_point() + geom_count()  +
      geom_path(data=puntos_mv,aes(x=V1,y=V2)) +
      scale_x_continuous(name='Payoff P1', waiver(), limits=c(0,200)) +
      scale_y_continuous(name='Payoff P2', waiver(), limits=c(0,200))
    
    }
  
  ggsave(here("Figures", pepito))
  }






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

