##### Data preparation #####
# load packages
#rm(list = ls())
library(here)
library(ggplot2)
library(dplyr)
library(haven)
library(xtable)
full_data = read.csv(here('Data','data_all.csv'))

#full_data <- filter(full_data, period > 20)

#Create all possible cells
full_data$bm1<-0
full_data$bm2<-0
full_data$bm3<-0
full_data$bm4<-0
  
full_data$mv1<-0
full_data$mv2<-0
full_data$mv3<-0
full_data$mv4<-0
full_data$mv5<-0
full_data$mv6<-0
full_data$mv7<-0
full_data$mv8<-0
full_data$mv9<-0

full_data$numero<-1

full_data <- full_data %>% 
  mutate(bm1=ifelse(game=='BM' & p1_strategy==0 & p2_strategy==0,1,bm1),
         bm2=ifelse(game=='BM' & p1_strategy==1 & p2_strategy==0,1,bm2),
         bm3=ifelse(game=='BM' & p1_strategy==0 & p2_strategy==1,1,bm3),
         bm4=ifelse(game=='BM' & p1_strategy==1 & p2_strategy==1,1,bm4),
         mv1=ifelse(game=='MV' & p1_strategy==0 & p2_strategy==0,1,mv1),
         mv2=ifelse(game=='MV' & p1_strategy==1 & p2_strategy==0,1,mv2),
         mv3=ifelse(game=='MV' & p1_strategy==2 & p2_strategy==0,1,mv3),
         mv4=ifelse(game=='MV' & p1_strategy==0 & p2_strategy==1,1,mv4),
         mv5=ifelse(game=='MV' & p1_strategy==1 & p2_strategy==1,1,mv5),
         mv6=ifelse(game=='MV' & p1_strategy==2 & p2_strategy==1,1,mv6),
         mv7=ifelse(game=='MV' & p1_strategy==0 & p2_strategy==2,1,mv7),
         mv8=ifelse(game=='MV' & p1_strategy==1 & p2_strategy==2,1,mv8),
         mv9=ifelse(game=='MV' & p1_strategy==2 & p2_strategy==2,1,mv9)
         )

df <- full_data %>%
  group_by(session_round_pair_id, treatment, game, information,regret_info)  %>%
  summarize(p1_strategy_0 =  sum(p1_strategy_0),
            p1_strategy_1 =  sum(p1_strategy_1),
            p1_strategy_2 =  sum(p1_strategy_2),
            p2_strategy_0 =  sum(p2_strategy_0),
            p2_strategy_1 =  sum(p2_strategy_1),
            p2_strategy_2 =  sum(p2_strategy_2),
            bm1 = sum(bm1),
            bm2 = sum(bm2),
            bm3 = sum(bm3),
            bm4 = sum(bm4),
            mv1 = sum(mv1),
            mv2 = sum(mv2),
            mv3 = sum(mv3),
            mv4 = sum(mv4),
            mv5 = sum(mv5),
            mv6 = sum(mv6),
            mv7 = sum(mv7),
            mv8 = sum(mv8),
            mv9 = sum(mv9),
            numero = sum(numero)
            )

df- filter(df, numero > 30)

df$q0_1<-NA
df$q0_2<-NA
df$q0_3<-NA
df$q0_4<-NA

df$q0_5<-NA
df$q0_6<-NA
df$q0_7<-NA
df$q0_8<-NA
df$q0_9<-NA


df<- df %>% 
  mutate(q0_1=ifelse(game=='BM',p1_strategy_0*p2_strategy_0/numero^2,q0_1),
         q0_2=ifelse(game=='BM',p1_strategy_1*p2_strategy_0/numero^2,q0_2),
         q0_3=ifelse(game=='BM',p1_strategy_0*p2_strategy_1/numero^2,q0_3),
         q0_4=ifelse(game=='BM',p1_strategy_1*p2_strategy_1/numero^2,q0_4),
         
         q0_1=ifelse(game=='MV',p1_strategy_0*p2_strategy_0/numero^2,q0_1),
         q0_2=ifelse(game=='MV',p1_strategy_1*p2_strategy_0/numero^2,q0_2),
         q0_3=ifelse(game=='MV',p1_strategy_2*p2_strategy_0/numero^2,q0_3),
         q0_4=ifelse(game=='MV',p1_strategy_0*p2_strategy_1/numero^2,q0_4),
         q0_5=ifelse(game=='MV',p1_strategy_1*p2_strategy_1/numero^2,q0_5),
         q0_6=ifelse(game=='MV',p1_strategy_2*p2_strategy_1/numero^2,q0_6),
         q0_7=ifelse(game=='MV',p1_strategy_0*p2_strategy_2/numero^2,q0_7),
         q0_8=ifelse(game=='MV',p1_strategy_1*p2_strategy_2/numero^2,q0_8),
         q0_9=ifelse(game=='MV',p1_strategy_2*p2_strategy_2/numero^2,q0_9)
  )

df$n_l = ifelse(df$q0_1==0,0,log(df$q0_1)*df$bm1) + 
  ifelse(df$q0_2==0,0,log(df$q0_2)*df$bm2) +
  ifelse(df$q0_3==0,0,log(df$q0_3)*df$bm3) +
  ifelse(df$q0_4==0,0,log(df$q0_4)*df$bm4) 
  
df<- df %>% 
  mutate(n_l = ifelse(game=='MV',ifelse(q0_1==0,0,log(q0_1)*mv1)+ 
                        ifelse(q0_2==0,0,log(q0_2)*mv2)+
                        ifelse(q0_3==0,0,log(q0_3)*mv3)+
                        ifelse(q0_4==0,0,log(q0_4)*mv4)+
                        ifelse(q0_5==0,0,log(q0_5)*mv5)+
                        ifelse(q0_6==0,0,log(q0_6)*mv6)+
                        ifelse(q0_7==0,0,log(q0_7)*mv7)+
                        ifelse(q0_8==0,0,log(q0_8)*mv8)+
                        ifelse(q0_9==0,0,log(q0_9)*mv9),n_l)
         )

df$n_al= ifelse(df$bm1==0,0,log(df$bm1/df$numero)*df$bm1) + 
  ifelse(df$bm2==0,0,log(df$bm2/df$numero)*df$bm2) +
  ifelse(df$bm3==0,0,log(df$bm3/df$numero)*df$bm3) +
  ifelse(df$bm4==0,0,log(df$bm4/df$numero)*df$bm4) 

df<- df %>% 
  mutate(n_al = ifelse(game=='MV',ifelse(mv1==0,0,log(mv1/numero)*mv1)+ 
                        ifelse(mv2==0,0,log(mv2/numero)*mv2)+
                        ifelse(mv3==0,0,log(mv3/numero)*mv3)+
                        ifelse(mv4==0,0,log(mv4/numero)*mv4)+
                        ifelse(mv5==0,0,log(mv5/numero)*mv5)+
                        ifelse(mv6==0,0,log(mv6/numero)*mv6)+
                        ifelse(mv7==0,0,log(mv7/numero)*mv7)+
                        ifelse(mv8==0,0,log(mv8/numero)*mv8)+
                        ifelse(mv9==0,0,log(mv9/numero)*mv9),n_al)
  )

df$lr_test<- -2*(df$n_l-df$n_al)

df$rejects<-0
df$rejects[df$game=='BM']<- df$lr_test[df$game=='BM']>3.84
df$rejects[df$game=='MV']<- df$lr_test[df$game=='MV']>9.49

t_all<-table(df$treatment,df$rejects)
t_all<-cbind(t_all,apply(t_all,1,sum))
final<-t(t_all[,2]/t_all[,3])
xtable(final)
  