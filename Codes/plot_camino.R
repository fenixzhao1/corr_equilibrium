##### Data preparation #####
# load packages
#rm(list = ls())
library(here)
library(ggplot2)
library(dplyr)
library(haven)


full_data = read.csv(here('Data','data_all.csv'))

df <- filter(full_data, period > 20)

df$clock<-1

df= df %>% group_by(session_round_pair_id) %>%
  mutate(tiempo= cumsum(clock))

df= df %>% group_by(session_round_pair_id) %>%
  mutate(p1_payoff_average= cumsum(p1_payoff)/tiempo, 
         p2_payoff_average= cumsum(p2_payoff)/tiempo, 
         )

data_chlc <-filter(df, treatment == 'BM_L_C' & period>30)


data_chlc_plot_1 <-filter(data_chlc, session_round_pair_id=="m1ix0n73_4_88vw15hz_bh2bb6il")
to_draw<-sample(unique(data_chlc$session_round_pair_id),2)
print(to_draw)
data_chlc_plot_2 <-filter(data_chlc, session_round_pair_id=="07gjoche_6_owiawee6_1q2imaam")
data_chlc_plot_3 <-filter(data_chlc, session_round_pair_id=="dlpeimt2_3_0yi5bg9p_syt1sikr")
data_chlc_plot_4 <-filter(data_chlc, session_round_pair_id==to_draw[1])

puntos_bm<-as.data.frame(rbind(worst_ce,ne_pure1,best_ce,ne_pure2,worst_ce)*100)
puntos_bm_matrix<-as.data.frame(rbind(c(1,1),ne_pure1,c(5,5),ne_pure2,c(1,1))*100)

x<-c(2,3,13/3,6)
y<-c(12-3*x[1:2],4-(1/3)*x[3:4])
y_u<-c(5/7*2+6-5/7*x[1:2],7/5*13/3+13/3-7/5*x[3:4])

puntos_shade<-as.data.frame(cbind(x,y,y_u)*100)
names(puntos_shade)<-c("V1","V2","V3")

ggplot(data=data_chlc_plot_1, aes(x=p1_payoff_average,y=p2_payoff_average))  +
  geom_segment(aes(xend = lead(p1_payoff_average), yend = lead(p2_payoff_average)),
               arrow = arrow(length = unit(0.2,"cm"))) +
  geom_segment(aes(x=p1_payoff_average,y=p2_payoff_average,xend = lead(p1_payoff_average), yend = lead(p2_payoff_average)),
               arrow = arrow(length = unit(0.2,"cm")),colour="red",data=data_chlc_plot_2) +
  geom_segment(aes(x=p1_payoff_average,y=p2_payoff_average,xend = lead(p1_payoff_average), yend = lead(p2_payoff_average)),
               arrow = arrow(length = unit(0.2,"cm")),colour="green",data=data_chlc_plot_3) +
  geom_segment(aes(x=p1_payoff_average,y=p2_payoff_average,xend = lead(p1_payoff_average), yend = lead(p2_payoff_average)),
               arrow = arrow(length = unit(0.2,"cm")),colour="blue",data=data_chlc_plot_4) +
  geom_path(data=puntos_bm_matrix,aes(x=V1,y=V2),linetype = "dashed") +
  geom_path(data=puntos_bm,aes(x=V1,y=V2)) +
  geom_ribbon(data=puntos_shade, aes(x=V1,y=V2,ymin=V2,ymax=V3), fill="blue", alpha=0.5) +
  scale_x_continuous(name='Payoff P1', waiver(), limits=c(100,600)) +
  scale_y_continuous(name='Payoff P2', waiver(), limits=c(100,600)) +
  annotate(geom="text", x=180, y=600, label="NE") +
  annotate(geom="text", x=600, y=190, label="NE") +
  annotate(geom="text", x=360, y=360, label="MNE") +
  annotate("point", x = 350, y = 350, colour = "blue") +
  annotate(geom="text", x=454, y=440, label="Target CE") +
  annotate("point", x = 433, y = 433, colour = "blue") 


ne_mv<-c(1,1)
ce_mv_s<-c(3/2,3/2)
ce_mv_1<-c(5/3,4/3)

x_mv<-c(1,1.33,1.6667)
y_mv<-1/2*x_mv+1/2
y_u_mv<-c(2*x_mv-1)
y_u_mv_2<-3-x_mv+0.0005
puntos_shade_mv<-as.data.frame(cbind(x_mv,y_mv,y_u_mv,y_u_mv_2)*100)
names(puntos_shade_mv)<-c("V1","V2","V3","V4")

puntos_mv<-as.data.frame(rbind(ne_mv,rev(ce_mv_1),ce_mv_1,ne_mv)*100)
puntos_mv_matrix<-as.data.frame(rbind(c(0,0),c(1,2),c(2,1),c(0,0))*100)


data_mvlc <-filter(df, treatment == 'MV_L_C' & period>30)
to_draw<-sample(unique(data_mvlc$session_round_pair_id),4)
print(to_draw)
data_mvlc_plot_1 <-filter(data_mvlc, session_round_pair_id=="wawy2548_3_tck3cu64_ydmqa6m1" )
data_mvlc_plot_2 <-filter(data_mvlc, session_round_pair_id=="wawy2548_6_s9547ukt_7oqu83nx")
data_mvlc_plot_3 <-filter(data_mvlc, session_round_pair_id==to_draw[3])
data_mvlc_plot_4 <-filter(data_mvlc, session_round_pair_id=="zt8he0n9_8_b51rzlfa_cvq27yc8")

ggplot(data=data_mvlc_plot_1  ,aes(x=p1_payoff_average,y=p2_payoff_average)) + 
  geom_segment(aes(xend = lead(p1_payoff_average), yend = lead(p2_payoff_average)),
               arrow = arrow(length = unit(0.2,"cm"))) +
  geom_segment(aes(x=p1_payoff_average,y=p2_payoff_average,xend = lead(p1_payoff_average), yend = lead(p2_payoff_average)),
               arrow = arrow(length = unit(0.2,"cm")),colour="red",data=data_mvlc_plot_2) +
  geom_segment(aes(x=p1_payoff_average,y=p2_payoff_average,xend = lead(p1_payoff_average), yend = lead(p2_payoff_average)),
               arrow = arrow(length = unit(0.2,"cm")),colour="green",data=data_mvlc_plot_3) +
  geom_segment(aes(x=p1_payoff_average,y=p2_payoff_average,xend = lead(p1_payoff_average), yend = lead(p2_payoff_average)),
               arrow = arrow(length = unit(0.2,"cm")),colour="blue",data=data_mvlc_plot_4) +
  
  geom_path(data=puntos_mv_matrix,aes(x=V1,y=V2),linetype = "dashed")+
  geom_path(data=puntos_mv,aes(x=V1,y=V2)) +
  geom_ribbon(data=puntos_shade_mv,aes(x=V1,y=V2,ymin=V2,ymax=V3), fill="blue", alpha=0.5) +
  geom_ribbon(data=puntos_shade_mv,aes(x=V1,y=V2,ymin=V2,ymax=V4), fill="blue", alpha=0.5) +
  scale_x_continuous(name='Payoff P1', waiver(), limits=c(0,200)) +
  scale_y_continuous(name='Payoff P2', waiver(), limits=c(0,200)) +
  annotate(geom="text", x=100, y=95, label="MNE") +
  annotate("point", x = 100, y = 100, colour = "blue") +
  annotate(geom="text", x=160, y=155, label="Target CE") +
  annotate("point", x = 150, y = 150, colour = "blue",shape=10) 








