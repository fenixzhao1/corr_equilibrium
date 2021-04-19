##### Data preparation #####
# load packages
#rm(list = ls())
library(here)
library(ggplot2)
library(dplyr)
library(haven)

full_data = read.csv(here('Data','data_all.csv'))

plot_no_data <- FALSE #save space without data points

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
puntos_bm_matrix<-as.data.frame(rbind(c(1,1),ne_pure1,c(5,5),ne_pure2,c(1,1))*100)

x<-c(2,3,13/3,6)
y<-c(12-3*x[1:2],4-(1/3)*x[3:4])
y_u<-c(5/7*2+6-5/7*x[1:2],7/5*13/3+13/3-7/5*x[3:4])

puntos_shade<-as.data.frame(cbind(x,y,y_u)*100)
names(puntos_shade)<-c("V1","V2","V3")
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

label_tre = unique(df_co$treatment)

if(plot_no_data==TRUE){
  ggplot(data=puntos_bm_matrix,aes(x=V1,y=V2)) + geom_path(linetype = "dashed") +
    geom_path(data=puntos_bm,aes(x=V1,y=V2)) +
    geom_ribbon(data=subset(puntos_shade, 200 <= V1 & V1 <= 600), 
                aes(ymin=V2,ymax=V3), fill="blue", alpha=0.5) +
    scale_x_continuous(name='Payoff P1', waiver(), limits=c(100,600)) +
    scale_y_continuous(name='Payoff P2', waiver(), limits=c(100,600)) +
    annotate(geom="text", x=190, y=600, label="NE") +
    annotate(geom="text", x=600, y=190, label="NE") +
    annotate(geom="text", x=360, y=360, label="MNE") +
    annotate("point", x = 350, y = 350, colour = "blue") +
    annotate(geom="text", x=454, y=440, label="Target CE") +
    annotate("point", x = 433, y = 433, colour = "blue") 
  ggsave(here("Figures", 'piCH.png'))
  
  ggplot(data=puntos_mv_matrix,aes(x=V1,y=V2)) + geom_path(linetype = "dashed") +
    geom_path(data=puntos_mv,aes(x=V1,y=V2)) +
    geom_ribbon(data=subset(puntos_shade_mv, 100 <= V1 & V1 <= 134), 
                aes(ymin=V2,ymax=V3), fill="blue", alpha=0.5) +
    geom_ribbon(data=subset(puntos_shade_mv, 130.00 <= V1 & V1 <= 167), 
                aes(ymin=V2,ymax=V4), fill="blue", alpha=0.5) +
    scale_x_continuous(name='Payoff P1', waiver(), limits=c(0,200)) +
    scale_y_continuous(name='Payoff P2', waiver(), limits=c(0,200)) +
    annotate(geom="text", x=100, y=95, label="MNE") +
    annotate("point", x = 100, y = 100, colour = "blue") +
    annotate(geom="text", x=160, y=155, label="Target CE") +
    annotate("point", x = 150, y = 150, colour = "blue",shape=10) 
  ggsave(here("Figures", 'piMV.png'))
}

for(tre in label_tre){
  data_tre <-filter(df_co, treatment == tre)
  pepito<- paste('pi_space_',tre,'.png',sep='')
  
  if(grepl("BM",tre, fixed = TRUE)==TRUE){
    ggplot(data=data_tre,aes(x=p1_payoff,y=p2_payoff)) + geom_point() +  geom_count() +
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
  }else{
    ggplot(data=data_tre ,aes(x=p1_payoff,y=p2_payoff)) + geom_point() + geom_count() +
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
    }
  
  ggsave(here("Figures", pepito))
  }



