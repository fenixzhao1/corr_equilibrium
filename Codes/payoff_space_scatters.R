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


##### Distance to CE and CDF #####
ne_pure1<-c(2,6)
ne_pure2<-c(6,2)
best_ce<-c(13/3,13/3)
worst_ce<-c(3,3)

df_co$in_ce<-0
df_co$tre=0

df_co <- df_co %>% 
  mutate(tre=ifelse(sum(grep("MV", treatment)==1),1,tre))

df_co<- df_co %>% 
  mutate(in_ce = ifelse(tre==0 & p1_payoff/p2_payoff>=1 & abs((p2_payoff-200)/(p1_payoff-600))>=1/3 & abs((p2_payoff-200)/(p1_payoff-600))<=7/5,1,in_ce), in_ce = ifelse(tre==0 & p1_payoff/p2_payoff<1 & abs((p2_payoff-600)/(p1_payoff-200))>=5/7 & abs((p2_payoff-600)/(p1_payoff-200))<=3,1,in_ce))

df_co<- df_co %>% 
  mutate(in_ce = ifelse(tre==0 & p1_payoff/p2_payoff>1 & p1_payoff==600,1,in_ce), in_ce = ifelse(tre==0 & p1_payoff/p2_payoff<1 & p1_payoff==200,1,in_ce)) 

df_co<- df_co %>% 
  mutate(in_ce = ifelse(tre==1 & p1_payoff>100 & abs((p2_payoff-100)/(p1_payoff-100))>=1/2 & abs((p2_payoff-100)/(p1_payoff-100))<=2,1,in_ce))

df_co<- df_co %>% 
  mutate(in_ce = ifelse(tre==1 & p1_payoff==100 & p2_payoff==100,1,in_ce)) 

tapply(df_co$in_ce,df_co$treatment,mean)

p_success<-rbind(tapply(df_co$in_ce,df_co$treatment,sum),tapply(df_co$in_ce,df_co$treatment,sum)/tapply(df_co$in_ce,df_co$treatment,mean)-tapply(df_co$in_ce,df_co$treatment,sum))

binom.test(c(p_success[,1]), p = 4/15)
binom.test(c(p_success[,2]), p = 4/15)
binom.test(c(p_success[,3]), p = 4/15)
binom.test(c(p_success[,4]), p = 4/15)

binom.test(c(p_success[,5]), p = 1/9)
binom.test(c(p_success[,6]), p = 1/9)
binom.test(c(p_success[,7]), p = 1/9)
binom.test(c(p_success[,8]), p = 1/9)

#eq. line for y=-3x+1200, y=-5/7x+5200/7, y=-7/5x+5200/5,y=-1/3x+400
# or x = 400 -(1/3)y, x=-7/5y+5200/5, x = -5/7+5200/7, x=-3x + 1200
df_co$d_ce<- 1-df_co$in_ce
dy<-(-3*df_co$p1_payoff+1200-df_co$p2_payoff)
dx<-(400-1/3*df_co$p2_payoff-df_co$p1_payoff)

dy2<-(-5/7*df_co$p1_payoff+5200/7-df_co$p2_payoff)
dx2<-(5200/5-7/5*df_co$p2_payoff-df_co$p1_payoff)

dy3<-(-7/5*df_co$p1_payoff+5200/5-df_co$p2_payoff)
dx3<-(5200/7-5/7*df_co$p2_payoff-df_co$p1_payoff)

dy4<-(-1/3*df_co$p1_payoff+400-df_co$p2_payoff)
dx4<-(1200-3*df_co$p2_payoff-df_co$p1_payoff)

dym<-(df_co$p2_payoff-(2*df_co$p1_payoff-100))
dxm<-(1/2*df_co$p2_payoff+50-df_co$p1_payoff)

dym2<-(1/2*df_co$p1_payoff+50-df_co$p2_payoff)
dxm2<-(df_co$p1_payoff-(2*df_co$p2_payoff-100))

df_co$eq1<-(dy*dx)/sqrt(dy^2+dx^2)
df_co$eq2<-(dy2*dx2)/sqrt(dy2^2+dx2^2)
df_co$eq3<-(dy3*dx3)/sqrt(dy3^2+dx3^2)
df_co$eq4<-(dy4*dx4)/sqrt(dy4^2+dx4^2)

df_co$eqm<-(dym*dxm)/sqrt(dym^2+dxm^2)
df_co$eqm2<-(dym2*dxm2)/sqrt(dym2^2+dxm2^2)

df_co<- df_co %>% 
  mutate(d_ce = ifelse(tre==0 & p1_payoff/p2_payoff<1 & in_ce==0, apply(cbind(eq1,eq2), 1, FUN=min),d_ce), 
         d_ce = ifelse(tre==0 & p1_payoff/p2_payoff>1 & in_ce==0, apply(cbind(eq3,eq4), 1, FUN=min),d_ce), 
         d_ce = ifelse(tre==0 & p1_payoff/p2_payoff==1 & in_ce==0, apply(cbind(sqrt((300-p2_payoff)^2+(300-p1_payoff)^2),sqrt((1300/3-p2_payoff)^2+(1300/3-p1_payoff)^2)), 1, FUN=min),d_ce),
         d_ce = ifelse(tre==1 & p1_payoff<=100 & p2_payoff<=150-p1_payoff/2 & in_ce==0,sqrt((100-p2_payoff)^2+(100-p1_payoff)^2),d_ce),
         d_ce = ifelse(tre==1 & p1_payoff<=100 & p2_payoff>150-p1_payoff/2 & in_ce==0,eqm,d_ce), 
         d_ce = ifelse(tre==1 & p1_payoff>100 & p2_payoff<=300-2*p1_payoff & in_ce==0,sqrt((100-p2_payoff)^2+(100-p1_payoff)^2),d_ce),
         d_ce = ifelse(tre==1 & p1_payoff>100 & p2_payoff>300-2*p1_payoff  & p2_payoff< -2*p1_payoff+1400/3 & in_ce==0,apply(cbind(eqm,eqm2), 1, FUN=min),d_ce),
         d_ce = ifelse(tre==1 & p1_payoff>100 & p2_payoff> -2*p1_payoff+1400/3 & in_ce==0,sqrt((400/3-p2_payoff)^2+(500/3-p1_payoff)^2),d_ce)
  )

df_co$d_target<-NA

df_co<- df_co %>% 
  mutate(d_target = ifelse(tre==0,sqrt((1300/3-p2_payoff)^2+(1300/3-p1_payoff)^2),sqrt((150-p2_payoff)^2+(150-p1_payoff)^2) ))

tapply(df_co$d_ce,df_co$treatment,mean)
tapply(df_co$d_ce,df_co$treatment,median)
tapply(df_co$d_target,df_co$treatment,mean)

df_co$d_mne<-NA
df_co<- df_co %>% 
  mutate(d_mne = ifelse(tre==0,sqrt((350-p2_payoff)^2+(350-p1_payoff)^2),sqrt((100-p2_payoff)^2+(100-p1_payoff)^2) ))

tapply(df_co$d_mne,df_co$treatment,median)
tapply(df_co$d_mne,df_co$treatment,mean)
tapply(df_co$d_target,df_co$treatment,median)
tapply(df_co$d_target,df_co$treatment,mean)

wilcox.test(df_co$d_mne[df_co$treatment=="MV_L_A"],df_co$d_target[df_co$treatment=="MV_L_A"],paired = TRUE)
wilcox.test(df_co$d_mne[df_co$treatment=="MV_H_A"],df_co$d_target[df_co$treatment=="MV_H_A"],paired = TRUE)
wilcox.test(df_co$d_mne[df_co$treatment=="MV_L_C"],df_co$d_target[df_co$treatment=="MV_L_C"],paired = TRUE)
wilcox.test(df_co$d_mne[df_co$treatment=="MV_H_C"],df_co$d_target[df_co$treatment=="MV_H_C"],paired = TRUE)
wilcox.test(df_co$d_mne[df_co$treatment=="BM_H_A"],df_co$d_target[df_co$treatment=="BM_H_A"],paired = TRUE)
wilcox.test(df_co$d_mne[df_co$treatment=="BM_L_A"],df_co$d_target[df_co$treatment=="BM_L_A"],paired = TRUE)
wilcox.test(df_co$d_mne[df_co$treatment=="BM_H_C"],df_co$d_target[df_co$treatment=="BM_H_C"],paired = TRUE)
wilcox.test(df_co$d_mne[df_co$treatment=="BM_L_C"],df_co$d_target[df_co$treatment=="BM_L_C"],paired = TRUE)

wilcox.test(df_co$d_target[df_co$treatment=="MV_L_A"],df_co$d_target[df_co$treatment=="MV_L_C"],paired = FALSE)
wilcox.test(df_co$d_target[df_co$treatment=="MV_H_A"],df_co$d_target[df_co$treatment=="MV_H_C"],paired = FALSE)
wilcox.test(df_co$d_target[df_co$treatment=="BM_H_A"],df_co$d_target[df_co$treatment=="BM_H_C"],paired = FALSE)
wilcox.test(df_co$d_target[df_co$treatment=="BM_L_A"],df_co$d_target[df_co$treatment=="BM_L_C"],paired = FALSE)
wilcox.test(df_co$d_target[df_co$treatment=="MV_H_A"],df_co$d_target[df_co$treatment=="MV_L_A"],paired = FALSE)
wilcox.test(df_co$d_target[df_co$treatment=="MV_H_C"],df_co$d_target[df_co$treatment=="MV_L_C"],paired = FALSE)
wilcox.test(df_co$d_target[df_co$treatment=="BM_H_A"],df_co$d_target[df_co$treatment=="BM_L_A"],paired = FALSE)
wilcox.test(df_co$d_target[df_co$treatment=="BM_H_C"],df_co$d_target[df_co$treatment=="BM_L_C"],paired = FALSE)


ecdf_mvla <- ecdf(df_co$d_target[df_co$treatment=="MV_L_A"])
ecdf_mvlc <- ecdf(df_co$d_target[df_co$treatment=="MV_L_C"])
ecdf_mvha <- ecdf(df_co$d_target[df_co$treatment=="MV_H_A"])
ecdf_mvhc <- ecdf(df_co$d_target[df_co$treatment=="MV_H_C"])

ecdf_bmla <- ecdf(df_co$d_target[df_co$treatment=="BM_L_A"])
ecdf_bmlc <- ecdf(df_co$d_target[df_co$treatment=="BM_L_C"])
ecdf_bmha <- ecdf(df_co$d_target[df_co$treatment=="BM_H_A"])
ecdf_bmhc <- ecdf(df_co$d_target[df_co$treatment=="BM_H_C"])


ecdf_mvla_c <- ecdf(df_co$d_ce[df_co$treatment=="MV_L_A"])
ecdf_mvlc_c <- ecdf(df_co$d_ce[df_co$treatment=="MV_L_C"])
ecdf_mvha_c <- ecdf(df_co$d_ce[df_co$treatment=="MV_H_A"])
ecdf_mvhc_c <- ecdf(df_co$d_ce[df_co$treatment=="MV_H_C"])

ecdf_bmla_c <- ecdf(df_co$d_ce[df_co$treatment=="BM_L_A"])
ecdf_bmlc_c <- ecdf(df_co$d_ce[df_co$treatment=="BM_L_C"])
ecdf_bmha_c <- ecdf(df_co$d_ce[df_co$treatment=="BM_H_A"])
ecdf_bmhc_c <- ecdf(df_co$d_ce[df_co$treatment=="BM_H_C"])

png(here("Figures/cdf_mv_d_target.png"), width = 500, height = 500)
plot(ecdf_mvla, verticals=TRUE, do.points=FALSE,main="MV game",xlab="distance to target CE",ylab = "CDF")
plot(ecdf_mvlc, verticals=TRUE, do.points=FALSE, add=TRUE, col='brown')
plot(ecdf_mvha, verticals=TRUE, do.points=FALSE, add=TRUE, col='orange')
plot(ecdf_mvhc, verticals=TRUE, do.points=FALSE, add=TRUE, col='blue')
abline(h=.5,lty=3)
legend(10, .90, legend=c("LxA", "LxC","HxA","HxC"),
       col=c("black", "brown","orange","blue"), lty=1, cex=0.8,bty = "n")
dev.off()

png(here("Figures/cdf_bm_d_target.png"), width = 500, height = 500)
plot(ecdf_bmla, verticals=TRUE, do.points=FALSE,main="CH game",xlab="distance to target CE",ylab = "CDF")
plot(ecdf_bmlc, verticals=TRUE, do.points=FALSE, add=TRUE, col='brown')
plot(ecdf_bmha, verticals=TRUE, do.points=FALSE, add=TRUE, col='orange')
plot(ecdf_bmhc, verticals=TRUE, do.points=FALSE, add=TRUE, col='blue')
abline(h=.5,lty=3)
legend(10, .90, legend=c("LxA", "LxC","HxA","HxC"),
       col=c("black", "brown","orange","blue"), lty=1, cex=0.8,bty = "n")
dev.off()

png(here("Figures/cdf_mv_d_ce.png"), width = 500, height = 500)
plot(ecdf_mvla_c, verticals=TRUE, do.points=FALSE,main="MV game",xlab="distance to CE space",ylab = "CDF")
plot(ecdf_mvlc_c, verticals=TRUE, do.points=FALSE, add=TRUE, col='brown')
plot(ecdf_mvha_c, verticals=TRUE, do.points=FALSE, add=TRUE, col='orange')
plot(ecdf_mvhc_c, verticals=TRUE, do.points=FALSE, add=TRUE, col='blue')
abline(h=.5,lty=3)
legend(40, .20, legend=c("LxA", "LxC","HxA","HxC"),
       col=c("black", "brown","orange","blue"), lty=1, cex=0.8,bty = "n")
dev.off()

png(here("Figures/cdf_bm_d_ce.png"), width = 500, height = 500)
plot(ecdf_bmla_c, verticals=TRUE, do.points=FALSE,main="CH game",xlab="distance to CE space",ylab = "CDF")
plot(ecdf_bmlc_c, verticals=TRUE, do.points=FALSE, add=TRUE, col='brown')
plot(ecdf_bmha_c, verticals=TRUE, do.points=FALSE, add=TRUE, col='orange')
plot(ecdf_bmhc_c, verticals=TRUE, do.points=FALSE, add=TRUE, col='blue')
abline(h=.5,lty=3)
legend(40, .20, legend=c("LxA", "LxC","HxA","HxC"),
       col=c("black", "brown","orange","blue"), lty=1, cex=0.8,bty = "n")
dev.off()


#df_co$in_ce[df_co$treatment=='BM_L_C']<-sqrt((df_co$p1_payoff[df_co$treatment=='BM_L_C']-best_ce[1]*100)^2+(df_co$p1_payoff[df_co$treatment=='BM_L_C']-best_ce[2]*100)^2)


##### Payoff vectors figures #####
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


