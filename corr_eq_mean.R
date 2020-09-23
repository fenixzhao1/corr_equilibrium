##### Data preparation #####
# load packages
library(ggplot2)
library(dplyr)
library(xtable)

# load data 
bimatrix_choice <- read.csv("D:/Dropbox/Working Papers/Correlated Equilibrium/data/pilots/pilot_mean_9_21.csv", header = T)

# create round variable in choice data and create full dataset
bimatrix_choice$round = as.double(substring(bimatrix_choice$subsession_id, 3, 4))
full_data = bimatrix_choice
full_data = arrange(full_data, full_data$session_code, full_data$subsession_id, full_data$id_in_subsession, full_data$tick)
full_data = full_data %>% mutate(period = tick + 1)

# create round/pair id
full_data$pair_id = paste(full_data$p1_code, full_data$p2_code, sep = "_")
full_data$round_pair_id = paste(full_data$round, full_data$pair_id,  sep = "_")
full_data$session_round_pair_id = paste(full_data$session_code, full_data$round_pair_id, sep = "_")
full_data$session_round_id = paste(full_data$session_code, full_data$round, sep = "_")

# create treatment variable
full_data = full_data %>% mutate(matching = ifelse(mean_matching == TRUE, 'Meanmatch', 'Pairwise'))
full_data = full_data %>% mutate(time = ifelse(num_subperiods == 0, 'Continuous', 'Discrete'))
full_data = full_data %>% mutate(information = ifelse(max_info == TRUE, 'MaxInfo', 'MinInfo'))
full_data$treatment = paste(full_data$game, full_data$information, full_data$matching, full_data$time, sep = '_')

# rewrite each player's choices into dummies
full_data = full_data %>% mutate(p1_strategy_0 = ifelse(p1_strategy == 0, 1, 0))
full_data = full_data %>% mutate(p1_strategy_1 = ifelse(p1_strategy == 1, 1, 0))
full_data = full_data %>% mutate(p1_strategy_2 = ifelse(p1_strategy == 2, 1, 0))
full_data = full_data %>% mutate(p2_strategy_0 = ifelse(p2_strategy == 0, 1, 0))
full_data = full_data %>% mutate(p2_strategy_1 = ifelse(p2_strategy == 1, 1, 0))
full_data = full_data %>% mutate(p2_strategy_2 = ifelse(p2_strategy == 2, 1, 0))
full_data = full_data %>% mutate(p3_strategy_0 = ifelse(p3_strategy == 0, 1, 0))
full_data = full_data %>% mutate(p3_strategy_1 = ifelse(p3_strategy == 1, 1, 0))
full_data = full_data %>% mutate(p3_strategy_2 = ifelse(p3_strategy == 2, 1, 0))
full_data = full_data %>% mutate(p4_strategy_0 = ifelse(p4_strategy == 0, 1, 0))
full_data = full_data %>% mutate(p4_strategy_1 = ifelse(p4_strategy == 1, 1, 0))
full_data = full_data %>% mutate(p4_strategy_2 = ifelse(p4_strategy == 2, 1, 0))
full_data = full_data %>% mutate(p5_strategy_0 = ifelse(p5_strategy == 0, 1, 0))
full_data = full_data %>% mutate(p5_strategy_1 = ifelse(p5_strategy == 1, 1, 0))
full_data = full_data %>% mutate(p5_strategy_2 = ifelse(p5_strategy == 2, 1, 0))
full_data = full_data %>% mutate(p6_strategy_0 = ifelse(p6_strategy == 0, 1, 0))
full_data = full_data %>% mutate(p6_strategy_1 = ifelse(p6_strategy == 1, 1, 0))
full_data = full_data %>% mutate(p6_strategy_2 = ifelse(p6_strategy == 2, 1, 0))

# calculate mean strategy
full_data = full_data %>% mutate(p1_average_0 = 0)
full_data = full_data %>% mutate(p1_average_1 = 0)
full_data = full_data %>% mutate(p1_average_2 = 0)
full_data = full_data %>% mutate(p2_average_0 = 0)
full_data = full_data %>% mutate(p2_average_1 = 0)
full_data = full_data %>% mutate(p2_average_2 = 0)
full_data = full_data %>% mutate(p3_average_0 = 0)
full_data = full_data %>% mutate(p3_average_1 = 0)
full_data = full_data %>% mutate(p3_average_2 = 0)

for (i in 1:length(full_data$tick)){
  if (full_data$game[i] != 'FP'){
    full_data$p1_average_0[i] = (full_data$p1_strategy_0[i] + full_data$p3_strategy_0[i] + full_data$p5_strategy_0[i]) / 3
    full_data$p2_average_0[i] = (full_data$p2_strategy_0[i] + full_data$p4_strategy_0[i] + full_data$p6_strategy_0[i]) / 3
    full_data$p1_average_1[i] = (full_data$p1_strategy_1[i] + full_data$p3_strategy_1[i] + full_data$p5_strategy_1[i]) / 3
    full_data$p2_average_1[i] = (full_data$p2_strategy_1[i] + full_data$p4_strategy_1[i] + full_data$p6_strategy_1[i]) / 3
    full_data$p1_average_2[i] = (full_data$p1_strategy_2[i] + full_data$p3_strategy_2[i] + full_data$p5_strategy_2[i]) / 3
    full_data$p2_average_2[i] = (full_data$p2_strategy_2[i] + full_data$p4_strategy_2[i] + full_data$p6_strategy_2[i]) / 3
  }
  else{
  full_data$p1_average_0[i] = (full_data$p1_strategy_0[i] + full_data$p4_strategy_0[i]) / 2
  full_data$p2_average_0[i] = (full_data$p2_strategy_0[i] + full_data$p5_strategy_0[i]) / 2
  full_data$p3_average_0[i] = (full_data$p3_strategy_0[i] + full_data$p6_strategy_0[i]) / 2
  full_data$p1_average_1[i] = (full_data$p1_strategy_1[i] + full_data$p4_strategy_1[i]) / 2
  full_data$p2_average_1[i] = (full_data$p2_strategy_1[i] + full_data$p5_strategy_1[i]) / 2
  full_data$p3_average_1[i] = (full_data$p3_strategy_1[i] + full_data$p6_strategy_1[i]) / 2
  full_data$p1_average_2[i] = (full_data$p1_strategy_2[i] + full_data$p4_strategy_2[i]) / 2
  full_data$p2_average_2[i] = (full_data$p2_strategy_2[i] + full_data$p5_strategy_2[i]) / 2
  full_data$p3_average_2[i] = (full_data$p3_strategy_2[i] + full_data$p6_strategy_2[i]) / 2
 }
}

# check number of subjects
uniquePlayer = union(unique(full_data$p1_code), unique(full_data$p2_code))


##### Pair-level data #####
# pair level dynamics
full_data = full_data %>% mutate(p1_strategy_j = ifelse(p1_role=='p1', p1_strategy, ifelse(p1_role=='p2', p1_strategy+0.01, p1_strategy+0.02)))
full_data = full_data %>% mutate(p2_strategy_j = ifelse(p2_role=='p1', p2_strategy, ifelse(p2_role=='p2', p2_strategy+0.01, p2_strategy+0.02)))
full_data = full_data %>% mutate(p3_strategy_j = ifelse(p3_role=='p1', p3_strategy, ifelse(p3_role=='p2', p3_strategy+0.01, p3_strategy+0.02)))
full_data = full_data %>% mutate(p4_strategy_j = ifelse(p4_role=='p1', p4_strategy, ifelse(p4_role=='p2', p4_strategy+0.01, p4_strategy+0.02)))
full_data = full_data %>% mutate(p5_strategy_j = ifelse(p5_role=='p1', p5_strategy, ifelse(p5_role=='p2', p5_strategy+0.01, p5_strategy+0.02)))
full_data = full_data %>% mutate(p6_strategy_j = ifelse(p6_role=='p1', p6_strategy, ifelse(p6_role=='p2', p6_strategy+0.01, p6_strategy+0.02)))

uniquepairs = unique(full_data$session_round_pair_id)

# loop over pairs
for (i in 1:length(uniquepairs)){
  pairdata = subset(full_data, session_round_pair_id == uniquepairs[i])
  
  title = paste(as.character(pairdata$game[1]), as.character(pairdata$information[1]),
                as.character(uniquepairs[i]), sep = '_')
  file = paste("D:/Dropbox/Working Papers/Correlated Equilibrium/data/figures/mean_group/", title, sep = "")
  file = paste(file, ".png", sep = "")
  
  png(file, width = 1000, height = 500)
  par(mai=c(1.5, 1, 0.5, 0.5))
  xy=par("usr")
  
  # BM plots
  if (pairdata$game[1] == 'BM'){
    plot(pairdata$period, pairdata$p1_strategy_j, type='l', col="blue",
         xlab="time", ylab="strategy mixture", ylim = c(0,1), main=title)
    lines(pairdata$period, pairdata$p2_strategy_j, type='l', col="red")
    lines(pairdata$period, pairdata$p3_strategy_j, type='l', col="blue")
    lines(pairdata$period, pairdata$p4_strategy_j, type='l', col="red")
    lines(pairdata$period, pairdata$p5_strategy_j, type='l', col="blue")
    lines(pairdata$period, pairdata$p6_strategy_j, type='l', col="red")
  }
  # MV plots
  else if (pairdata$game[1] == 'MV'){
    plot(pairdata$period, pairdata$p1_strategy_j, type='l', col="blue",
         xlab="time", ylab="strategy mixture", ylim = c(0,2), main=title)
    lines(pairdata$period, pairdata$p2_strategy_j, type='l', col="red")
    lines(pairdata$period, pairdata$p3_strategy_j, type='l', col="blue")
    lines(pairdata$period, pairdata$p4_strategy_j, type='l', col="red")
    lines(pairdata$period, pairdata$p5_strategy_j, type='l', col="blue")
    lines(pairdata$period, pairdata$p6_strategy_j, type='l', col="red")
  }
  # FP plots
  else{
    plot(pairdata$period, pairdata$p1_strategy_j, type='l', col="blue",
         xlab="time", ylab="strategy mixture", ylim = c(0,2), main=title)
    lines(pairdata$period, pairdata$p2_strategy_j, type='l', col="red")
    lines(pairdata$period, pairdata$p3_strategy_j, type='l', col="black")
    lines(pairdata$period, pairdata$p4_strategy_j, type='l', col="blue")
    lines(pairdata$period, pairdata$p5_strategy_j, type='l', col="red")
    lines(pairdata$period, pairdata$p6_strategy_j, type='l', col="black")
  }
  
  # add legend
  legend(x=xy[2]-xinch(0.2), y=xy[3]-yinch(0.8), lty=1,
         c("p1", "p2", "p3"), col=c("blue", "red", "black"), xpd=TRUE)
  
  dev.off()
}


##### Joint density #####
uniquetreatment = unique(full_data$treatment)
density_matrix = list()

for (i in 1:length(uniquetreatment)){
  df_treatment = filter(full_data, treatment == uniquetreatment[i])
  
  # BM game
  if (df_treatment$game[1] == 'BM'){
    density_matrix[[i]] = matrix(NA, nrow = 2, ncol = 2)
    rownames(density_matrix[[i]]) = c('U', 'D')
    colnames(density_matrix[[i]]) = c('L', 'R')
    
    density_matrix[[i]][1,1] = mean(df_treatment$p1_average_0)*mean(df_treatment$p2_average_0)
    density_matrix[[i]][1,2] = mean(df_treatment$p1_average_0)*mean(df_treatment$p2_average_1)
    density_matrix[[i]][2,1] = mean(df_treatment$p1_average_1)*mean(df_treatment$p2_average_0)
    density_matrix[[i]][2,2] = mean(df_treatment$p1_average_1)*mean(df_treatment$p2_average_1)
  }
  # MV game
  else if (df_treatment$game[1] == 'MV'){
    density_matrix[[i]] = matrix(NA, nrow = 3, ncol = 3)
    rownames(density_matrix[[i]]) = c('T', 'M', 'D')
    colnames(density_matrix[[i]]) = c('L', 'C', 'R')
    
    density_matrix[[i]][1,1] = mean(df_treatment$p1_average_0)*mean(df_treatment$p2_average_0)
    density_matrix[[i]][1,2] = mean(df_treatment$p1_average_0)*mean(df_treatment$p2_average_1)
    density_matrix[[i]][1,3] = mean(df_treatment$p1_average_0)*mean(df_treatment$p2_average_2)
    density_matrix[[i]][2,1] = mean(df_treatment$p1_average_1)*mean(df_treatment$p2_average_0)
    density_matrix[[i]][2,2] = mean(df_treatment$p1_average_1)*mean(df_treatment$p2_average_1)
    density_matrix[[i]][2,3] = mean(df_treatment$p1_average_1)*mean(df_treatment$p2_average_2)
    density_matrix[[i]][3,1] = mean(df_treatment$p1_average_2)*mean(df_treatment$p2_average_0)
    density_matrix[[i]][3,2] = mean(df_treatment$p1_average_2)*mean(df_treatment$p2_average_1)
    density_matrix[[i]][3,3] = mean(df_treatment$p1_average_2)*mean(df_treatment$p2_average_2)
  }
  # FT game
  else{
    density_matrix[[i]] = matrix(NA, nrow = 2, ncol = 6)
    rownames(density_matrix[[i]]) = c('U', 'D')
    colnames(density_matrix[[i]]) = c('LA', 'RA', 'LB', 'RB', 'LC', 'RC')
    
    density_matrix[[i]][1,1] = mean(df_treatment$p1_average_0)*mean(df_treatment$p2_average_0)*mean(df_treatment$p3_average_0)
    density_matrix[[i]][1,2] = mean(df_treatment$p1_average_0)*mean(df_treatment$p2_average_1)*mean(df_treatment$p3_average_0)
    density_matrix[[i]][2,1] = mean(df_treatment$p1_average_1)*mean(df_treatment$p2_average_0)*mean(df_treatment$p3_average_0)
    density_matrix[[i]][2,2] = mean(df_treatment$p1_average_1)*mean(df_treatment$p2_average_1)*mean(df_treatment$p3_average_0)
    density_matrix[[i]][1,3] = mean(df_treatment$p1_average_0)*mean(df_treatment$p2_average_0)*mean(df_treatment$p3_average_1)
    density_matrix[[i]][1,4] = mean(df_treatment$p1_average_0)*mean(df_treatment$p2_average_0)*mean(df_treatment$p3_average_1)
    density_matrix[[i]][2,3] = mean(df_treatment$p1_average_0)*mean(df_treatment$p2_average_0)*mean(df_treatment$p3_average_1)
    density_matrix[[i]][2,4] = mean(df_treatment$p1_average_0)*mean(df_treatment$p2_average_0)*mean(df_treatment$p3_average_1)
    density_matrix[[i]][1,5] = mean(df_treatment$p1_average_0)*mean(df_treatment$p2_average_0)*mean(df_treatment$p3_average_2)
    density_matrix[[i]][1,6] = mean(df_treatment$p1_average_0)*mean(df_treatment$p2_average_0)*mean(df_treatment$p3_average_2)
    density_matrix[[i]][2,5] = mean(df_treatment$p1_average_0)*mean(df_treatment$p2_average_0)*mean(df_treatment$p3_average_2)
    density_matrix[[i]][2,6] = mean(df_treatment$p1_average_0)*mean(df_treatment$p2_average_0)*mean(df_treatment$p3_average_2)
  }
}

# table output
xtable(density_matrix[[1]], digits = 2, caption = uniquetreatment[1])
xtable(density_matrix[[2]], digits = 2, caption = uniquetreatment[2])
xtable(density_matrix[[3]], digits = 2, caption = uniquetreatment[3])
xtable(density_matrix[[4]], digits = 2, caption = uniquetreatment[4])
xtable(density_matrix[[5]], digits = 2, caption = uniquetreatment[5])
xtable(density_matrix[[6]], digits = 2, caption = uniquetreatment[6])


##### Aggregate over time #####
uniquetreatment = unique(full_data$treatment)

# generate aggregate data over period by treatments
aggregate_plot = list()
for (i in 1:length(uniquetreatment)){
  df_treatment = filter(full_data, treatment == uniquetreatment[i])
  # BM and MV data
  if (df_treatment$game[1] != 'FP'){
    aggregate_plot[[i]] = data.frame(
      p1_average_0 = tapply(df_treatment$p1_average_0, df_treatment$period, mean),
      p1_average_1 = tapply(df_treatment$p1_average_1, df_treatment$period, mean),
      p1_average_2 = tapply(df_treatment$p1_average_2, df_treatment$period, mean),
      p2_average_0 = tapply(df_treatment$p2_average_0, df_treatment$period, mean),
      p2_average_1 = tapply(df_treatment$p2_average_1, df_treatment$period, mean),
      p2_average_2 = tapply(df_treatment$p2_average_2, df_treatment$period, mean),
      period = tapply(df_treatment$period, df_treatment$period, mean)
    )
  }
  # FT data
  else{
    aggregate_plot[[i]] = data.frame(
      p1_average_0 = tapply(df_treatment$p1_average_0, df_treatment$period, mean),
      p1_average_1 = tapply(df_treatment$p1_average_1, df_treatment$period, mean),
      p1_average_2 = tapply(df_treatment$p1_average_2, df_treatment$period, mean),
      p2_average_0 = tapply(df_treatment$p2_average_0, df_treatment$period, mean),
      p2_average_1 = tapply(df_treatment$p2_average_1, df_treatment$period, mean),
      p2_average_2 = tapply(df_treatment$p2_average_2, df_treatment$period, mean),
      p3_average_0 = tapply(df_treatment$p3_average_0, df_treatment$period, mean),
      p3_average_1 = tapply(df_treatment$p3_average_1, df_treatment$period, mean),
      p3_average_2 = tapply(df_treatment$p3_average_2, df_treatment$period, mean),
      period = tapply(df_treatment$period, df_treatment$period, mean)
    )
    
  }
  
  # start to draw plot
  title = as.character(df_treatment$treatment[1])
  file = paste("D:/Dropbox/Working Papers/Correlated Equilibrium/data/figures/mean_aggregate/", title, sep = "")
  file = paste(file, ".png", sep = "")
  png(file, width = 800, height = 500)
  
  # BM plot
  if (df_treatment$game[1] == 'BM'){
    pic = ggplot(data = aggregate_plot[[i]]) +
      geom_line(aes(x=period, y=p1_average_1, colour='blue', linetype='solid')) +
      geom_line(aes(x=period, y=p2_average_1, colour='red', linetype='solid')) +
      scale_x_discrete(name='period', waiver(), limits=c(1,10,20,30,40)) +
      scale_y_continuous(name='average mixture', limits=c(0,1)) +
      ggtitle(title) + 
      theme_bw() + 
      scale_colour_manual(values=c('blue','red'), labels=c('p1-D', 'p2-R')) +
      theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
            axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
            axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))
  }
  # MV plot
  else if (df_treatment$game[1] == 'MV'){
    pic = ggplot(data = aggregate_plot[[i]]) +
      geom_line(aes(x=period, y=p1_average_1, colour='blue', linetype='solid')) +
      geom_line(aes(x=period, y=p1_average_2, colour='blue', linetype='dashed')) +
      geom_line(aes(x=period, y=p2_average_1, colour='red', linetype='solid')) +
      geom_line(aes(x=period, y=p2_average_2, colour='red', linetype='dashed')) +
      scale_x_discrete(name='period', waiver(), limits=c(1,10,20,30,40)) +
      scale_y_continuous(name='average mixture', limits=c(0,1)) +
      ggtitle(title) + 
      theme_bw() + 
      scale_colour_manual(values=c('blue','red'), labels=c('p1', 'p2')) +
      scale_linetype_manual(values=c('dashed', 'solid'), labels=c('B/R', 'M/C')) +
      theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
            axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
            axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))
  }
  # FT plot
  else if (df_treatment$game[1] == 'FP'){ 
    pic = ggplot(data = aggregate_plot[[i]]) +
      geom_line(aes(x=period, y=p1_average_1, colour='blue', linetype='solid')) +
      geom_line(aes(x=period, y=p2_average_1, colour='red', linetype='solid')) +
      geom_line(aes(x=period, y=p3_average_1, colour='black', linetype='solid')) +
      geom_line(aes(x=period, y=p3_average_2, colour='black', linetype='dashed')) +
      scale_x_discrete(name='period', waiver(), limits=c(1,10,20,30,40)) +
      scale_y_continuous(name='average mixture', limits=c(0,1)) +
      ggtitle(title) + 
      theme_bw() + 
      scale_colour_manual(values=c('black', 'blue', 'red'), labels=c('p3', 'p1', 'p2')) +
      scale_linetype_manual(values=c('dashed', 'solid'), labels=c('C', 'D/R/B')) +
      theme(plot.title = element_text(hjust = 0.5, size = 20), legend.text = element_text(size = 15),
            axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15),
            axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15))
  }
  
  print(pic)
  dev.off()
}
