# add package
rm(list = ls())
library(ggplot2)
library(xtable)
library(dplyr)
library(here)
source(here("simulations/MVregrets.R"))

# set up the parameters for the simulation
pay_MV = matrix(c(0,200,100,100,0,200,200,100,0),3,3) # payoff matrix 3x3
n = 50000 # number of periods in each simulation
experiment = 100 # number of experimentation periods where players randomly make decisions
start = 100 # number of periods when the data start counting
mu = 600 # HM response parameter
beta = 1 # logit response parameter
Delta = 0.8 # inertia logit parameter

# set up the vectors for choices and game parameters
history_p1 = rep(0, n)
history_p2 = rep(0, n)

# calculate the experimentation periods with random starting decisions
history_p1[1:experiment] = sample(1:3, experiment, replace = TRUE)
history_p2[1:experiment] = sample(1:3, experiment, replace = TRUE)

# calculate the rest of the decisions to n periods
for (i in (experiment+1):n){
  
  history_p1[i] = decision_hm2000r_logitR(mu, beta, i, history_p1, history_p2)
  history_p2[i] = decision_hm2000r_logitR(mu, beta, i, history_p2, history_p1)
}

# create the dataset for this simulation
dfsim = data.frame(
  p1_choice = history_p1[(experiment+1):n],
  p2_choice = history_p2[(experiment+1):n],
  period = (experiment+1):n
)

dfsim = dfsim %>% mutate(
  is_11 = ifelse(p1_choice==1 & p2_choice==1, 1, 0),
  is_12 = ifelse(p1_choice==1 & p2_choice==2, 1, 0),
  is_13 = ifelse(p1_choice==1 & p2_choice==3, 1, 0),
  is_21 = ifelse(p1_choice==2 & p2_choice==1, 1, 0),
  is_22 = ifelse(p1_choice==2 & p2_choice==2, 1, 0),
  is_23 = ifelse(p1_choice==2 & p2_choice==3, 1, 0),
  is_31 = ifelse(p1_choice==3 & p2_choice==1, 1, 0),
  is_32 = ifelse(p1_choice==3 & p2_choice==2, 1, 0),
  is_33 = ifelse(p1_choice==3 & p2_choice==3, 1, 0)
)

# calculate joint density
jd = matrix(0, nrow = 3, ncol = 3)

# record the joint density
jd[1,1] = round(mean(dfsim$is_11),3)
jd[1,2] = round(mean(dfsim$is_12),3)
jd[1,3] = round(mean(dfsim$is_13),3)
jd[2,1] = round(mean(dfsim$is_21),3)
jd[2,2] = round(mean(dfsim$is_22),3)
jd[2,3] = round(mean(dfsim$is_23),3)
jd[3,1] = round(mean(dfsim$is_31),3)
jd[3,2] = round(mean(dfsim$is_32),3)
jd[3,3] = round(mean(dfsim$is_33),3)
print(jd)

# pick 101-500 periods.
df = filter(dfsim, period>100&period<=500)
jd = matrix(0, nrow = 3, ncol = 3)
jd[1,1] = round(mean(df$is_11),3)
jd[1,2] = round(mean(df$is_12),3)
jd[1,3] = round(mean(df$is_13),3)
jd[2,1] = round(mean(df$is_21),3)
jd[2,2] = round(mean(df$is_22),3)
jd[2,3] = round(mean(df$is_23),3)
jd[3,1] = round(mean(df$is_31),3)
jd[3,2] = round(mean(df$is_32),3)
jd[3,3] = round(mean(df$is_33),3)
print(jd)

# pick 501-2000 periods.
df = filter(dfsim, period>40000&period<=50000)
jd = matrix(0, nrow = 3, ncol = 3)
jd[1,1] = round(mean(df$is_11),3)
jd[1,2] = round(mean(df$is_12),3)
jd[1,3] = round(mean(df$is_13),3)
jd[2,1] = round(mean(df$is_21),3)
jd[2,2] = round(mean(df$is_22),3)
jd[2,3] = round(mean(df$is_23),3)
jd[3,1] = round(mean(df$is_31),3)
jd[3,2] = round(mean(df$is_32),3)
jd[3,3] = round(mean(df$is_33),3)
print(jd)
