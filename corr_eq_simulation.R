# set up parameters
mu = 100
pay_chicken = matrix(c(100,200,600,500),2,2)
pay_MV = matrix(c(0,200,100,100,0,200,200,100,0),3,3)

# calculate the initial periods with random starting decisions
history_p1 = sample(1:2, 1)
history_p2 = sample(1:2, 1)
payoff_p1 = pay_chicken[history_p1, history_p2]
payoff_p2 = pay_chicken[history_p1, history_p2]
