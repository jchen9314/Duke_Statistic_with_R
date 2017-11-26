library(statsr)
library('ggplot2')
library('dplyr')

data("kobe_basket")
# counting streak lengths 
# using custom function: "calc_streak" 
kobe_streak <- calc_streak(kobe_basket$shot)

# Question 1: The streak length of 1 means a hit followed by a miss

# Question 2: The streak length of 0 means a miss followed by a miss

# Question 3: 
ggplot(data = kobe_streak, aes(x = length)) +
  geom_histogram(binwidth = 1)
summary(kobe_streak$length)
# Answer: The shortest streak is of length 1

# Simulations in R
# Simulate flipping a fair coin
coin_outcomes <-c("heads", "tails")
sample(coin_outcomes, size = 1, replace = TRUE)
# Flipping a coin 100 times: change size
sim_fair_coin <- sample(coin_outcomes, size = 100, replace = TRUE)
# View the results of the simulation
sim_fair_coin
table(sim_fair_coin)
# Simulate unfair coin: add "prob"
sim_unfair_coin <- sample(coin_outcomes, size = 100, replace = TRUE, 
                          prob = c(0.2, 0.8))

# Question 4
# Simulating the Independent Shooter
shot_outcomes <- c("H", "M")
sim_basket <- sample(shot_outcomes, size = 133, replace = TRUE, 
                     prob = c(0.45, 0.55))
sim_streak <- calc_streak(sim_basket)
# Answer: Somewhat similar

# Compute summaries
summary(kobe_streak)
summary(sim_streak)

# Question 5
# Make bar plots
kobe_table <- table(kobe_streak)
sim_table <- table(sim_streak)

barplot(kobe_table, main='kobe_table')
barplot(sim_table, main='sim_table')
# Answer: The distributions look very similar. Therefore, there doesn't 
# appear to be evidence for Kobe Bryant's hot hand.