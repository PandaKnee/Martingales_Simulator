#Import Libraries
library(tidyverse)
library(mosaic)
library(kableExtra)
#set.seed(1) #optional for reproducibility

#adjustable variables
roulette_odds <- 18/37
sessions <- 10000
starting_bet <- 10
allotted_budget <- 10000

#constants
multiplier <- 2
earningsList <- numeric(sessions + 1)
earningsList[1] = 0
max_iterations <- floor(log2((allotted_budget / starting_bet) + 1))

#reset simulation
for(i in 2:sessions){
  earnings <- 0
  bal <- allotted_budget
  bet <- starting_bet

  while(bal > 0) {
    
    #if bet is over balance, quit
    if(bet > bal){
      break
    }
    
    #spin the wheel
    flip <- nflip(n = 1, prob = roulette_odds)
    bal <- bal - bet
    
    #if win, cash out
    if(flip == 1) {
      bal <- bal + bet * multiplier
      earnings <- bal - allotted_budget
      bal <- allotted_budget
      bet <- starting_bet
      earningsList[i] <- earnings
      break
    }
    else {
      earningsList[i] <- earningsList[i] - bet
      bet <- bet * multiplier
    }
  }
}
  
df <- tibble(earningsList)[1:sessions,]

# plot cumulative profit over time
ggplot(df, aes(x=1:sessions, y=cumsum(earningsList[1:sessions]))) + geom_line() + ylab("Net Profit") + geom_abline(slope = 1/37 * -starting_bet, color = 'red') + geom_abline(slope = 0 * -starting_bet, intercept = 0, color = 'green') + xlab(paste(sessions, "Sessions"))
######################################  

#expected value of strategy
expected <- starting_bet * ((1-(1-roulette_odds) ** max_iterations) - ((1-roulette_odds) ** max_iterations) * ((2 ** max_iterations) - 1))
  
#expected values plots (purple = martingale, green = 0, red = E(x) of repeated sessions of starting value)
ggplot() + geom_abline(slope = 1/37 * -starting_bet, color = 'red') + xlim(0,1000)+ ylim(-3000,0) + geom_abline(slope = expected, color = 'purple') + geom_abline(slope = 0 * -starting_bet, intercept = 0, color = 'green')

#win percentage by sessions
prob <- tibble(sessions = 1:1000, all_wins = (1-((1-roulette_odds) ** max_iterations))**sessions)
ggplot(prob) + geom_line(aes(x = sessions, y = all_wins))
kable(prob[1:5,])
