# A casino offers a House Special bet on roulette, which is a bet on five pockets 
# (00, 0, 1, 2, 3) out of 38 total pockets. The bet pays out 6 to 1. In other words, 
# a losing bet yields -$1 and a successful bet yields $6. A gambler wants to know 
# the chance of losing money if he places 500 bets on the roulette House Special.
# 
# The following 7-part question asks you to do some calculations related to this scenario.

################################################################################
# Question 3a
# What is the expected value of the payout for one bet?

# (00,0,1,2,3) --> 5 chances out o 38 

p<-5/38
a<-6
b<--1

mu<-a*p +b*(1-p)
mu
################################################################################

# Question 3b
# What is the standard error of the payout for one bet?

sigma<-abs(b-a)*sqrt(p*(1-p))
sigma

################################################################################

# Question 3c
# What is the expected value of the average payout over 500 bets?
# Remember there is a difference between expected value of the average and expected 
# value of the sum.

mu

################################################################################

# Question 3d
# What is the standard error of the average payout over 500 bets?
# Remember there is a difference between the standard error of the average and 
# standard error of the sum.

n<-500
sigma/sqrt(n)

################################################################################

# Question 3e
# What is the expected value of the sum of 500 bets?

n*mu


################################################################################

# Question 3f
# What is the standard error of the sum of 500 bets?
sqrt(n)*sigma

################################################################################

# Question 3g Use pnorm() with the expected value of the sum and standard error 
# of the sum to calculate the probability of losing money over 500 bets, Pr(X???0) .


