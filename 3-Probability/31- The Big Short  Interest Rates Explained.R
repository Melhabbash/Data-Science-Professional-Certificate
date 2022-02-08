# Interest rates explained with chance model
# More complex versions of the sampling models we have discussed are also used by banks to decide interest
# rates. Suppose you run a small bank that has a history of identifying potential homeowners that can be
# trusted to make payments. In fact, historically, in a given year, only 2% of your customers default, meaning
# that they don't pay back the money that you lent them. However, you are aware that if you simply loan
# money to everybody without interest, you will end up losing money due to this 2%. Although you know 2%
#   of your clients will probably default, you don't know which ones. Yet by charging everybody just a bit extra
# in interest, you can make up the losses incurred due to that 2% and also cover your operating costs. You
# can also make a profit, but if you set the interest rates too high, your clients will go to another bank. We
# use all these facts and some probability theory to decide what interest rate you should charge.
# Suppose your bank will give out 1,000 loans for $180,000 this year. Also, after adding up all costs, suppose
# your bank loses $200,000 per foreclosure. For simplicity, we assume this includes all operational costs. A
# sampling model for this scenario can be coded like this:

# Code: Interest rate sampling model
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)


################################################################################

# Note that the total loss defined by the final sum is a random variable. Every time you run the above code, you
# get a different answer. We can easily construct a Monte Carlo simulation to get an idea of the distribution
# of this random variable.

# Code: Interest rate Monte Carlo simulation
B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
  sum(defaults * loss_per_foreclosure)
})

################################################################################

# Code: Plotting expected losses
library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col = "black")

################################################################################

# We don't really need a Monte Carlo simulation though. Using what we have learned, the CLT tells us that
# because our losses are a sum of independent draws, its distribution is approximately normal with expected
# value and standard errors given by:
# Code: Expected value and standard error of the sum of 1,000 loans

n <- 1000
p <- 0.02
loss_per_foreclosure <- -200000

mu<- n*(p*loss_per_foreclosure + (1-p)*0)
sigma<- sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))

################################################################################

# We can now set an interest rate to guarantee that, on average, we break even. Basically, we need to add a
# quantity x to each loan, which in this case are represented by draws, so that the expected value is 0. If we
# define l to be the loss per foreclosure, we need:
# l*p + x*(1 ??? p) = 0

# We can calculate the amount  x  to add to each loan so that the expected value is 0 using the equation  
# l*p + x*(1???p)=0 . Note that this equation is the definition of expected value given a loss per foreclosure  l 
# with foreclosure probability  p  and profit  x  if there is no foreclosure (probability  1???p ).

# We solve for  x= ???l*p/(1???p)  and calculate  x :
x <- - loss_per_foreclosure*p/(1-p)
x
# On a $180,000 loan, this equals an interest rate of:
x/180000

################################################################################

# However, we still have a problem. Although this interest rate guarantees that on average we break even,
# there is a 50% chance that we lose money. If our bank loses money, we have to close it down. We therefore
# need to pick an interest rate that makes it unlikely for this to happen. At the same time, if the interest rate
# is too high, our clients will go to another bank so we must be willing to take some risks. So let's say that
# we want our chances of losing money to be 1 in 100, what does the x quantity need to be now? This one is
# a bit harder. We want the sum S to have:
#   Pr(S < 0) = 0.01
# We know that S is approximately normal. The expected value of S is
# E[S] = {lp + x(1 ??? p)}*n
# with n the number of draws, which in this case represents loans. The standard error is
# SD[S] = |x ??? l|*???np(1 ??? p)
# .
# Because x is positive and l negative |x ??? l| = x ??? l. Note that these are just an application of the formulas
# shown earlier, but using more compact symbols.

# Equations: Calculating interest rate for 1% probability of losing money
# We want to calculate the value of  x  for which  Pr(S<0)=0.01 . 
# The expected value  E[S]  of the sum of  n=1000  loans given our definitions of  x ,l and p is:
#   
#   ??S=(l*p + x*(1???p))???n

# And the standard error of the sum of  n  loans,  SE[S] , is:
#   
#   ??S= ???x???l???*sqrt(n*p(1???p))

# Because we know the definition of a Z-score is  Z=(x?????)/?? , we know that  Pr(S<0)=Pr(Z<???????) . 
# Thus,  Pr(S<0)=0.01  equals:
#   
#   Pr(Z<???{lp+x(1???p)}n / ???x???l???*sqrt(n*p(1???p ))) = 0.01 

# z<-qnorm(0.01) gives us the value of  z  for which  Pr(Z???z)=0.01 , meaning:
#   
#   z= {lp+x(1???p)}n / ???x???l???*sqrt(n*p(1???p )) 

# Solving for  x  gives:
#   
#   x=-l*{n*p-z*sqrt(n*p(1???p))}/{n*(1-p)+z*sqrt(n*p*(1-p))}


# Code: Calculating interest rate for 1% probability of losing money

l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))\x
x/180000    # interest rate
loss_per_foreclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p)) # expected value of the profit over n loans


