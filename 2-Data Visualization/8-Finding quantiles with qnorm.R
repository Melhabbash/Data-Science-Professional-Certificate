# pnorm function gives the probability that a value from a standard normal 
# distribution will be less than or equal to a z-score value z. 
# pnorm(-1.96) ???0.025 
# The result of pnorm() is the quantile.

p<-pnorm(-1.96)

# note that qnorm(0.025)??????1.96

qnorm(p)

# qnorm() and pnorm() are inverse functions:
# pnorm(qnorm(0.025))  =0.025
pnorm(qnorm(0.025))


# Suppose male heights follow a normal distribution with a mean of 69 inches and standard deviation of 3 inches. 
# The theoretical quantiles are:
p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p, 69, 3)
theoretical_quantiles 
