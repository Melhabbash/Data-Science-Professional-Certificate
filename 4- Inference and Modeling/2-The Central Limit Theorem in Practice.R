# Code: Computing the probability of  X¯  being within .01 of  p

X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/25)
se
pnorm(0.01/se) - pnorm(-0.01/se)
