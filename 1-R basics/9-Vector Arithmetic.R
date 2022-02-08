
murders$state[which.max(murders$population)]
max(murders$population)

heights<- c(69,62,66,70,70,73,67,73,67,70)
heights*2.54
heights-69


murder_rate<- murders$total/murders$population*100000
murders$state[order(murder_rate, decreasing = TRUE)]
murders$total
str(murders)
(pi^2)/6
 