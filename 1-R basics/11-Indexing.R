
murder_rate<- murders$total/murders$population*100000
index<- murder_rate <0.71
index<- murder_rate<=0.71
index
murders$state[index]
sum(index)
west<- murders$region=="West"
safe<- murder_rate <=1
index<- west&safe
murders$state[index]

