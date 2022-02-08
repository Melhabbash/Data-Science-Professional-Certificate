
population_in_millions<-murders$population/10^6
total_gun_murders<-murders$total
plot(population_in_millions,total_gun_murders)
hist(murders$rate)
murders$state[which.max(murders$rate)]
boxplot(rate~region, data = murders)
