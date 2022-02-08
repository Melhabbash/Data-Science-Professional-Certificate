
x<- c(2,43,27,96,18)
sort(x)
order(x)
rank(x)
min(x)
which.min(x)
max(x)
which.max(x)


name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)
time<-time/60
speed<-distance/time
data.frame(name=name, distance=distance, time=time, speed=speed)
