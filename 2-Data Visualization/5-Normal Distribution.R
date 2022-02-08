x<- 1:10
average<- sum(x)/length(x)
average
SD<- sqrt(sum((x-average)^2)/length(x))
SD

index<- heights$sex=="Male"
x<- heights$height[index]
average<-mean(x)
SD<- sd(x)
c(average=average, SD=SD)
z<- scale(x)
mean(abs(z)<2)
