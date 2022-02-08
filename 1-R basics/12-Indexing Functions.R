
x<- c(FALSE, TRUE,FALSE, TRUE,TRUE,FALSE)
which(x)

index<- which(murders$state=="Massachusetts")
index
murder_rate[index]               

index<-murders$state=="Massachusetts"
murder_rate[index]

index<- match(c("New York", "Florida", "Texas"), murders$state)
index
murders$state[index]
murder_rate[index]


x<- c("a", "b", "c", "d", "e")
y<- c("a","d", "f")

y%in%x

c("Boston", "Dakota", "Washington") %in% murders$state


str(murders)
