a<- 2
class(a)
class(ls)
library(dslabs)
data("murders")
str(murders)
head(murders)
murders$population
names(murders)

pop<-murders$population
length(pop)
class(pop)

class(murders$state)

z<-2==3
z
class(z)
class(murders$region)

levels(murders$region)


