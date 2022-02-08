#This Library "tidyverse" is a must to use ggplot2
library(tidyverse)
library(dslabs)
data(murders)
ggplot(data = murders)
# we can also use pipe "murders %>% ggplot() . it does the same as the last code line , 
murders %>% ggplot()
p<- ggplot(data = murders)
class(p)
p
print(p) # this is equivalent to simply typing p
