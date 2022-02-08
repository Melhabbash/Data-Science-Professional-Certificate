
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

avg<- mean(x)
s<-sd(x)
1-pnorm(76,avg,s)

dnorm(x)
