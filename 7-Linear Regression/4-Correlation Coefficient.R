
# rho <- mean(scale(x)*scale(y))
# galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)
library(tidyverse)
library(HistData)
data("GaltonFamilies")
galton_heights<-GaltonFamilies %>%filter(childNum==1 & gender=="male") %>% 
  select(father,childHeight) %>%rename(son=childHeight)
galton_height %>% summarize(cor(father,son))
