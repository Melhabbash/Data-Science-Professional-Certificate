# load stringr through tidyverse
library(tidyverse)
library(dslabs)
data(murders)


# detect whether a comma is present
pattern <- ","
str_detect(murders$total, pattern) 
murders$total

# show the subset of strings including "cm"
str_subset(reported_heights$height, "cm")
reported_heights$height

# use the "or" symbol inside a regex (|)
yes <- c("180 cm", "70 inches")
yes
no <- c("180", "70''")
no
s <- c(yes, no)
str_detect(s, "cm") | str_detect(s, "inches")
str_detect(s, "cm|inches")

# highlight the first occurrence of a pattern
yes<- c("5","6","5'10","5 feet","4'11")
no<- c("",".","Five","six")
s<- c(yes,no)
s
pattern<-"\\d"
str_detect(s,pattern )
str_view(s, pattern)

# highlight all instances of a pattern
str_view_all(s, pattern)
