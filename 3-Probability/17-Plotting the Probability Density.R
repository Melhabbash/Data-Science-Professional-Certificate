# We can use dnorm() to plot the density curve for the normal distribution. dnorm(z) gives 
# the probability density f(z) of a certain z-score, so we can draw a curve by calculating 
# the density over a range of possible values of z.
# First, we generate a series of z-scores covering the typical range of the normal 
# distribution. Since we know 99.7% of observations will be within -3<=z<=3. we can use 
# a value of  z  slightly larger than 3 and this will cover most likely values of the 
# normal distribution. Then, we calculate f(z) , which is dnorm() of the series of z-scores.
# Last, we plot z against f(z).

library(tidyverse)
x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()
