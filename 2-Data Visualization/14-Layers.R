
# Data %>%ggplot()+LAYER 1+LAYER 2+...+LAYER n
# geom_point()
# aes() aesthetic mapping

library(tidyverse)
library(dslabs)
data(murders)

murders %>% ggplot() +
  geom_point(aes(x = population/10^6, y = total))

# add points layer to predefined ggplot object
p <- ggplot(data = murders)
p + geom_point(aes(population/10^6, total))

# add text layer to scatterplot
p + geom_point(aes(population/10^6, total)) +
  geom_text(aes(population/10^6, total, label = abb))

p + geom_point(aes(population/10^6, total)) +
  geom_label(aes(population/10^6, total, label = abb))
