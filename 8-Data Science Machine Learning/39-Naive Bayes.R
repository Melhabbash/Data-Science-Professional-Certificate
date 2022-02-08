# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Distance, Knn, Cross-validation, and Generative Models

### Generative Models

### Naive Bayes

library(tidyverse)
library(caret)

library(dslabs)
data("heights")

y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

# Estimating averages and standard deviations
params <- train_set %>% 
     group_by(sex) %>% 
     summarize(avg = mean(height), sd = sd(height))
params

# Estimating the prevalence
pi <- train_set %>% summarize(pi=mean(sex=="Female")) %>% pull(pi)
pi

x <- test_set$height

f0 <- dnorm(x, params$avg[2], params$sd[2])
f1 <- dnorm(x, params$avg[1], params$sd[1])

p_hat_bayes <- f1*pi / (f1*pi + f0*(1 - pi))


tmp <- heights %>% 
     mutate(x = round(height)) %>%
     group_by(x) %>%
     filter(n() >= 10) %>%
     summarize(prob = mean(sex == "Female")) 
naive_bayes_curve <- data.frame(x = seq(min(tmp$x), max(tmp$x))) %>%
     mutate(p_hat = dnorm(x, params$avg[1], params$sd[1])*pi/
                 (dnorm(x, params$avg[1], params$sd[1])*pi +
                       dnorm(x, params$avg[2], params$sd[2])*(1-pi)))
tmp %>% 
     ggplot(aes(x, prob)) +
     geom_point() +
     geom_line(data = naive_bayes_curve,
               mapping = aes(x, p_hat), lty = 3) 
