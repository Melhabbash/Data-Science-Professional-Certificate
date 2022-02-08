# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Model Fitting and Recommendation Systems

## Case Study: MNIST

### Case Study: MNIST

library(tidyverse)
library(dslabs)
mnist <- read_mnist()

names(mnist)

dim(mnist$train$images)

class(mnist$train$labels)
table(mnist$train$labels)

# sample 10k rows from training set, 1k rows from test set
set.seed(123)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$train$images), 1000)

#note that the line above is the corrected code - code in video at 0:52 is incorrect
x_test <- mnist$train$images[index,]
y_test <- factor(mnist$train$labels[index])
