# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Classification with More than Two Classes and the Caret Package

## Classification with More than Two Classes

### Classification and Regression Trees (CART)

# Load data
library(tidyverse)
library(dslabs)
data("olive")
olive %>% tbl_df

table(olive$region)

olive <- select(olive, -area)

# Predict region using KNN
library(caret)
fit <- train(region ~ .,  method = "knn", 
             tuneGrid = data.frame(k = seq(1, 15, 2)), 
             data = olive)
ggplot(fit)

# Plot distribution of each predictor stratified by region
olive %>% gather(fatty_acid, percentage, -region) %>%
     ggplot(aes(region, percentage, fill = region)) +
     geom_boxplot() +
     facet_wrap(~fatty_acid, scales = "free")+
        theme(axis.text.x = element_blank())

# plot values for eicosenoic and linoleic
p <- olive %>% 
     ggplot(aes(eicosenoic, linoleic, color = region)) + 
     geom_point()
p

p + geom_vline(xintercept = 0.065, lty = 2) + 
     geom_segment(x = -0.2, y = 10.54, xend = 0.065, yend = 10.54, color = "black", lty = 2)

# load data for regression tree
library(caret)
library(rpart)
train_rpart <- train(region ~ ., method = "rpart", data = olive)
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

data("polls_2008")
qplot(day, margin, data = polls_2008)

# visualize the splits 
library(rpart)
fit <- rpart(margin ~ ., data = polls_2008)
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

polls_2008 %>% 
     mutate(y_hat = predict(fit)) %>% 
     ggplot() +
     geom_point(aes(day, margin)) +
     geom_step(aes(day, y_hat), col="red")

# change parameters
fit <- rpart(margin ~ ., data = polls_2008,
             control = rpart.control(cp = 0, minsplit = 2))
polls_2008 %>% 
     mutate(y_hat = predict(fit)) %>% 
     ggplot() +
     geom_point(aes(day, margin)) +
     geom_step(aes(day, y_hat), col="red")

pruned_fit <- prune(fit, cp = 0.01)
polls_2008 %>% 
     mutate(y_hat = predict(pruned_fit)) %>% 
     ggplot() +
     geom_point(aes(day, margin)) +
     geom_step(aes(day, y_hat), col="red")

# use cross validation to choose cp
library(caret)
train_rpart <- train(margin ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
                     data = polls_2008)
ggplot(train_rpart)


# access the final model and plot it
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

polls_2008 %>% 
     mutate(y_hat = predict(train_rpart)) %>% 
     ggplot() +
     geom_point(aes(day, margin)) +
     geom_step(aes(day, y_hat), col="red")

# prune the tree 
pruned_fit <- prune(fit, cp = 0.01)