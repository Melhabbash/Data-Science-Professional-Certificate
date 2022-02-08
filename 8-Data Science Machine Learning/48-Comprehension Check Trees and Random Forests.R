#Create a simple dataset where the outcome grows 0.75 units on average for every increase in 
# a predictor, using this code:

library(rpart)
n <- 1000
sigma <- 0.25
# set.seed(1) # if using R 3.5 or ealier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

# Q1
#Which code correctly uses rpart() to fit a regression tree and saves the result to fit?

# fit <- rpart(y ~ .) 
# 
# fit <- rpart(y, ., data = dat) 
# 
# fit <- rpart(x ~ ., data = dat) 
# 
# (True) fit <- rpart(y ~ ., data = dat)

fit <- rpart(y ~ ., data = dat) 


#########################################################################################################
# Q2
# Which of the following plots has the same tree shape obtained in Q1?

# Explanation
# The plot can be made using the following code:
plot(fit)
text(fit)


#########################################################################################################
# Q3
# Below is most of the code to make a scatter plot of y versus x along with the predicted values 
# based on the fit.

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  #BLANK
  
  
# Which line of code should be used to replace #BLANK in the code above?
  
# (True) geom_step(aes(x, y_hat), col=2)
# 
# geom_smooth(aes(y_hat, x), col=2)
# 
# geom_quantile(aes(x, y_hat), col=2)
# 
# geom_step(aes(y_hat, x), col=2)  
  
  geom_step(aes(x, y_hat), col=2)



#########################################################################################################
# Q4
# Now run Random Forests instead of a regression tree using randomForest() from the randomForest package, 
# and remake the scatterplot with the prediction line. Part of the code is provided for you below.

library(randomForest)
fit <- #BLANK 
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

#What code should replace #BLANK in the provided code?
#   (True) randomForest(y ~ x, data = dat)
# 
# randomForest(x ~ y, data = dat)
# 
# randomForest(y ~ x, data = data)
# 
# randomForest(x ~ y)

#########################################################################################################
# Q5
# Use the plot() function to see if the Random Forest from Q4 has converged or if we need more trees.

# Which of these graphs is most similar to the one produced by plotting the random forest? Note that 
# there may be slight differences due to the seed not being set.

plot(fit)
# Answer 3
########################################################################################################
# Q6
#It seems that the default values for the Random Forest result in an estimate that is too 
# flexible (unsmooth). Re-run the Random Forest but this time with a node size of 50 and a maximum of 
# 25 nodes. Remake the plot.
#Part of the code is provided for you below.

library(randomForest)
fit <- #BLANK
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

# What code should replace #BLANK in the provided code?

# randomForest(y ~ x, data = dat, nodesize = 25, maxnodes = 25)
# 
# randomForest(y ~ x, data = dat, nodes = 50, max = 25)
# 
# randomForest(x ~ y, data = dat, nodes = 50, max = 25)
# 
#        (True) randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
# 
# randomForest(x ~ y, data = dat, nodesize = 50, maxnodes = 25)


  randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
  
  # Explanation
  # We see that using randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25) yields smoother 
  # results. We'll pick up with this exercise after we learn more about the caret package.  