#############################################################################################
# Q1
# Create a data set using the following code:
library(tidyverse)
library(caret)

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

# We will build 100 linear models using the data above and calculate the mean and standard 
# deviation of the combined models. First, set the seed to 1 again (make sure to use sample.
# kind="Rounding" if your R is version 3.6 or later). Then, within a replicate() loop, 
# (1) partition the dataset into test and training sets with p = 0.5 and using dat$y to 
# generate your indices, 
# (2) train a linear model predicting y from x, 
# (3) generate predictions on the test set, and 
# (4) calculate the RMSE of that model. 
# Then, report the mean and standard deviation (SD) of the RMSEs from all 100 models.
# Report all answers to at least 3 significant digits.

library(caret)
set.seed(1)
rmse_dat <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  
  fit <- lm(y~x, data=train_set)
  y_hat <- predict(fit, test_set)
  
  RMSE<-sqrt(mean((y_hat - test_set$y)^2))
  
})

mean(rmse_dat)
sd(rmse_dat)

#################################################################################################
# Q2
# Now we will repeat the exercise above but using larger datasets. Write a function that takes 
# a size n, then (1) builds a dataset using the code provided at the top of Q1 but with 
# n observations instead of 100 and without the set.seed(1), (2) runs the replicate() loop 
# that you wrote to answer Q1, which builds 100 linear models and returns a vector of RMSEs, 
# and (3) calculates the mean and standard deviation of the 100 RMSEs.

# Note: You only need to set the seed once before running your function; do not set a seed within 
# your function. Also be sure to use sapply() or map() as you will get different answers 
# running the simulations individually due to setting the seed.

# set.seed(1) # if R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if R 3.6 or later
n <- c(100, 500, 1000, 5000, 10000)
res <- sapply(n, function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    sqrt(mean((y_hat-test_set$y)^2))
  })
  c(avg = mean(rmse), sd = sd(rmse))
})

res

################################################################################################
# Q3
# What happens to the RMSE as the size of the dataset becomes larger?

# (True) On average, the RMSE does not change much as n gets larger, but the variability of the 
#        RMSE decreases.

  # Because of the law of large numbers the RMSE decreases; more data means more precise estimates.

  # n = 10000 is not sufficiently large. To see a decrease in the RMSE we would need to make it 
  # larger.

  # The RMSE is not a random variable.


################################################################################################
# Q4
# Now repeat the exercise from Q1, this time making the correlation between x and y larger, 
# as in the following code:

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))


set.seed(1)
rmse_dat <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  
  fit <- lm(y~x, data=train_set)
  y_hat <- predict(fit, test_set)
  
  RMSE<-sqrt(mean((y_hat - test_set$y)^2))
  
})

mean(rmse_dat) #0.9099808
sd(rmse_dat) #0.06244347


################################################################################################
# Q5
# Which of the following best explains why the RMSE in question 4 is so much lower than the RMSE 
# in question 1?

  # It is just luck. If we do it again, it will be larger.

  # The central limit theorem tells us that the RMSE is normal.

#(True) When we increase the correlation between x and y, x has more predictive power and thus 
  # provides a better estimate of y.

  # These are both examples of regression so the RMSE has to be the same.

# Explanation
# The correlation between x and y has a much bigger effect on RMSE than n. 
# Large n simply provides us with more precise estimates of the linear model coefficients.
################################################################################################

# Q6
# Create a data set using the following code.
set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

# Note that y is correlated with both x_1 and x_2 but the two predictors are independent of each other, as seen by cor(dat).

# Set the seed to 1, then use the caret package to partition into test and training sets 
# with p = 0.5. Compare the RMSE when using just x_1, just x_2 and both x_1 and x_2. 
# Train a single linear model for each (not 100 like in the previous questions).

# Which of the three models performs the best (has the lowest RMSE)?

cor(dat)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)

train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

###############
fit <- lm(y~x_1, data=train_set)
y_hat <- predict(fit, test_set)

RMSE<-sqrt(mean((y_hat - test_set$y)^2))
mean(RMSE)
#0.6708231
sd(RMSE)
#Na
###############
fit <- lm(y~x_2, data=train_set)
y_hat <- predict(fit, test_set)

RMSE<-sqrt(mean((y_hat - test_set$y)^2))
mean(RMSE)
#0.6775359
sd(RMSE)
###############
fit <- lm(y~x_1 + x_2, data=train_set)
y_hat <- predict(fit, test_set)

RMSE<-sqrt(mean((y_hat - test_set$y)^2))
mean(RMSE)
#0.3552544
sd(RMSE)
###############
#x_1 and x_2

################################################################################################
# Q7
# Report the lowest RMSE of the three models tested in Q6.
set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)

train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y~x_1 + x_2, data=train_set)
y_hat <- predict(fit, test_set)

RMSE<-sqrt(mean((y_hat - test_set$y)^2))
mean(RMSE) #0.3070962

################################################################################################
# Q8
# Repeat the exercise from q6 but now create an example in which x_1 and x_2 are highly correlated.
set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
data.frame() %>% setNames(c("y", "x_1", "x_2"))

# Set the seed to 1, then use the caret package to partition into a test and training set of equal 
# size. Compare the RMSE when using just x_1, just x_2, and both x_1 and x_2.

# Compare the results from q6 and q8. What can you conclude?

  # Unless we include all predictors we have no predictive power.

  # Adding extra predictors improves RMSE regardless of whether the added predictors are 
  # correlated with other predictors or not.

  # Adding extra predictors results in over fitting.

# (True) Adding extra predictors can improve RMSE substantially, but not when the added predictors 
  # are highly correlated with other predictors.


set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)

train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y~x_1, data=train_set)
y_hat <- predict(fit, test_set)
RMSE<-sqrt(mean((y_hat - test_set$y)^2))
mean(RMSE)

fit_x2 <- lm(y~x_2, data=train_set)
y_hat <- predict(fit, test_set)
RMSE<-sqrt(mean((y_hat - test_set$y)^2))
mean(RMSE)

fit_x1_x2 <- lm(y~x_1 + x_2, data=train_set)
y_hat <- predict(fit, test_set)
RMSE<-sqrt(mean((y_hat - test_set$y)^2))
mean(RMSE)

# When the predictors are highly correlated with each other, adding addtional predictors 
# does not improve the model substantially, thus RMSE stays roughly the same.