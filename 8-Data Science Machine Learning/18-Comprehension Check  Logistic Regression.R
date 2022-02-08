##################################################################################################
# Q1
# Define a dataset using the following code:
library(caret)
library(dplyr)
set.seed(2)
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

# Note that we have defined a variable x that is predictive of a binary outcome y: 
# dat$train %>% ggplot(aes(x, color = y)) + geom_density().

# Set the seed to 1, then use the make_data() function defined above to generate 25 different 
# datasets with mu_1 <- seq(0, 3, len=25). Perform logistic regression on each of the 25 different 
# datasets (predict 1 if p>0.5) and plot accuracy (res in the figures) vs mu_1 
# (delta in the figures).
# Which is the correct plot?



dat$train %>% ggplot(aes(x, color = y)) + geom_density()
set.seed(1)
delta <- seq(0, 3, len = 25)
res <- sapply(delta, function(d){
  dat <- make_data(mu_1 = d)
  fit_glm <- dat$train %>% glm(y ~ x, family = "binomial", data = .)
  y_hat_glm <- ifelse(predict(fit_glm, dat$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
  mean(y_hat_glm == dat$test$y)
})
qplot(delta, res)
