#################################################################################################
# Question 1
# 
# The following code was used in the video to plot RSS with beta_0=25 .


beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)


# In a model for sons' heights vs fathers' heights, what is the least squares estimate (LSE) 
# for  if we assume  is 36?
  # Hint: modify the code above to do your analysis.
  # 
  # 0.65
#(True) 0.5
  # 0.2
  # 12

# Correct. You can tell from a plot of RSS vs beta_1 that the minimum estimate is 0.5
# 
# Explanation
# Using the code from the video, you can plot RSS vs beta_1 to find the value for beta_1 that 
# minimizes the RSS.
# In this case, that value is 0.5 when we assume that beta_0 is 36.
# 
# When we assumed that beta_0 was 25, as in the sample code, the LSE for  beta_1 was 0.65.


#################################################################################################
# Question 2
# 
# The least squares estimates for the parameters ____________  the residual sum of squares.
# Select an option

# maximize
#(True)minimize
# equal

# Explanation
# The least squares estimates minimize, not maximize, the residual sum of squares.
#################################################################################################
# Question 3
# 
# Load the Lahman library and filter the Teams data frame to the years 1961-2001. 
# Run a linear model in R predicting the number of runs per game based on both the number 
# of bases on balls per game and the number of home runs per game.
# 
# What is the coefficient for bases on balls?
#(True)0.39
#   1.56
#   1.74
#   0.027

# Explanation
# 
# The coefficient for bases on balls is 0.39; the coefficient for home runs is 1.56; 
# the intercept is 1.74; the standard error for the BB coefficient is 0.027.

library(Lahman)
library(broom)
Teams_small <- Teams %>% filter(yearID %in% 1961:2001)
Teams_small %>% 
  mutate(R_per_game = R/G, BB_per_game = BB/G, HR_per_game = HR/G) %>% 
  do(tidy(lm(R_per_game ~ BB_per_game + HR_per_game, data = .)))

##################################################################################################
# Question 4
# 
# We run a Monte Carlo simulation where we repeatedly take samples of N = 100 from the 
# Galton heights data and compute the regression slope coefficients for each sample:
  
  
B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 


# What does the central limit theorem tell us about the variables beta_0 and beta_1?
#   Select ALL that apply.
# 
#(True)They are approximately normally distributed.
#(True)The expected value of each is the true value of  and (assuming the Galton heights data 
#   is a complete population).
#   The central limit theorem does not apply in this situation.
#   It allows us to test the hypothesis that  and .


# Correct. With a large enough N, the distributions of both beta_0 and beta_1 are approximately 
# normal.
# 
# Explanation
# With a large enough N, the central limit theorem applies and tells us that the distributions 
# of both beta_0 and beta_1 are approximately normal. The expected values of beta_0 and beta_1 
# are the true values of  and , assuming that the Galton heights data are a complete population.
# For hypothesis testing, we assume that the errors in the model are normally distributed.

#################################################################################################
# Question 5
# 
# Which R code(s) below would properly plot the predictions and confidence intervals for 
# our linear model of sons' heights?
#   
#   NOTE: The function as.tibble() has been replaced by as_tibble() in a recent dplyr update.
# Select ALL that apply.
 

#    galton_heights %>% ggplot(aes(father, son)) +
#    geom_point() +
#    geom_smooth()
 
##############
#(True)galton_heights %>% ggplot(aes(father, son)) +
#   geom_point() +
#   geom_smooth(method = "lm")
 
##############
#(True)model <- lm(son ~ father, data = galton_heights)
#   predictions <- predict(model, interval = c("confidence"), level = 0.95)
#   data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

#   ggplot(data, aes(x = father, y = fit)) +
#   geom_line(color = "blue", size = 1) + 
#   geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
#   geom_point(data = galton_heights, aes(x = father, y = son))

#####################
#   model <- lm(son ~ father, data = galton_heights)
#   predictions <- predict(model)
#   data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)
# 
# ggplot(data, aes(x = father, y = fit)) +
#   geom_line(color = "blue", size = 1) + 
#   geom_point(data = galton_heights, aes(x = father, y = son))


# Correct. This is one way to plot predictions and confidence intervals for a linear model of 
# sons' heights vs. fathers' heights. This is one of two correct answers.
# Correct. This code uses the predict command to generate predictions and 95% confidence 
# intervals for the linear model of sons' heights vs. fathers' heights. This is one of two 
# correct answers.
# 
# Explanation
# If using the geom_smooth command, you need to specify that method = "lm" in your geom_smooth 
# command, otherwise the smooth line is a loess smooth and not a linear model.
# If using the predict command, you need to include the confidence intervals on your figure
# by first specifying that you want confidence intervals in the predict command, and then 
# adding them to your figure as a geom_ribbon.

