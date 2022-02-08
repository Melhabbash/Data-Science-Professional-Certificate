# In Questions 7 and 8, you'll look again at female heights from GaltonFamilies.

# Define female_heights, a set of mother and daughter heights sampled from GaltonFamilies, 
# as follows:

set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
    filter(gender == "female") %>%     
    group_by(family) %>%     
    sample_n(1) %>%     
    ungroup() %>%     
    select(mother, childHeight) %>%     
    rename(daughter = childHeight)

#################################################################################################
# Question 7
# 
# Fit a linear regression model predicting the mothers' heights using daughters' heights.
fit <-lm(formula = childHeight ~ mother, data = GaltonFamilies)
fit$coefficients
summary(fit)
# What is the slope of the model?
#  0.315  
#  
# What the intercept of the model?
   
# 46.587

#################################################################################################
# Question 8
# 
# Predict mothers' heights using the model.
# 
Y_hat <- predict(fit, se.fit = TRUE)
Y_hat[[1]]
predict(fit, se.fit = TRUE)
# What is the predicted height of the first mother in the dataset?
# 67.7   
#
female_heights$mother[[1]]
# What is the actual height of the first mother in the dataset?
#   67 


# We have shown how BB and singles have similar predictive power for scoring runs. Another way to 
# compare the usefulness of these baseball metrics is by assessing how stable they are across 
# the years. Because we have to pick players based on their previous performances, we will prefer 
# metrics that are more stable. In these exercises, we will compare the stability of singles and BBs.
# Before we get started, we want to generate two tables: one for 2002 and another for the average of
# 1999-2001 seasons. We want to define per plate appearance statistics, keeping only players with 
# more than 100 plate appearances. Here is how we create the 2002 table:
  
  library(Lahman)
  bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

 
############################################################################################## 
#   Question 9
# 
# Now compute a similar table but with rates computed over 1999-2001. Keep only rows from 1999-2001
# where players have 100 or more plate appearances, calculate each player's single rate and BB 
# rate per stint (where each row is one stint - a player can have multiple stints within a season),
# then calculate the average single rate (mean_singles) and average BB rate (mean_bb) per player
# over the three year period.
  bat_9901 <- Batting %>% filter(yearID %in% 1999:2001) %>%
    mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
    filter(pa >= 100) %>%
    group_by(playerID) %>%
    summarize(mean_singles = mean(singles), mean_bb = mean(bb))%>%
  select(playerID, mean_singles, mean_bb)
  
  
# How many players had a single rate mean_singles of greater than 0.2 per plate appearance over 
# 1999-2001?
  bat_9901 %>% filter(mean_singles > 0.2)
  # 46
 
# How many players had a BB rate mean_bb of greater than 0.2 per plate appearance over 1999-2001?
  bat_9901 %>% filter(mean_bb > 0.2) 
 # 3



# Question 10

# Use inner_join() to combine the bat_02 table with the table of 1999-2001 rate averages you 
# created in the previous question.

  j <- inner_join(bat_02, bat_9901)
# What is the correlation between 2002 singles rates and 1999-2001 average singles rates?
  cor(j$singles, j$mean_singles)  
  # 0.551
  
# What is the correlation between 2002 BB rates and 1999-2001 average BB rates?
  cor(j$bb, j$mean_bb) 
 
  # 0.717

##################################################################################################
# Question 11
# 
# Make scatter plots of mean_singles versus singles and mean_bb versus bb.
# 
  j %>% ggplot(aes(singles, mean_singles)) + 
    geom_point()
  j %>% ggplot(aes(bb, mean_bb)) + 
    geom_point()
# Are either of these distributions bivariate normal?
# Neither distribution is bivariate normal.
# singles and mean_singles are bivariate normal, but bb and mean_bb are not.
# bb and mean_bb are bivariate normal, but singles and mean_singles are not.
# Both distributions are bivariate normal.


#################################################################################################
# Question 12
# 
# Fit a linear model to predict 2002 singles given 1999-2001 mean_singles.
#
  fit <- lm(singles ~ mean_singles, data = j)
  fit$coefficients
  summary(fit) 
# What is the coefficient of mean_singles, the slope of the fit?
#   0.5881 
#  
# Fit a linear model to predict 2002 bb given 1999-2001 mean_bb.
  fit <- lm(bb ~ mean_bb, data = j)
  fit$coefficients
 
   summary(fit)
# What is the coefficient of mean_bb, the slope of the fit?
   # 0.8290
 