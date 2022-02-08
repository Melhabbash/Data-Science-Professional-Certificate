# This assessment has 6 multi-part questions that will all use the setup below.
# 
# Game attendance in baseball varies partly as a function of how well a team is playing.
# 
# Load the Lahman library. The Teams data frame contains an attendance column. 
# This is the total attendance for the season. To calculate average attendance, divide by the number of games played, as follows:
  
library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

# Use linear models to answer the following 3-part question about Teams_small.

# Question 1a


# Use runs (R) per game to predict average attendance.
Teams_small %>% mutate(rpg = R/G) %>%
  lm(avg_attendance ~ rpg, data = .)
# For every 1 run scored per game, average attendance increases by how much?
# 4117

# find regression line predicting attendance from R and take slope
Teams_small %>% 
  mutate(R_per_game = R/G) %>% 
  lm(avg_attendance ~ R_per_game, data = .) %>% 
  .$coef %>%
  .[2]
################################################################################ 
# Use home runs (HR) per game to predict average attendance.
#
Teams_small %>% mutate(hrpg = HR/G) %>%
  lm(avg_attendance ~ hrpg, data = .)
# For every 1 home run hit per game, average attendance increases by how much?
# 8113

Teams_small %>% 
  mutate(HR_per_game = HR/G) %>% 
  lm(avg_attendance ~ HR_per_game, data = .) %>% 
  .$coef %>%
  .[2]

######################################################################################## 
# Question 1b
# 
# Use number of wins to predict average attendance; do not normalize for number of games.
# 
Teams_small %>% lm(avg_attendance ~ W, data = .) # slope
# For every game won in a season, how much does average attendance increase?
# 121
Teams_small %>% 
  lm(avg_attendance ~ W, data = .) %>%
  .$coef %>%
  .[2]
#################
#
Teams_small %>% lm(avg_attendance ~ W, data = .) # intercept
# Suppose a team won zero games in a season.
# 
# Predict the average attendance.
#  1129
Teams_small %>% 
  lm(avg_attendance ~ W, data = .) %>% 
  .$coef %>%
  .[1]

# 
############################################################################################## 
# Question 1c
# 
# Use year to predict average attendance.
Teams_small %>% lm(avg_attendance ~ yearID, data = .)
# How much does average attendance increase each year?
#
Teams_small %>% 
  lm(avg_attendance ~ yearID, data = .) %>% 
  .$coef %>%
  .[2]

############################################################################################## 
# Question 2
# 
# Game wins, runs per game and home runs per game are positively correlated with attendance. 
# We saw in the course material that runs per game and home runs per game are correlated with
# each other. Are wins and runs per game or wins and home runs per game correlated? 
# Use the Teams_small data once again.
# 
corr <- Teams_small %>% mutate(rpg = R/G)
cor(x = corr$W, y = corr$rpg)
# What is the correlation coefficient for wins and runs per game?
# 0.412   
cor(Teams_small$W, Teams_small$R/Teams_small$G)
##################################################
corr <- Teams_small %>% mutate(hrpg = HR/G)
cor(x = corr$W, y = corr$hrpg)
# What is the correlation coefficient for wins and home runs per game?
# 0.274   
cor(Teams_small$W, Teams_small$HR/Teams_small$G)
################################################## 
# 
# Stratify Teams_small by wins: divide number of wins by 10 and then round to the nearest integer. 
# Keep only strata 5 through 10, which have 20 or more data points.
# 
# Use the stratified dataset to answer this three-part question.

strat_teams <- Teams_small %>%
mutate(strat = round(W/10)) %>%
  group_by(strat) %>%
  filter(strat %in% 5:10)
# Question 3a
# 
# How many observations are in the 8 win strata?
#   (Note that due to division and rounding, these teams have 75-85 wins.)
strat_teams %>% filter(strat == 8) %>% count()

# another solution
dat <- Teams_small %>%
  mutate(W_strata = round(W/10)) %>%
  filter(W_strata >= 5 & W_strata <= 10)

sum(dat$W_strata == 8)
############################################################################################# 
# Question 3b
# 
# Calculate the slope of the regression line predicting average attendance given runs per game 
# for each of the win strata.

# calculate slope of regression line after stratifying by R per game
dat %>%  
  group_by(W_strata) %>%
  summarize(slope = cor(R/G, avg_attendance)*sd(avg_attendance)/sd(R/G))
# Which win stratum has the largest regression line slope?
#(True)5
# 6
# 7
# 8
# 9
# 10
####################################################
# calculate slope of regression line after stratifying by HR per game
dat %>%  
  group_by(W_strata) %>%
  summarize(slope = cor(HR/G, avg_attendance)*sd(avg_attendance)/sd(HR/G))
# Calculate the slope of the regression line predicting average attendance given HR per game for 
# each of the win strata.
# 
# Which win stratum has the largest regression line slope?
#(True)5
# 6
# 7
# 8
# 9
# 10
# 
###################################### 
# 
# Question 3c
# 
# Which of the followng are true about the effect of win strata on average attendance?
#   Select ALL that apply.
# 
#(True)Across all win strata, runs per game are positively correlated with average attendance.
# Runs per game have the strongest effect on attendance when a team wins many games.
# After controlling for number of wins, home runs per game are not correlated with attendance.
#(True)Home runs per game have the strongest effect on attendance when a team does not win many games.
#(True)Among teams with similar numbers of wins, teams with more home runs per game have larger 
# average attendance.
# 
# 
################################################################################################# 
# Question 4
# 
# Fit a multivariate regression determining the effects of runs per game, home runs per game,
# wins, and year on average attendance. Use the original Teams_small wins column, not the win 
# strata from question 3.
# build model
fit <- Teams_small %>% 
  mutate(R_per_game = R/G,
         HR_per_game = HR/G) %>%
  lm(avg_attendance ~ R_per_game + HR_per_game + W + yearID, data = .)
tidy(fit) %>%
  filter(term == "R_per_game") %>%
  pull(estimate)


# What is the estimate of the effect of runs per game on average attendance?
#322

#################################
tidy(fit) %>%
  filter(term == "HR_per_game") %>%
  pull(estimate)
# What is the estimate of the effect of home runs per game on average attendance?
#    
##################################
tidy(fit) %>%
  filter(term == "W") %>%
  pull(estimate)
# What is the estimate of the effect of number of wins in a season on average attendance?
#    
# 
# 
###############################################################################################
# Question 5
# 
# Use the multivariate regression model from Question 4. Suppose a team averaged 5 runs per
# game, 1.2 home runs per game, and won 80 games in a season.
#
predict(fit, data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 2002))  

# What would this team's average attendance be in 2002?
# 16149   
#

################################
predict(fit, data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 1960))
# What would this team's average attendance be in 1960?
# 6505   
# 
################################ 
# Question 6
# 
# Use your model from Question 4 to predict average attendance for teams in 2002 in the original 
# Teams data frame.
# 
pred_teams <- Teams %>% filter(yearID == 2002) %>%
  mutate(rpg = R/G, hrpg = HR/G) %>%
  mutate(pred_attendance = predict(fit, data.frame(rpg = rpg, hrpg = hrpg, W = W, yearID = yearID)))

cor(pred_teams$attendance, pred_teams$pred_attendance)
# What is the correlation between the predicted attendance and actual attendance?
# 0.519   
newdata <- Teams %>%
  filter(yearID == 2002) %>%
  mutate(avg_attendance = attendance/G,
         R_per_game = R/G,
         HR_per_game = HR/G)
preds <- predict(fit, newdata)
cor(preds, newdata$avg_attendance)
