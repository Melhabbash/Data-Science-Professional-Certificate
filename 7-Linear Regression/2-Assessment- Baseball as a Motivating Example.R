# Question 1
# 
# What is the application of statistics and data science to baseball called?
#   Moneyball
# (True)Sabermetrics 
#   The "Oakland A's Approach"
#   There is no specific name for this; it's just data science.
#
# Correct. The term "sabermetrics" was coined by Bill James, and is derived from the acronym SABR:
# the society for American baseball research.
# 
# #############################################################################################
# Question 2
# 
# Which of the following outcomes is not included in the batting average?
#   A home run
#(True)A base on balls  
#   An out
#   A single
#
# Correct:Correct. A base on balls is not considered a hit and is excluded from the at-bat total.
#
# #############################################################################################
# Question 3
# 
# Why do we consider team statistics as well as individual player statistics?
#   The success of any individual player also depends on the strength of their team. 
#(True)Team statistics can be easier to calculate.
#   The ultimate goal of sabermetrics is to rank teams, not players.
# 
# #############################################################################################
# Question 4
# You want to know whether teams with more at-bats per game have more runs per game.
# 
# What R code below correctly makes a scatter plot for this relationship?
#   
#   Teams %>% filter(yearID %in% 1961:2001 ) %>%
#   ggplot(aes(AB, R)) + 
#   geom_point(alpha = 0.5)
# 
# 
#(True) Teams %>% filter(yearID %in% 1961:2001 ) %>%
#   mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
#   ggplot(aes(AB_per_game, R_per_game)) + 
#   geom_point(alpha = 0.5)   
# 
# 
# Teams %>% filter(yearID %in% 1961:2001 ) %>%
#   mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
#   ggplot(aes(AB_per_game, R_per_game)) + 
#   geom_line()
# 
# 
# Teams %>% filter(yearID %in% 1961:2001 ) %>%
#   mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
#   ggplot(aes(R_per_game, AB_per_game)) + 
#   geom_point()
# 
# Correct. This makes a scatter plot of runs per game (y-axis) vs. at-bats per game (x-axis).
# ###############################################################################################
# Question 5
# What does the variable "SOA" stand for in the Teams table?
#   Hint: make sure to use the help file (?Teams).
# 
#   sacrifice out
#   slides or attempts
#(True)strikeouts by pitchers
#   accumulated singles
# 
# ###############################################################################################
# Question 6
# Load the Lahman library. Filter the Teams data frame to include years from 1961 to 2001. 
# Make a scatterplot of runs per game versus at bats (AB) per game.
  library(Lahman)
  Teams_small <- Teams %>% filter(yearID %in% 1961:2001)
  cor(Teams_small$R/Teams_small$G, Teams_small$AB/Teams_small$G)
# head Teams
# Which of the following is true?
#   There is no clear relationship between runs and at bats per game.
#(True)As the number of at bats per game increases, the number of runs per game tends to increase.
#   As the number of at bats per game increases, the number of runs per game tends to decrease.
# 
# ###############################################################################################
# Question 7
# 
# Use the filtered Teams data frame from Question 6. Make a scatterplot of win rate 
# (number of wins per game) versus number of fielding errors (E) per game.
  Teams %>% filter(yearID %in% 1961:2001) %>%
    mutate(win_rate = W / G, E_per_game = E / G) %>%
    ggplot(aes(win_rate, E_per_game)) + 
    geom_point(alpha = 0.5)
# 
# Which of the following is true?
#   There is no relationship between win rate and errors per game.
#   As the number of errors per game increases, the win rate tends to increase.
#(True)As the number of errors per game increases, the win rate tends to decrease.
# 
################################################################################################# 
# Question 8
# 
# Use the filtered Teams data frame from Question 6. Make a scatterplot of triples (X3B) 
# per game versus doubles (X2B) per game.
  Teams %>% filter(yearID %in% 1961:2001) %>%
    mutate(doubles_per_game = X2B / G, triples_per_game = X3B / G) %>%
    ggplot(aes(doubles_per_game, triples_per_game)) + 
    geom_point(alpha = 0.5)
# 
# Which of the following is true?
#(True)There is no clear relationship between doubles per game and triples per game.
#   As the number of doubles per game increases, the number of triples per game tends to increase.
#   As the number of doubles per game increases, the number of triples per game tends to decrease.
# 

# Explanation
# When you examine the scatterplot, you can see no clear relationship between doubles and triples
# per game. The following code can be used to make the scatterplot:
    
    Teams %>% filter(yearID %in% 1961:2001) %>%
    mutate(doubles_per_game = X2B / G, triples_per_game = X3B / G) %>%
    ggplot(aes(doubles_per_game, triples_per_game)) + 
    geom_point(alpha = 0.5)
