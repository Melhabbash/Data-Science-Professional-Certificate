# Question 1
# While studying heredity, Francis Galton developed what important statistical concept?
#   Standard deviation
#   Normal distribution
#(True) Correlation
#   Probability
############################################################################################

# Question 2
# The correlation coefficient is a summary of what?
#(True)The trend between two variables
#   The dispersion of a variable
#   The central tendency of a variable
#   The distribution of a variable

#############################################################################################
# Question 3
# Below is a scatter plot showing the relationship between two variables, x and y.
# Scatter plot of relationship between x (plotted on the x-axis) and y (plotted on the y-axis). 
# y-axis values range from -3 to 3; x-axis values range from -3 to 3. Points are fairly 
# well distributed in a tight band with a range from approximately (-2, 2) to (3, -3).
# From this figure, the correlation between x and y appears to be about:
#(True)-0.9
#   -0.2
#   0.9
#   2

# Correct. The variables x and y have a strong negative relationship with each other; 
# as x increases, y decreases.
###############################################################################################
# Question 4
# Instead of running a Monte Carlo simulation with a sample size of 25 from the 179 father-son 
# pairs described in the videos, we now run our simulation with a sample size of 50.
# Would you expect the mean of our sample correlation to increase, decrease, or 
# stay approximately the same?
#   Increase
#   Decrease
#(True)Stay approximately the same

################################################################################################
# Question 5
# Instead of running a Monte Carlo simulation with a sample size of 25 from the 179 
# father-son pairs described in the videos, we now run our simulation with a sample size of 50.
# Would you expect the standard deviation of our sample correlation to increase, decrease, 
# or stay approximately the same?
#   Increase
#(True)Decrease
#   Stay approximately the same

##################################################################################################
# Question 6
# 1 point possible (graded)
# If X and Y are completely independent, what do you expect the value of the correlation 
# coefficient to be?
#   -1
#   -0.5
#(True)0
#   0.5
#   1
#   Not enough information to answer the question

# Correct. If X and Y are independent, then their correlation coefficient is 0.
##################################################################################################
# Question 7
# Load the Lahman library. Filter the Teams data frame to include years from 1961 to 2001.What 
# is the correlation coefficient between number of runs per game and number of at bats per game?

library(Lahman)
Teams_small <- Teams %>% filter(yearID %in% 1961:2001)
cor(Teams_small$R/Teams_small$G, Teams_small$AB/Teams_small$G)   

##################################################################################################
# Question 8
# Use the filtered Teams data frame from Question 7.
# What is the correlation coefficient between win rate (number of wins per game)
# and number of errors per game?
cor(Teams_small$W/Teams_small$G, Teams_small$E/Teams_small$G)

# -0.3396947
##################################################################################################
# Question 9
# Use the filtered Teams data frame from Question 7.
# What is the correlation coefficient between doubles (X2B) per game and triples (X3B) per game?

cor(Teams_small$X2B/Teams_small$G, Teams_small$X3B/Teams_small$G) 

# -0.01157404