# Question 1
# 
# Look at the figure below.
# 
# Scatter plot of son and father heights with son heights on the y-axis and father heights on 
# the x-axis. There is also a regression line that runs from roughly (63,66) to (78,76). 
# The dots on the plot are scattered around the line.
# The slope of the regression line in this figure is equal to what, in words?
#(True)Slope = (correlation coefficient of son and father heights) * (standard deviation of sons' heights / standard deviation of fathers' heights)
#   Slope = (correlation coefficient of son and father heights) * (standard deviation of fathers' heights / standard deviation of sons' heights)
#   Slope = (correlation coefficient of son and father heights) / (standard deviation of sons' heights * standard deviation of fathers' heights)
#   Slope = (mean height of fathers) - (correlation coefficient of son and father heights * mean height of sons).
# 
################################################################################################### 
# Question 2
# Why does the regression line simplify to a line with intercept zero and slope ?? when we 
# standardize our x and y variables?
#   Try the simplification on your own first!
#   
#   When we standardize variables, both x and y will have a mean of one and a standard deviation of zero. When you substitute this into the formula for the regression line, the terms cancel out until we have the following equation: yi=??xi.
#(True)When we standardize variables, both x and y will have a mean of zero and a standard deviation of one. When you substitute this into the formula for the regression line, the terms cancel out until we have the following equation: yi=??xi.
#   When we standardize variables, both x and y will have a mean of zero and a standard deviation of one. When you substitute this into the formula for the regression line, the terms cancel out until we have the following equation: yi=??+xi.
# 
################################################################################################### 
# Question 3
# 
# What is a limitation of calculating conditional means?
#   Select ALL that apply.
# 
#(True)Each stratum we condition on (e.g., a specific father's height) may not have many data points.
#(True)Because there are limited data points for each stratum, our average values have large standard errors.
#(True)Conditional means are less stable than a regression line.
#   Conditional means are a useful theoretical tool but cannot be calculated.
# 
################################################################################################### 
# Question 4
# 
# A regression line is the best prediction of Y given we know the value of X when:
#(True)X and Y follow a bivariate normal distribution.
#   Both X and Y are normally distributed.
#   Both X and Y have been standardized.
#   There are at least 25 X-Y pairs.
# 
################################################################################################## 
# Question 5
# 
# Which one of the following scatterplots depicts an x and y distribution that is NOT well-approximated by the bivariate normal distribution?
#(True)scatter plot with v-shaped distribution of points centered on zero
#   scatter plot with rising slope and a roughly oval shaped distribution
#   scatter plot with slope of approximately zero and a very round oval shaped distribution
#   scatter plot with negative slope and a very long, tight oval shaped distribution of points
# 
# #################################################################################################
# Question 6
# We previously calculated that the correlation coefficient ?? between fathers' and sons' heights is 0.5.
# Given this, what percent of the variation in sons' heights is explained by fathers' heights?
#   0%
#(True)25%
#   50%
#   75%
# 
# Correct. When two variables follow a bivariate normal distribution, 
# the variation explained can be calculated as  ??2×100 .
################################################################################################### 
# Question 7
# 
# Suppose the correlation between father and son's height is 0.5, the standard deviation of fathers' 
# heights is 2 inches, and the standard deviation of sons' heights is 3 inches.
# Given a one inch increase in a father's height, what is the predicted change in the son's height?
#   0.333
#   0.5
#   0.667
#(True)0.75
#   1
#   1.5

# Correct! TThe slope of the regression line is calculated by multiplying the correlation 
# coefficient by the ratio of the standard deviation of son heights and standard deviation 
# of father heights:  ??son/??father .