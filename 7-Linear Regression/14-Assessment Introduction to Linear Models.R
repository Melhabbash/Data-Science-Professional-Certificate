################################################################################################# 
# Question 1
# 
# As described in the videos, when we stratified our regression lines for runs per game vs. 
# bases on balls by the number of home runs, what happened?
#   
#(True)The slope of runs per game vs. bases on balls within each stratum was reduced 
#   because we removed confounding by home runs.

#   The slope of runs per game vs. bases on balls within each stratum was reduced 
#   because there were fewer data points.

#   The slope of runs per game vs. bases on balls within each stratum increased 
#   after we removed confounding by home runs.

#   The slope of runs per game vs. bases on balls within each stratum stayed about 
#   the same as the original slope.
# 
# Explanation
# Home runs are a confounder in the runs per game vs. bases on balls regression analysis. 
# When we removed confounding by home runs, the slope of runs per game vs. bases on balls 
# within each stratum decreased.
################################################################################################## 
# Question 2
# 
# We run a linear model for sons' heights vs. fathers' heights using the Galton height data, 
# and get the following results:
#   
lm(son ~ father, data = galton_heights)

# Call:
#   lm(formula = son ~ father, data = galton_heights)
# 
# Coefficients:
#   (Intercept)    father
# 35.71       0.50
# 
# Interpret the numeric coefficient for "father."
#   For every inch we increase the son's height, the predicted father's height increases by 0.5 inches.
#(True)For every inch we increase the father's height, the predicted son's height grows by 0.5 inches.
#   For every inch we increase the father's height, the predicted son's height is 0.5 times greater.
#
# Explanation
# The coefficient for "father" gives the predicted increase in son's height for each increase 
# of 1 unit in the father's height. In this case, it means that for every inch we increase 
# the father's height, the son's predicted height increases by 0.5 inches.
# 
################################################################################################## 
# Question 3
# 
# We want the intercept term for our model to be more interpretable, so we run the same model as
# before but now we subtract the mean of fathers' heights from each individual father's height 
# to create a new variable centered at zero.

galton_heights <- galton_heights %>%
mutate(father_centered=father - mean(father))

# We run a linear model using this centered fathers' height variable.
# 
lm(son ~ father_centered, data = galton_heights)

# Call:
#   lm(formula = son ~ father_centered, data = galton_heights)

# Coefficients:
#   (Intercept)    father_centered  
# 70.45          0.50  
# 
# Interpret the numeric coefficient for the intercept.
#(True)The height of a son of a father of average height is 70.45 inches.
#   The height of a son when a father's height is zero is 70.45 inches.
#   The height of an average father is 70.45 inches.
# 
# Explanation

# Because the fathers' heights (the independent variable) have been centered on their mean, 
# the intercept represents the height of the son of a father of average height. 
# In this case, that means that the height of a son of a father of average height is 70.45 inches.
# If we had not centered fathers' heights to its mean, then the intercept would represent 
# the height of a son when a father's height is zero.
# 
################################################################################################## 
# Question 4
# 
# Suppose we fit a multivariable regression model for expected runs based on BB and HR:
#   
#                                E[R|BB=x1,HR=x2]=??0+??1x1+??2x2
#Suppose we fix BB=x1. Then we observe a linear relationship between runs and HR with intercept of:
#   ??0
#   ??0+??2x2
#(True)??0+??1x1
#   ??0+??2x1
# 
#Explanation
# If  x1  is fixed, then  ??1x1  is fixed and acts as the intercept for this regression model. 
# This is the basis of stratificaton. 
################################################################################################# 
# Question 5
# 
# Which of the following are assumptions for the errors ??i in a linear regression model?
#   Check ALL correct answers.
# 
#(True)The ??i are independent of each other
#(True)The ??i have expected value 0
#(True)The variance of ??i is a constant
# 
