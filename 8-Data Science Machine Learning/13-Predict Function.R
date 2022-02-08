# Key points
# The predict() function takes a fitted object from functions such as lm() or glm() and 
# a data frame with the new predictors for which to predict. We can use predict like this:
y_hat <- predict(fit, test_set)
# predict() is a generic function in R that calls other functions depending on what kind of 
# object it receives. To learn about the specifics, you can read the help files using code 
# like this: 
?predict.lm    # or ?predict.glm

###############################################################################################
# Code
### Predict Function

y_hat <- predict(fit, test_set)

# seed was not set so values may be different
mean((y_hat - test_set$son)^2)

?predict.lm
?predict.glm