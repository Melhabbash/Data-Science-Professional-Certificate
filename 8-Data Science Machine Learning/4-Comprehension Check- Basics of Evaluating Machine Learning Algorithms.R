#######################################################################################
# Q1
# For each of the following, indicate whether the outcome is continuous or categorical.
# 
# Digit reader
# 
# categorical
################## 
# Height
# 
# continuous
##################
# Spam filter
# 
#categorical.
##################
# Stock prices
#
#continuous
##################
# Sex
# 
#categorical.

################################################################################### 
# Q2
# How many features are available to us for prediction in the mnist digits dataset?
# You can download the mnist dataset using the read_mnist() function from the dslabs package.
mnist <- read_mnist()
ncol(mnist$train$images)   
