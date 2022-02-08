# For datasets that are not normal, the CDF can be calculated manually by defining 
# a function to compute the probability above. 
# This function can then be applied to a range of values across the range of the 
# dataset to calculate a CDF. 
# Given a dataset my_data, the CDF can be calculated and plotted like this:

# define range of values spanning the dataset
a <- seq(min(my_data), max(my_data), length = 100)
# computes prob. for a single value
cdf_function <- function(x) {   
  mean(my_data <= x)
}
cdf_values <- sapply(a, cdf_function)
plot(a, cdf_values)