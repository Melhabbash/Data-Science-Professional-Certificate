# Exercise 1.Sample average

# Write function called take_sample that takes the proportion of Democrats p and the sample size N as arguments 
# and returns the sample average of Democrats (1) and Republicans (0).
# Calculate the sample average if the proportion of Democrats equals 0.45 and the sample size is 100.
# 
# Instructions
# 
# Define a function called take_sample that takes p and N as arguments.
# Use the sample function as the first statement in your function to sample N elements from a vector of options 
# where Democrats are assigned the value '1' and Republicans are assigned the value '0'.
# Use the mean function as the second statement in your function to find the average value of the random sample.


# Write a function called `take_sample` that takes `p` and `N` as arguements and returns the average value of
# a randomly sampled population.
take_sample <- function(p, N){
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1 - p, p))
  mean(X)
}

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Call the `take_sample` function to determine the sample average of `N` randomly selected people from 
# a population containing a proportion of Democrats equal to `p`. Print this value to the console.
take_sample(p,N)

################################################################################

# Exercise 2. Distribution of errors - 1
# Assume the proportion of Democrats in the population p equals 0.45 and that your sample size N is 100 polled 
# voters. The take_sample function you defined previously generates our estimate,  X¯.
# Replicate the random sampling 10,000 times and calculate  p???X¯ for each random sample. 
# Save these differences as a vector called errors. Find the average of errors and plot a histogram of the 
# distribution.
# 
# Instructions
# 
# The function take_sample that you defined in the previous exercise has already been run for you.
# Use the replicate function to replicate subtracting the result of take_sample from the value of p 10,000 times.
# Use the mean function to calculate the average of the differences between the sample average and actual 
# value of p. 
# 
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Create an objected called `errors` that replicates subtracting the result of the `take_sample` function 
# from `p` for `B` replications
errors <- replicate(B, p - take_sample(p, N))

# Calculate the mean of the errors. Print this value to the console.
mean(errors)

################################################################################

# Exercise 3.Distribution of errors - 2
# In the last exercise, you made a vector of differences between the actual value for  p and an estimate,  X¯. 
# We called these differences between the actual and estimated values errors.
# The errors object has already been loaded for you. Use the hist function to plot a histogram of the values 
# contained in the vector errors. Which statement best describes the distribution of the errors?

hist(errors)
#   
#   Possible Answers
# 
# A. The errors are all about 0.05.
# B. The error are all about -0.05.
# C. The errors are symmetrically distributed around 0.
# D. The errors range from -1 to 1.

