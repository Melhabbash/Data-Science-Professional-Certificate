#############################################################
# Exercise 1. American Roulette probability of winning money
# The exercises in the previous chapter explored winnings in American roulette. In this chapter of exercises,
# we will continue with the roulette example and add in the Central Limit Theorem.
# 
# In the previous chapter of exercises, you created a random variable S that is the sum of your winnings after 
# betting on green a number of times in American Roulette.
# 
# What is the probability that you end up winning money if you bet on green 100 times?
# 
# Execute the sample code to determine the expected value avg and standard error se as you have done in 
# previous exercises.
# Use the pnorm function to determine the probability of winning money.

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 100

# Calculate 'avg', the expected outcome of 100 spins if you win $17 when the ball lands on green and you 
# lose $1 when the ball doesn't land on green
avg <- n * (17*p_green + -1*p_not_green)

# Compute 'se', the standard error of the sum of 100 outcomes
se <- sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)

# Using the expected value 'avg' and standard error 'se', compute the probability that you win money betting 
# on green 100 times.
1-pnorm(0,avg,se)

################################################################################

# Exercise 2. American Roulette Monte Carlo simulation
# Create a Monte Carlo simulation that generates 10,000 outcomes of S, the sum of 100 bets.
# 
# Compute the average and standard deviation of the resulting list and compare them to 
# the expected value (-5.263158) and standard error (40.19344) for S that you calculated 
# previously.

# Use the replicate function to replicate the sample code for B <- 10000 simulations.
# Within replicate, use the sample function to simulate n <- 100 outcomes of either 
# a win (17) or a loss (-1) for the bet. Use the order c(17, -1) and corresponding 
# probabilities. Then, use the sum function to add up the winnings over all iterations 
# of the model. Make sure to include sum or DataCamp may crash with a "Session Expired" 
# error.
# Use the mean function to compute the average winnings.
# Use the sd function to compute the standard deviation of the winnings.

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 100

# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `S` that replicates the sample code for `B` iterations and sums the outcomes.
S <- replicate(B,{
  X <- sample(c(17,-1), size = n, replace = TRUE, prob = c(p_green, p_not_green))
  sum(X)
})

# Compute the average value for 'S'
mean(S)

# Calculate the standard deviation of 'S'
sd(S)

################################################################################

# Exercise 3. American Roulette Monte Carlo vs CLT
# In this chapter, you calculated the probability of winning money in American roulette 
# using the CLT.
# 
# Now, calculate the probability of winning money from the Monte Carlo simulation. 
# The Monte Carlo simulation from the previous exercise has already been pre-run for you, 
# resulting in the variable S that contains a list of 10,000 simulated outcomes.

# Use the mean function to calculate the probability of winning money from the
# Monte Carlo simulation, S.

# Calculate the proportion of outcomes in the vector `S` that exceed $0
mean(S>0)

################################################################################

# Exercise 4. American Roulette Monte Carlo vs CLT comparison
# The Monte Carlo result and the CLT approximation for the probability of losing money 
# after 100 bets are close, but not that close. What could account for this?

# Possible Answers
# 
# 10,000 simulations is not enough. If we do more, the estimates will match.
# 
# The CLT does not work as well when the probability of success is small. (True)
# 
# The difference is within rounding error.
# 
# The CLT only works for the averages.


################################################################################

# Exercise 5. American Roulette average winnings per bet
# Now create a random variable Y that contains your average winnings per bet after 
# betting on green 10,000 times.

# Run a single Monte Carlo simulation of 10,000 bets using the following steps. 
# (You do not need to replicate the sample code.)
# Specify n as the number of times you want to sample from the possible outcomes.
# Use the sample function to return n values from a vector of possible values: 
# winning $17 or losing $1. Be sure to assign a probability to each outcome and 
# indicate that you are sampling with replacement.
# Calculate the average result per bet placed using the mean function.

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Define the number of bets using the variable 'n'
n <- 10000

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green 
# pocket
p_not_green <- 1 - p_green

# Create a vector called `X` that contains the outcomes of `n` bets
X <- sample(c(17,-1), size = n, replace = TRUE, prob = c(p_green, p_not_green))

# Define a variable `Y` that contains the mean outcome per bet. Print this mean to 
# the console.
Y <- mean(X);Y

################################################################################

# Exercise 6. American Roulette per bet expected value
# What is the expected value of Y, the average outcome per bet after betting on 
# green 10,000 times?

# Using the chances of winning $17 (p_green) and the chances of losing $1 (p_not_green), 
# calculate the expected outcome of a bet that the ball will land in a green pocket.
# Use the expected value formula rather than a Monte Carlo simulation.
# Print this value to the console (do not assign it to a variable).

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in 
# a green pocket
p_not_green <- 1 - p_green

# Calculate the expected outcome of `Y`, the mean outcome per bet in 10,000 bets
Y <- p_green * 17 + p_not_green * -1;Y

################################################################################

# Exercise 7. American Roulette per bet standard error
# What is the standard error of Y, the average result of 10,000 spins?

# Compute the standard error of Y , the average result of 10,000 independent spins.

# Define the number of bets using the variable 'n'
n <- 10000

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in 
# a green pocket
p_not_green <- 1 - p_green

# Compute the standard error of 'Y', the mean outcome per bet from 10,000 bets.
abs((17 - -1))*sqrt(p_green*p_not_green) / sqrt(n)
################################################################################

# Exercise 8. American Roulette winnings per game are positive
# What is the probability that your winnings are positive after betting on green 
# 10,000 times?

# Execute the code that we wrote in previous exercises to determine the average 
# and standard error.
# Use the pnorm function to determine the probability of winning more than $0.
# We defined the average using the following code
avg <- 17*p_green + -1*p_not_green

# We defined standard error using this equation
se <- 1/sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)

# Given this average and standard error, determine the probability of winning more 
# than $0. Print the result to the console.
1 - pnorm(0, avg, se)
################################################################################
# Exercise 9. American Roulette Monte Carlo again
# Create a Monte Carlo simulation that generates 10,000 outcomes of S, the average 
# outcome from 10,000 bets on green.
# 
# Compute the average and standard deviation of the resulting list to confirm 
# the results from previous exercises using the Central Limit Theorem.

# Use the replicate function to model 10,000 iterations of a series of 10,000 bets.
# Each iteration inside replicate should simulate 10,000 bets and determine the 
# average outcome of those 10,000 bets. If you forget to take the mean, DataCamp 
# will crash with a "Session Expired" error.
# Find the average of the 10,000 average outcomes. Print this value to the console.
# Compute the standard deviation of the 10,000 simulations. Print this value to 
# the console.

## Make sure you fully follow instructions, including printing values to the console and correctly running the `replicate` loop. If not, you may encounter "Session Expired" errors.

# The variable `n` specifies the number of independent bets on green
n <- 10000

# The variable `B` specifies the number of times we want the simulation to run
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result 
# after random number generation
set.seed(1)

# Generate a vector `S` that contains the the average outcomes of 10,000 bets 
# modeled 10,000 times
S <- replicate(B,{  
  X <- sample(c(17,-1), size = n, replace = TRUE, prob = c(p_green, p_not_green))
  mean(X)
})

# Compute the average of `S`
mean(S)

# Compute the standard deviation of `S`
sd(S)

################################################################################


# Exercise 10. American Roulette comparison
# In a previous exercise, you found the probability of winning more than $0 after 
# betting on green 10,000 times using the Central Limit Theorem. Then, you used 
# a Monte Carlo simulation to model the average result of betting on green 
# 10,000 times over 10,000 simulated series of bets.
# 
# What is the probability of winning more than $0 as estimated by your 
# Monte Carlo simulation? The code to generate the vector S that contains  
# the average outcomes of 10,000 bets modeled 10,000 times has already been run for you.

# Calculate the probability of winning more than $0 in the Monte Carlo simulation 
# from the previous exercise using the mean function.
# You do not need to run another simulation: the results of the simulation are 
# in your workspace as the vector S.

# Compute the proportion of outcomes in the vector 'S' where you won more than $0
mean(S>0)


################################################################################

# 
# Exercise 11. American Roulette comparison analysis
# The Monte Carlo result and the CLT approximation are now much closer than when 
# we calculated the probability of winning for 100 bets on green. What could 
# account for this difference?

# Possible Answers
# 
# We are now computing averages instead of sums.
# 
# 10,000 Monte Carlo simulations was not enough to provide a good estimate.
# 
# The CLT works better when the sample size is larger. (True)
# 
# It is not closer. The difference is within rounding error.


################################################################################


