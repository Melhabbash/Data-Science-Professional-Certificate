# Exercise 1.Confidence interval for p
# For the following exercises, we will use actual poll data from the 2016 election. The exercises will contain 
# pre-loaded data from the dslabs package.

library(dslabs)
library(dplyr)
library(ggplot2)
data(polls_us_election_2016)

# We will use all the national polls that ended within a few weeks before the election.
# Assume there are only two candidates and construct a 95% confidence interval for the election night proportion p.
# 
# Instructions
# 
# Use filter to subset the data set for the poll data you want. Include polls that ended on or after
# October 31, 2016 (enddate). Only include polls that took place in the United States. Call this filtered 
# object polls.
# Use nrow to make sure you created a filtered object polls that contains the correct number of rows.
# Extract the sample size Nfrom the first poll in your subset object polls.
# Convert the percentage of Clinton voters (rawpoll_clinton) from the first poll in polls to a proportion, X_hat. 
# Print this value to the console.
# Find the standard error of X_hat given N. Print this result to the console.
# Calculate the 95% confidence interval of this estimate using the qnorm function.
# Save the lower and upper confidence intervals as an object called ci. Save the lower confidence interval first.

# Load the data
data(polls_us_election_2016)

# Generate an object `polls` that contains data filtered for polls that ended on or after October 31, 2016 in 
# the United States
polls <- filter(polls_us_election_2016, enddate >= "2016-10-31" & state == "U.S.")

# How many rows does `polls` contain? Print this value to the console.
nrow(polls)

# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- head(polls$samplesize,1)
N

# For the first poll in `polls`, assign the estimated percentage of Clinton voters to a variable called `X_hat`. 
# Print this value to the console.
X_hat <- (head(polls$rawpoll_clinton,1)/100)
X_hat

# Calculate the standard error of `X_hat` and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(X_hat*(1-X_hat)/N)
se_hat

# Use `qnorm` to calculate the 95% confidence interval for the proportion of Clinton voters. Save the lower 
# and then the upper confidence interval to a variable called `ci`.
qnorm(0.975)
ci <- c(X_hat - qnorm(0.975)*se_hat, X_hat + qnorm(0.975)*se_hat)
ci
