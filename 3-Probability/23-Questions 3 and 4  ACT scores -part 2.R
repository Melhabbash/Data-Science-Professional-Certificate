
# In this 3-part question, you will convert raw ACT scores to Z-scores and answer some questions about them.
# Convert act_scores to Z-scores. Recall from Data Visualization (the second course in this series) that 
# to standardize values (convert values into Z-scores, that is, values distributed with a mean of 0 and 
# standard deviation of 1), you must subtract the mean and then divide by the standard deviation. 
# Use the mean and standard deviation of act_scores, not the original values used to generate random test scores.

# Question 3a
# What is the probability of a Z-score greater than 2 (2 standard deviations above the mean)?

# set seed to 16
set.seed(16)

# generate a random normal distribution of 10,000 tests
# mean 20.9 and sd of 5.7
act_scores <- rnorm(10000,mean=20.9, sd=5.7)
avg_act <- mean(act_scores)
sd_act <- sd(act_scores)

# scaling act_scores
z_act_scores <- scale(act_scores)

# probability of z >= 2
mean(z_act_scores>=2)

# Question 3b
# What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?
# z=(x-mu)/segma
# x=z*mu+mu
x=avg_act + 2*sd_act
x

# Question 3c
# A Z-score of 2 corresponds roughly to the 97.5th percentile.
# Use qnorm() to determine the 97.5th percentile of normally distributed data with the mean and 
# standard deviation observed in act_scores.
# What is the 97.5th percentile of act_scores?
qnorm(0.975,avg_act,sd_act)

####################################################

# In this 4-part question, you will write a function to create a CDF for ACT scores.
# 
# Write a function that takes a value and produces the probability of an ACT score less than or equal to 
# that value (the CDF). Apply this function to the range 1 to 36.

# Question 4a
# What is the minimum integer score such that the probability of that score or lower is at least .95?
#   Your answer should be an integer 1-36.
qnorm(0.95,avg_act,sd_act)

# The answer is 31


# Question 4b
# Use qnorm() to determine the expected 95th percentile, the value for which the probability of receiving 
# that score or lower is 0.95, given a mean score of 20.9 and standard deviation of 5.7.
# What is the expected 95th percentile of ACT scores?
qnorm(0.95,20.9,5.7)



# Question 4c
# As discussed in the Data Visualization course, we can use quantile() to determine sample quantiles from the data.
# 
# Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01), the 1st through 99th percentiles of the 
# act_scores data. Save these as sample_quantiles.
# 
# In what percentile is a score of 26?
# Your answer should be an integer (i.e. 60), not a percent or fraction. Note that a score between the 98th 
# and 99th percentile should be considered the 98th percentile, for example, and that quantile numbers are used 
# as names for the vector sample_quantiles.

p <- seq(0.01, 0.99, 0.01)
sample_quantiles<-quantile(act_scores,p)
max(which(sample_quantiles < 26))
names(sample_quantiles[max(which(sample_quantiles < 26))])

# to determine percentile of a score of 26
names(sample_quantiles[max(which(sample_quantiles < 26))])

# which(s...<26) returns a vector with all percentiles for scores 26 or lower
which(sample_quantiles<26)

# doing max(which(s...< 26)) we isolate the percentile for score 26
max(which(sample_quantiles < 26))

# sample quantile(...) returns the tibble
sample_quantiles[max(which(sample_quantiles < 26))]

# in order to know the value __% we need to use names (like getting the column name)
names(sample_quantiles[max(which(sample_quantiles < 26))])


# 
# Question 4d
# Make a corresponding set of theoretical quantiles using qnorm() over the interval p <- seq(0.01, 0.99, 0.01) 
# with mean 20.9 and standard deviation 5.7. Save these as theoretical_quantiles. Make a QQ-plot graphing 
# sample_quantiles on the y-axis versus theoretical_quantiles on the x-axis.
# Which of the following graphs is correct?

# recap, sample_quantiles <- quantile(act_scores, p)
# now we calculate theoretical qs

p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p, 20.9, 5.7)

# to qqplot
qqplot(theoretical_quantiles, sample_quantiles)
