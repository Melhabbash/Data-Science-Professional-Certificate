# The ACT is a standardized college admissions test used in the United States. The four multi-part questions in this 
# assessment all involve simulating some ACT test scores and answering probability questions about them.
# 
# For the three year period 2016-2018, ACT standardized test scores were approximately normally distributed with 
# a mean of 20.9 and standard deviation of 5.7. (Real ACT scores are integers between 1 and 36, but we will ignore 
# this detail and use continuous values instead.)
# 
# First we'll simulate an ACT test score dataset and answer some questions about it.
# 
# Set the seed to 16, then use rnorm() to generate a normal distribution of 10000 tests with a mean of 20.9 
# and standard deviation of 5.7. Save these values as act_scores. You'll be using this dataset throughout these 
# four multi-part questions.
# 
# (IMPORTANT NOTE! If you use R 3.6 or later, you will need to use the command format 
# set.seed(x, sample.kind = "Rounding") instead of set.seed(x). Your R version will be printed at the top of the 
# Console window when you start RStudio.)

# set seed to 16
set.seed(16)

# generate a random normal distribution of 10,000 tests
# mean 20.9 and sd of 5.7

act_scores <- rnorm(10000,mean=20.9, sd=5.7)

# Question 1a
# What is the mean of act_scores?
avg_act<-mean(act_scores)
avg_act

# Question 1b
# What is the standard deviation of act_scores?
sd_act <-sd(act_scores)
sd_act 

# Question 1c
# A perfect score is 36 or greater (the maximum reported score is 36).
# 
# In act_scores, how many perfect scores are there out of 10,000 simulated tests?
mean(act_scores>=36)*10000

# Question 1d
# In act_scores, what is the probability of an ACT score greater than 30?
1-pnorm(30,avg_act,sd_act)

# Question 1e
# In act_scores, what is the probability of an ACT score less than or equal to 10?
pnorm(10,avg_act,sd_act)

##########################################################

# Question 2
# 1 point possible (graded)
# Set x equal to the sequence of integers 1 to 36. Use dnorm to determine the value of the probability density 
# function over x given a mean of 20.9 and standard deviation of 5.7; save the result as f_x. Plot x against f_x.
# 
# Which of the following plots is correct?
x<-seq(1,36,1)
f_x<-dnorm(x,20.9,5.7)
plot(x,f_x)
