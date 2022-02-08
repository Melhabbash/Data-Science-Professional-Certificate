#################################################################################################
# Question 1
# 
# In the videos, we ran one million tests of correlation for two random variables, X and Y.
# 
# How many of these correlations would you expect to have a significant p-value (), just by chance?
#   5,000
#(True)50,000
#   100,000
#   It's impossible to know
# 
# 
# :. In this example, the chance of finding a correlation when none exists is 0.05*1,000,000 
# chances.

# Explanation
# 
# The p-value is defined as the probability of finding the observed result when the null hypothesis
# (no correlation) is true. When we have a p-value of 0.05, this means the chance of finding 
# a correlation when none exists is 5% - e.g., 0.05*1,000,000 chances, which is 50,000.
# You have used 1 of 2 attemptsSome problems have options such as save, reset, hints, or  .
# These options follow the  button.s are displayed within the problemReview

#################################################################################################
# Question 2
# 
# Which of the following are examples of p-hacking?
# Select ALL that apply.
# 
#(True) Looking for associations between an outcome and several exposures and only reporting the one that is significant.
#(True) Trying several different models and selecting the one that yields the smallest p-value.
#(True) Repeating an experiment multiple times and only reporting the one with the smallest p-value.
#   Using a Monte Carlo simulations in an analysis.


# Explanation
# 
# Repeating an experiment multiple times and only reporting the one with the smallest p-value, 
# looking for associations between an outcome and several exposures and only reporting the one 
# that is significant, and trying several different models and selecting the one that yields 
# the smallest p-value are all examples of p-hacking.
# 
# Monte Carlo simulations do not necessarily lead to multiple testing problems such as p-hacking 
# in and of themselves.

#################################################################################################
# Question 3

# The Spearman correlation coefficient is robust to outliers because:
#   It drops outliers before calculating correlation.
#   It is the correlation of standardized values.
#(True) It calculates correlation between ranks, not values.

# Explanation
# Because the Spearman correlation coefficient uses ranks instead of values to calculate 
# correlation, it is more robust to outliers.

 #################################################################################################
# Question 4
# 
# What can you do to determine if you are misinterpreting results because of a confounder?
#   Nothing. If the p-value says the result is significant, then it is.
#(True)More closely examine the results by stratifying and plotting the data.
#   Always assume that you are misinterpreting the results.
#   Use linear models to tease out a confounder.


# Although you can sometimes use linear models, you can't always and exploratory data analysis 
# (stratifying and plotting data) will help determine if there is a confounder.
# Explanation
# 
# Exploratory data analysis (stratifying and plotting data) can help determine if there is 
# a confounder. Linear models cannot be used in all situations.

#################################################################################################
# Question 5
# 
# Look again at the admissions data presented in the confounders video using ?admissions.
# 
# What important characteristic of the table variables do you need to know to understand 
# the calculations used in this video?
#   The data are from 1973.
#   The columns major and gender are of class character, while admitted and applicants are numeric.
#   The data are from the dslabs package.
#(True)The column admitted is the percent of students admitted, while the column applicants 
#   is the total number of applicants.
# 
# In all data science projects, it is important to understand the data that you are working with.
# Explanation
# 
# Several of these statements are true but not relevant to understanding the calculations 
# in the video. The only statement that is critical for the analysis is that 
# "The column admitted is the percent of students admitted, while the column applicants is 
# the total number of applicants." In all data science projects, it is important to 
# understand the data that you are working with.

# #################################################################################################
# Question 6
# 
# In the example in the confounders video, major selectivity confounds the relationship between 
# UC Berkeley admission rates and gender because:
#   It was harder for women to be admitted to UC Berkeley.
#(True)Major selectivity is associated with both admission rates and with gender, as women tended 
#     to apply to more selective majors.
#   Some majors are more selective than others.
#   Major selectivity is not a confounder.
# 
# Explanation
# 
# Major selectivity is a confounder because it is associated with both admission rate and 
# with gender.

#################################################################################################
# Question 7
# 
# Admission rates at UC Berkeley are an example of Simpson's Paradox because:
#   It appears that men have higher a higher admission rate than women, however, after we 
#     stratify by major, we see that on average women have a higher admission rate than men.
#   It was a paradox that women were being admitted at a lower rate than men.
#   The relationship between admissions and gender is confounded by major selectivity.
# 
# 
# this is a good explanation of why this example is considered an example of Simpson's Paradox
# Explanation
# 
# Simpson's Paradox refers specifically to cases where the sign of the correlation flips when 
# comparing the entire dataset vs. specific strata, so only the first statement is .

 