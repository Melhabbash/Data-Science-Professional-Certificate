# We have investigated the relationship between fathers' heights and sons' heights. 
# But what about other parent-child relationships? Does one parent's height have a stronger 
# association with child height? How does the child's gender affect this relationship in heights? 
# Are any differences that we observe statistically significant?
  
# The galton dataset is a sample of one male and one female child from each family in the 
# GaltonFamilies dataset. The pair column denotes whether the pair is father and daughter, 
# father and son, mother and daughter, or mother and son.

# Create the galton dataset using the code below:
  
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton

###################################################################################################
# Question 8
# 
# Group by pair and summarize the number of observations in each group.
galton %>% group_by(pair) %>%
  summarize(n = n())
# How many father-daughter pairs are in the dataset?
# 176
# How many mother-son pairs are in the dataset?
# 179
#################################################################################################
# Question 9
# 
# Calculate the correlation coefficients for fathers and daughters, fathers and sons, mothers 
# and daughters and mothers and sons.
# 
galton %>%
  group_by(pair) %>%
  summarize(cor = cor(parentHeight, childHeight)) %>%
  filter(cor == max(cor))
# Which pair has the strongest correlation in heights?
#   fathers and daughters
#(True)fathers and sons
#   mothers and daughters
#   mothers and sons
# 
galton %>%
  group_by(pair) %>%
  summarize(cor = cor(parentHeight, childHeight)) %>%
  filter(cor == min(cor))
# Which pair has the weakest correlation in heights?
#   fathers and daughters
#   fathers and sons
#   mothers and daughters
#(True)mothers and sons

#################################################################################################
# Question 10 has two parts. The information here applies to both parts.
# 
# Use lm() and the broom package to fit regression lines for each parent-child pair type. 
# Compute the least squares estimates, standard errors, confidence intervals and p-values 
# for the parentHeight coefficient for each pair.

##############
# Question 10a
library(broom)
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight", pair == "father_daughter") %>%
  pull(estimate)

# What is the estimate of the father-daughter coefficient?
# 0.345  

galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight", pair == "mother_son") %>%
  pull(estimate)
# For every 1-inch increase in mother's height, how many inches does the typical son's height 
# increase?
# 0.381
###############
# Question 10b
galton %>% 
group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight")
# Which sets of parent-child heights are significantly correlated at a p-value cut off of .05?
#   Select ALL that apply.
# 
#(True)father-daughter
#(True)father-son
#(True)mother-daughter
#(True)mother-son

# Explanation
# All of the parent-child heights are correlated with a p-value of <0.05.
###########################
# When considering the estimates, which of the following statements are true?
#   Select ALL that apply.
# 
#(True)All of the confidence intervals overlap each other.
#   At least one confidence interval covers zero.
#(True)The confidence intervals involving mothers' heights are larger than the confidence intervals 
#   involving fathers' heights.
#   The confidence intervals involving daughters' heights are larger than the confidence intervals 
#   involving sons' heights.
#(True)The data are consistent with inheritance of height being independent of the child's gender.
#(True)The data are consistent with inheritance of height being independent of the parent's gender.

# Explanation
# The following code can be used to answer both questions:
  
  galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight" & p.value < .05)

# All four of the confidence intervals overlap. The confidence intervals for mothers' heights 
# are larger than those for fathers' heights, as observed from the standard errors.
# Because the confidence intervals overlap, the data are consistent with inheritance of height 
# being independent of the child's or the parent's gender.

