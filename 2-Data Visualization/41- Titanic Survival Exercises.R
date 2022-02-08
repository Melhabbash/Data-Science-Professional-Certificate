# Put all your new skills together to perform exploratory data analysis on a 
# classic machine learning dataset: Titanic survival!
#   
#   Background
# The Titanic was a British ocean liner that struck an iceberg and sunk on its 
# maiden voyage in 1912 from the United Kingdom to New York. More than 1,500 of 
# the estimated 2,224 passengers and crew died in the accident, making this one 
# of the largest maritime disasters ever outside of war. The ship carried a wide 
# range of passengers of all ages and both genders, from luxury travelers in 
# first-class to immigrants in the lower classes. However, not all passengers 
# were equally likely to survive the accident. We use real data about a selection
# of 891 passengers to learn who was on the Titanic and which passengers were 
# more likely to survive.
# 
# Libraries, Options, and Data 
# Be sure that you have installed the titanic package before proceeding.
# 
# Define the titanic dataset starting from the titanic library with the following
# code:

options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

?titanic_train
str(titanic_train)

############################
# 
# Question 2: Demographics of Titanic Passengers
# 0.0/3.5 points (graded)
# Make density plots of age grouped by sex. Try experimenting with combinations 
# of faceting, alpha blending, stacking and using variable counts on the y-axis 
# to answer the following questions. Some questions may be easier to answer with 
# different versions of the density plot.
# 
# Which of the following are true?
#   Select all correct answers.

total <- titanic %>%
  filter(Sex %in% c("female","male")) %>%
  count(Sex)
total

# There were more females than males. (false)
#############################################
age40 <- titanic %>%
  filter(Age == 40 & Sex %in% c("female","male")) %>%
  count(Sex)
age40

# The count of males of age 40 was higher than the count of females of age 40.(True)
###############################################
max_age <- titanic %>%
  filter(!is.na(Age)) %>%
  group_by(Sex) %>%
  summarize(max = max(Age))
max_age
# The oldest passengers were female.(false)
###############################################

titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(x = Age, group = Sex, fill = Sex)) + 
  geom_density(alpha = 0.2, bw = 10) 

# Females and males had the same general shape of age distribution.(True)
# The age distribution was bimodal, with one mode around 25 years of age and 
# a second smaller mode around 5 years of age. (True)
# The proportion of males age 18-35 was higher than the proportion of females 
# age 18-35. (True)
# The proportion of females under age 17 was higher than the proportion of males
# under age 17. (True)

##############################################
# Question 3: QQ-plot of Age Distribution
# 1 point possible (graded)
# Use geom_qq() to make a QQ-plot of passenger age and add an identity line with 
# geom_abline(). Filter out any individuals with an age of NA first. Use the 
# following object as the dparams argument in geom_qq():

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))
params

p <- titanic %>%
  ggplot(aes(sample = Age)) +
  geom_qq(dparams = params) +
  geom_abline()
p
#############################################

# Question 4: Survival by Sex
# 0.0/2.0 points (graded)
# To answer the following questions, make barplots of the Survived and Sex 
# variables using geom_bar(). Try plotting one variable and filling by the other 
# variable. You may want to try the default plot, then try adding 
# position = position_dodge() to geom_bar() to make separate bars for each group.
# 
# You can read more about making barplots in the textbook section on ggplot2 
# geometries External link.
# 
# Which of the following are true?
#   Select all correct answers.

p <- titanic %>%
  ggplot(aes(x = Survived, fill = Sex)) +
  geom_bar(position = position_dodge())
p

# Less than half of passengers survived. (true)
# Most of the survivors were female.(true)
# Most of the males survived. (false)
# Most of the females survived. (true)

#######################################
# 
# Question 5: Survival by Age
# 0.0/3.0 points (graded)
# 
# Make a density plot of age filled by survival status. Change the y-axis to 
# count and set alpha = 0.2.
# 
# Which age group is the only group more likely to survive than die? * 0-8 * 
# 10-18 * 18-30 * 30-50 * 50-70 * 70-80
# 
# correct --> 0-8

# Which age group had the most deaths? * 0-8 * 10-18 * 18-30 * 30-50 * 50-70 * 
# 70-80
# 
# correct --> 18-30

# Which age group had the highest proportion of deaths? * 0-8 * 10-18 * 18-30 * 
# 30-50 * 50-70 * 70-80
#
# correct --> 70-80

p <- titanic %>%
  ggplot(aes(x = Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2)
p

###################################
# Question 6: Survival by Fare
# 0.0/2.5 points (graded)
# Filter the data to remove individuals who paid a fare of 0. Make a boxplot of
# fare grouped by survival status. Try a log2 transformation of fares. Add the 
# data points with jitter and alpha blending.
# 
# Which of the following are true?
#   Select all correct answers.

p <- titanic %>%
  filter(Fare > 0) %>%
  ggplot(aes(Survived, Fare)) +
  geom_boxplot(alpha = 0.2) +
  scale_y_continuous(trans = "log2") +
  geom_point(show.legend = FALSE) + 
  geom_jitter()
p 
# 
# Passengers who survived generally payed higher fares than those who did not 
# survive.(True)

# The interquartile range for fares was smaller for passengers who survived.(False)

# The median fare was lower for passengers who did not survive.(True)

# Only one individual paid a fare around $500. That individual survived.(False)

# Most individuals who paid a fare around $8 did not survive.(True)
###################################

# Question 7: Survival by Passenger Class
# 0.0/3.0 points (graded)
# The Pclass variable corresponds to the passenger class. Make three barplots. 
# For the first, make a basic barplot of passenger class filled by survival. 
# For the second, make the same barplot but use the argument 
# position = position_fill() to show relative proportions in each group instead 
# of counts. For the third, make a barplot of survival filled by passenger 
# class using position = position_fill().
# 
# You can read more about making barplots in the textbook section on ggplot2 
# geometries External link.
# 
# Which of the following are true?
#   Select all correct answers.

p <- titanic %>%
  ggplot(aes(x = Pclass, fill = Survived)) +
  geom_bar()
p


p <- titanic %>%
  ggplot(aes(x = Pclass, fill = Survived)) +
  geom_bar(position = position_fill())
p


p <- titanic %>%
  ggplot(aes(x = Survived, fill = Pclass)) +
  geom_bar(position = position_fill())
p

# There were more third class passengers than passengers in the first two 
# classes combined.(True)

# There were the fewest passengers in first class, second-most passengers in 
# second class, and most passengers in third class.(False)

# Survival proportion was highest for first class passengers, followed by 
# second class. Third-class had the lowest survival proportion.(True)

# Most passengers in first class survived. Most passengers in other classes did 
# not survive.(True)

# The majority of survivors were from first class. (Majority means over 50%.)(False)

# The majority of those who did not survive were from third class.(True)

############################
# Question 8: Survival by Age, Sex and Passenger Class
# 0.0/2.5 points (graded)
# Create a grid of density plots for age, filled by survival status, with count 
# on the y-axis, faceted by sex and passenger class.
# 
# Which of the following are true?
#   Select all correct answers.
p <- titanic %>%
  ggplot(aes(x = Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2) +
  facet_grid(Pclass ~ Sex)
p

# The largest group of passengers was third-class males.(True)
# The age distribution is the same across passenger classes.(False)
# The gender distribution is the same across passenger classes.(False)
# Most first-class and second-class females survived.(True)
# Almost all second-class males did not survive, with the exception 
# of children.(True)