##########################################################################################################
# The reported_heights and heights datasets were collected from three classes taught in the Departments 
# of Computer Science and Biostatistics, as well as remotely through the Extension School. 
# The Biostatistics class was taught in 2016 along with an online version offered by the Extension School. 
# On 2016-01-25 at 8:15 AM, during one of the lectures, 
# the instructors asked student to fill in the sex and height questionnaire that populated the 
# reported_heights dataset. The online students filled out the survey during the next few days, 
# after the lecture was posted online. We can use this insight to define a variable which we will 
# call type, to denote the type of student, inclass or online.

# The code below sets up the dataset for you to analyze in the following exercises:
  
library(dslabs)
library(dplyr)
library(lubridate)
library(caret)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

dat$sex
y <- factor(dat$sex, c("Female", "Male"))
y
x <- dat$type
x
################################################################################
head(dat)
head(y)
head(x)
##########################################################################################################
# Q1
# The type column of dat indicates whether students took classes in person ("inclass") or online 
# ("online"). What proportion of the inclass group is female? What proportion of the online group 
# is female?
# Enter your answer as a percentage or decimal (eg "50%" or "0.50") to at least the hundredths place.
# 
dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))
# In class
# 0.667 
# 
# Online
# 0.378  
########################################################################################################## 
# Q2
# In the course videos, height cutoffs were used to predict sex. Instead of height, use the 
# type variable to predict sex. Assume that for each class type the students are either all male or 
# all female, based on the most prevalent sex in each class type you calculated in Q1. 
# Report the accuracy of your prediction of sex based on type. You do not need to split the data 
# into training and test sets.
# 

y_hat <- ifelse(x == "online", "Male", "Female") %>% factor(levels = levels(y))
y_hat
y_hat<-factor(y_hat)
mean(y_hat == y)

# Enter your accuracy as a percentage or decimal (eg "50%" or "0.50") to at least the hundredths place.
# 0.6333333 
# ########################################################################################################
# Q3
# Write a line of code using the table() function to show the confusion matrix between y_hat and y. 
# Use the exact format function(a, b) for your answer and do not name the columns and rows. 
# Your answer should have exactly one space. Enter the line of code below.
# 
table(y_hat, y)
##########################################################################################################
# Q4
# What is the sensitivity of this prediction? You can use the sensitivity() function from the
# caret package. Enter your answer as a percentage or decimal (eg "50%" or "0.50") to at least 
# the hundredths place.
sensitivity(y_hat, y)
# 0.3823529
########################################################################################################## 
# Q5
# What is the specificity of this prediction? You can use the specificity() function from the caret 
# package. Enter your answer as a percentage or decimal (eg "50%" or "0.50") to at least the hundredths 
# place.
# 
specificity(y_hat, y)
# 0.8414634
########################################################################################################## 
# Q6
# What is the prevalence (% of females) in the dat dataset defined above? 
# Enter your answer as a percentage or decimal (eg "50%" or "0.50") to at least the hundredths place.
prev <- mean(y == "Female")
prev

