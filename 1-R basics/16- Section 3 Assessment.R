
library(dslabs)
data(heights)
options(digits = 3)    # report 3 significant digits for all answers
str(heights)
heights$height
heights$sex
#Question 1
#1 point possible (graded)
#First, determine the average height in this dataset.
avg<- mean(heights$height)
#Then create a logical vector ind with the indices for those individuals who are 
#above average height.
ind<-heights$height>avg
ind
#How many individuals in the dataset are above average height?
sum(ind)

#Question 2
#1 point possible (graded)
#How many individuals in the dataset are above average height and are female?
Female<-heights$sex=="Female"
sum(ind&Female)

#Question 3
#1 point possible (graded)
#If you use mean() on a logical (TRUE/FALSE) vector, it returns the proportion 
#of observations that are TRUE.
mean(ind)
#What proportion of individuals in the dataset are female?
# Report 3 significant digits.
mean(heights$sex=="Female")

#Question 4
#This question takes you through three steps to determine the sex of the 
#individual with the minimum height.

#Question 4a
#1 point possible (graded)
#Determine the minimum height in the heights dataset.
ind<-which.min(heights$height)
ind
min<-heights$height[ind]
#or
min<-heights$height[which.min(heights$height)]
min

#Question 4b
#1 point possible (graded)
#Use the match() function to determine the index of the first individual with 
#the minimum height.
ind_1<- match(min, heights$height)
ind_1

#Question 4c
#1 point possible (graded)
#Subset the sex column of the dataset by the index in 4b to determine the 
#individual's sex.
heights$sex[ind]


#Question 5
#This question takes you through three steps to determine how many of the 
#integer height values between the minimum and maximum heights are not actual 
#heights of individuals in the heights dataset.

#Question 5a
#1 point possible (graded)
#Determine the maximum height.
#Report 3 significant digits.
ind<-which.max(heights$height)
max<-heights$height[ind]
max
max<-heights$height[which.max(heights$height)]
max

#Question 5b
#1 point possible (graded)
#Which integer values are between the maximum and minimum heights? 
#For example, if the minimum height is 10.2 and the maximum height is 20.8, 
#your answer should be x <- 11:20 to capture the integers in between those values. (If either the maximum or minimum height are integers, include those values too.)

#Write code to create a vector x that includes the integers between the minimum 
#and maximum heights (as numbers).
#There are multiple ways to solve this problem, but the grader expects you 
#to use the format in the problem description. 
#Your answer should have numbers and a colon (:), 
#and it should not use other functions.
min
max
min:max
x<-50:82
x
length(x)
#Question 5c
#1 point possible (graded)
#How many of the integers in x are NOT heights in the dataset?
 # Use the sum() and %in% functions in addition to the ! operator.

sum(!(x %in% heights$height))


#Question 6
#Using the heights dataset, create a new column of heights in centimeters 
#named ht_cm. 
#Recall that 1 inch = 2.54 centimeters. Save the resulting dataset as heights2.
heights<-mutate(heights,ht_cm=height*2.54)
head(heights)

#Question 6a
#1 point possible (graded)
#What is the height in centimeters of the 18th individual (index 18)?
heights$ht_cm[18]

#Question 6b
#1 point possible (graded)
#What is the mean height in centimeters?
mean(heights$ht_cm)
mean(heights$height)*2.54

#Create a data frame females by filtering the heights2 data to contain only 
#female individuals.
heights2 <- data.frame(filter(heights,heights$sex=="Female"))
heights2

#Question 7a
#1 point possible (graded)
#How many females are in the heights2 dataset?
str(heights2)
sum(heights$sex=="Female")

#Question 7b
#1 point possible (graded)
#What is the mean height of the females in centimeters?
mean(heights2$ht_cm)

#Question 8
#1 point possible (graded)
#The olive dataset in dslabs contains composition in percentage of eight fatty 
#acids found in the lipid fraction of 572 Italian olive oils:
library(dslabs)
data(olive)
head(olive)

#Plot the percent palmitic acid versus palmitoleic acid in a scatterplot. 
#What relationship do you see?
  
plot(olive$palmitic,olive$palmitoleic)

#Question 9
#1 point possible (graded)
#Create a histogram of the percentage of eicosenoic acid in olive.
#Which of the following is true?
hist(olive$eicosenoic)

#Question 10
#2 points possible (graded)
#Make a boxplot of palmitic acid percentage in olive with separate distributions for each region.

#Which region has the highest median palmitic acid percentage?  
boxplot(palmitic~region, data = olive)
