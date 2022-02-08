###################################################################################################
#install.packages(c("tidyverse","dslabs","caret","matrixStats","randomForest","lubridate"))
library(tidyverse)
library(caret)
library(dslabs)
library(matrixStats)
library(rpart)
library(randomForest)
library(lubridate)
library(RColorBrewer)

###################################################################################################
#The brca dataset from the dslabs package contains information about breast cancer diagnosis 
#biopsy samples for tumors that were determined to be either benign (not cancer) and malignant 
#(cancer). The brca object is a list consisting of

#1) brca$y: a vector of sample classifications ("B" = benign or "M" = malignant)
#2) brca$x: a matrix of numeric features describing properties of the shape and size of cell 
#nuclei extracted from biopsy microscope images

# For these exercises, load the data by setting your options and loading the libraries and data 
# as shown in the code here:
options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)
# 
# The exercises in this assessment are available to Verified Learners only and are split into 
# four parts, all of which use the data described here.
# 
# IMPORTANT: Some of these exercises use dslabs datasets that were added in a July 2019 update. 
# Make sure your package is up to date with the command install.packages("dslabs").
###################################################################################################

#Question 1: Dimensions and properties
#dimensions and properties of the dataset
#How many samples are in the dataset? 569
length(brca$y)
dim(brca$x)
nrow(brca$x)
#How many predictors are in the matrix? 30
ncol(brca$x)
#What proportion of the samples are malignant? 0.373
mean(brca$y == "M")
#Which column number has the highest mean? 24
which.max(colMeans(brca$x))
#Which column number has the lowest standard deviation?
which.min(colSds(brca$x))
view(brca$x)
###################################################################################################

#Question 2: Scaling the matrix
#Use sweep() two times to scale each column: subtract the column means of brca$x, then divide 
#by the column standard deviations of brca$x
x_centered <- sweep(brca$x, 2, colMeans(brca$x))
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN = "/")

#After scaling, what is the standard deviation of the first column? 1
sd(x_scaled[,1])
#After scaling, what is the median value of the first column? -0.215
median(x_scaled[,1])
###################################################################################################
#Question 3: Distance
#Calculate the distance between all samples using the scaled matrix.
d_samples <- dist(x_scaled)

#What is the average distance between the first sample, which is benign, and other 
# benign samples?  4.41
dist_BtoB <- as.matrix(d_samples)[1, brca$y == "B"]
mean(dist_BtoB[2:length(dist_BtoB)])
#What is the average distance between the first sample and malignant samples? 7.12
dist_BtoM <- as.matrix(d_samples)[1, brca$y == "M"]
mean(dist_BtoM)
###################################################################################################

#Question 4: Heatmap of features
#Make a heatmap of the relationship between features using the scaled matrix
# Which of these heatmaps is correct?
# To remove column and row labels like the images below, use labRow = NA and labCol = NA

# (True) 1st plot
# 2nd plot
# 3rd plot
# 4th plot

d_features <- dist(t(x_scaled))
heatmap(as.matrix(d_features), labRow = NA, labCol = NA)

###################################################################################################
#Question 5: Hierarchical clustering
#Perform hierarchical clustering on the 30 features. Cut the tree into 5 groups.
# All but one of the answer options are in the same group.
# Which is in a different group?

# smoothness_mean
# smoothness_worst
# compactness_mean
# compactness_worst
# (True) concavity_mean
# concavity_worst
  
h <- hclust(d_features)
groups <- cutree(h, k = 5)
split(names(groups), groups)
