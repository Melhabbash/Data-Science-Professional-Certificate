#Comprehension Check: Generative Models
# --------------------------------------------------------------------------------
#
# Generative Models - comprehension check
#
# --------------------------------------------------------------------------------
# In the following exercises, we are going to apply LDA and QDA to the tissue_gene_expression dataset
# from dslabs. We will start with simple examples based on this dataset and then develop a realistic 
# example.

library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(Lahman)
library(HistData)
library(caret)
library(e1071)
library(matrixStats)

#########################################################################################################
# Q1
# Create a dataset of samples from just cerebellum and hippocampus, two parts of the brain, and 
# a predictor matrix with 10 randomly selected columns using the following code:
# Setup
library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

# set.seed(1993) #if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
 
# Use the train() function to estimate the accuracy of LDA. For this question, 
# use the version of x and y created with the code above: do not split them or tissue_gene_expression 
# into training and test sets (understand this can lead to overfitting).
# Report the accuracy from the train() results (do not make predictions).
# 
# What is the accuracy? Enter your answer as a percentage or decimal (eg "50%" or "0.50") to at 
# least the thousandths place.
set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
train_lda <- train(x,y,method="lda", data = x)
train_lda

# Linear Discriminant Analysis 
# 
# 69 samples
# 10 predictors
# 2 classes: 'cerebellum', 'hippocampus' 
# 
# No pre-processing
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 69, 69, 69, 69, 69, 69, ... 
# Resampling results:
#      
#  Accuracy      Kappa    
# 0.8707879  0.7358585

#########################################################################################################
# Q2
# In this case, LDA fits two 10-dimensional normal distributions. Look at the
# fitted model by looking at the finalModel component of the result of train.
# Notice there is a component called means that includes the estimated means of
# both distributions. Plot the mean vectors against each other and determine which predictors 
# (genes) appear to be driving the algorithm.

# Which TWO genes appear to be driving the algorithm (i.e. the two genes with the highest means)?
# PLCB1
# (True) RAB1B
# MSH4
# (True) OAZ2
# SPI1
# SAPCD1
# HEMK1

head(train_lda$finalModel)

means <- data.frame(t(train_lda$finalModel$means)) 
means <- means %>% mutate(gene = as.factor(rownames(means)))

means %>% ggplot(aes(x = cerebellum, y = hippocampus, colour = gene, label = gene)) +
  ggtitle("LDA Means - Cerebellum vs Hippocampus") +
  geom_point() +
  geom_text_repel(aes(label=gene)) +
  theme(legend.position="none")  

# RAB1B
# OAZ2
#########################################################################################################
# Q3
# Repeat the exercise in Q1 with QDA. Create a dataset of samples from just
# cerebellum and hippocampus, two parts of the brain, and a predictor matrix
# with 10 randomly selected columns using the following code:

library(dslabs)      
library(caret)
data("tissue_gene_expression")

set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

# Use the train function to estimate the accuracy of QDA.
# What is the accuracy?

train_qda <- train(x,y,method="qda", data = x)
train_qda

#0.815

# Explanation
# The following code can be used to estimate the accuracy of QDA:
  
fit_qda <- train(x, y, method = "qda")
fit_qda$results["Accuracy"]


#########################################################################################################
#Q4
#Which TWO genes drive the algorithm when using QDA instead of LDA?

# PLCB1
# (True) RAB1B
# MSH4
# (True) OAZ2
# SPI1
# SAPCD1
# HEMK1

t(fit_qda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

#Answer: RAB1B & OAZ2

#########################################################################################################
#  Q5
#  One thing we saw in the previous plots is that the values of the predictors correlate in both 
# groups: some predictors are low in both groups and others high in both groups. 
# The mean value of each predictor found in colMeans(x) is not informative or useful for prediction 
# and often for purposes of interpretation, it is useful to center or scale each column. 
# This can be achieved with the preProcess argument in train. 
# Re-run LDA with preProcess = "center". Note that accuracy does not change, but it is now easier 
# to identify the predictors that differ more between groups than based on the plot made in Q2.
#  Which TWO genes drive the algorithm after performing the scaling?  

# Which TWO genes drive the algorithm after performing the scaling? OAZ2 & 

# C21orf62
# PLCB1
# RAB1B
# MSH4
# (True) OAZ2
# (True) SPI1
# SAPCD1
# IL18R1


# Explanation
# The following code can be used to make the plot to evaluate which genes are driving the algorithm 
# after scaling:
  
fit_lda <- train(x, y, method = "lda", preProcess = "center")
fit_lda$results["Accuracy"]
t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(predictor_name, hippocampus)) +
  geom_point() +
  coord_flip()

# You can see that it is different genes driving the algorithm now. This is because the predictor means 
# change.
# In the previous exercises we saw that both LDA and QDA approaches worked well. 
# For further exploration of the data, you can plot the predictor values for the two genes with the 
# largest differences between the two groups in a scatter plot to see how they appear to follow 
# a bivariate distribution as assumed by the LDA and QDA approaches, coloring the points by 
# the outcome, using the following code:
  
d <- apply(fit_lda$finalModel$means, 2, diff)
ind <- order(abs(d), decreasing = TRUE)[1:2]
plot(x[, ind], col = y)

#########################################################################################################
#  Q6
# Now we are going to increase the complexity of the challenge slightly. Repeat the LDA analysis 
# from Q5 but using all tissue types. Use the following code to create your dataset:
library(dslabs)      
library(caret)
data("tissue_gene_expression")

# set.seed(1993) # if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

#  What is the accuracy using LDA?
#  Answer:0.819

# Explanation
# The following code can be used to obtain the accuracy of the LDA:
  
fit_lda <- train(x, y, method = "lda", preProcess = c("center"))
fit_lda$results["Accuracy"]

# We see that the results are slightly worse when looking at all of the tissue types instead of only 
# selected ones. You can use the confusionMatrix function to learn more about what type of errors 
# we are making, like this: confusionMatrix(fit_lda).
