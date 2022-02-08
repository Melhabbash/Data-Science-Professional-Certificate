# Titanic Exercises, part 2

library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded 
# with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), 
                      median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, 
         SibSp, Parch, FamilySize, Embarked)

set.seed(42, sample.kind = "Rounding")

idx <- createDataPartition(titanic_clean$Survived,
                           times = 1, 
                           p = .2, 
                           list = F)
test_set <- titanic_clean[idx,]
train_set <- titanic_clean[-idx,]
###########################################################
# Question 7: Survival by fare - LDA and QDA
# 
# Set the seed to 1. Train a model using linear discriminant analysis (LDA) with the caret lda method 
# using fare as the only predictor.
# 
# What is the accuracy on the test set for the LDA model?
# 0.693 
set.seed(1, sample.kind = "Rounding")
train_lda <- 
  train(Survived ~ Fare, method = "lda", data = train_set)
y_hat <- predict(train_lda, test_set)
confusionMatrix(data = y_hat, 
                reference = test_set$Survived)$overall["Accuracy"]
############################
# Set the seed to 1. Train a model using quadratic discriminant analysis (QDA) with the caret qda 
# method using fare as the only predictor.
# What is the accuracy on the test set for the QDA model?
#    
# 0.693 
set.seed(1, sample.kind = "Rounding")
train_qda <- 
  train(Survived ~ Fare, method = "qda", data = train_set)
y_hat <- predict(train_qda, test_set)
confusionMatrix(data = y_hat, 
                reference = test_set$Survived)$overall["Accuracy"]

# Note: when training models for Titanic Exercises Part 2, please use the S3 method for class formula
# rather than the default S3 method of caret train() (see ?caret::train for details).
 
########################################################################################################## 
# Question 8: Logistic regression models
######################################################################################### 
# Set the seed to 1. Train a logistic regression model with the caret glm method using age as the only 
# predictor.
# What is the accuracy of your model (using age as the only predictor) on the test set ?
# 0.615 

# Explanation# 
# The accuracy can be determined using the following code:
#set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
train_glm_age <- train(Survived ~ Age, method = "glm", data = train_set)
glm_preds_age <- predict(train_glm_age, test_set)
mean(glm_preds_age == test_set$Survived)
####################################################################################################### 
# Set the seed to 1. Train a logistic regression model with the caret glm method using four predictors: 
# sex, class, fare, and age.

# What is the accuracy of your model (using these four predictors) on the test set?
#0.849     

# Explanation
# The accuracy can be determined using the following code:
#set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
train_glm <- train(Survived ~ Sex + Pclass + Fare + Age, method = "glm", data = train_set)
glm_preds <- predict(train_glm, test_set)
mean(glm_preds == test_set$Survived)
####################################################################################################
# Set the seed to 1. Train a logistic regression model with the caret glm method using all predictors.
# Ignore warnings about rank-deficient fit.
#
# What is the accuracy of your model (using all predictors) on the test set ?
# 0.849 

# Explanation
# The accuracy can be determined using the following code:
#set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") if using R 3.6 or later
train_glm_all <- train(Survived ~ ., method = "glm", data = train_set)
glm_all_preds <- predict(train_glm_all, test_set)
mean(glm_all_preds == test_set$Survived)
# #######################################################################################################
# Question 9a: kNN model
# 
# Set the seed to 6. Train a kNN model on the training set using the caret train function. Try tuning 
# with k = seq(3, 51, 2).
# 
# What is the optimal value of the number of neighbors k?    
#
#set.seed(6)
set.seed(6, sample.kind = "Rounding") # if using R 3.6 or later
train_knn <- train(Survived ~ .,
                   method = "knn",
                   data = train_set,
                   tuneGrid = data.frame(k = seq(3, 51, 2)))
train_knn$bestTune
# 
######################################################################################################### 
# Question 9b: kNN model
# 
# Plot the kNN model to investigate the relationship between the number of neighbors and accuracy on 
# the training set.
# 
# Of these values of k, which yields the highest accuracy?
# 7
# (True) 11
# 17
# 21

train_knn$results %>%
  ggplot(aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(
    x = k,
    ymin = Accuracy - AccuracySD,
    ymax = Accuracy + AccuracySD
  ))

max(train_knn$results$Accuracy)

plot(train_knn$results$k, train_knn$results$Accuracy)
######################################################################################################### 
# Question 9c: kNN model
# 
# What is the accuracy of the kNN model on the test set?
#    
#0.709

# Explanation
# The accuracy can be calculated using the following code:
  knn_preds <- predict(train_knn, test_set)
mean(knn_preds == test_set$Survived)# 
######################################################################################################### 
# Question 10: Cross-validation
# 
# Set the seed to 8 and train a new kNN model. Instead of the default training control, use 10-fold 
# cross-validation where each partition consists of 10% of the total. Try tuning with k = seq(3, 51, 2).
# 
# What is the optimal value of k using cross-validation?
#  5

#set.seed(8)
set.seed(8, sample.kind = "Rounding")    # simulate R 3.5
train_knn_cv <- train(Survived ~ .,
                      method = "knn",
                      data = train_set,
                      tuneGrid = data.frame(k = seq(3, 51, 2)),
                      trControl = trainControl(method = "cv", number = 10, p = 0.9))
train_knn_cv$bestTune

###########################################################################
# What is the accuracy on the test set using the cross-validated kNN model?
#0.648 

y_hat <- predict(train_knn, test_set)
confusionMatrix(data = y_hat, 
                reference = test_set$Survived)$overall["Accuracy"] 
######################################################################################################### 
# Question 11a: Classification tree model
# 
# Set the seed to 10. Use caret to train a decision tree with the rpart method. Tune the complexity 
# parameter with cp = seq(0, 0.05, 0.002).
set.seed(10, sample.kind = "Rounding")    # simulate R 3.5
train_rpart <- train(Survived ~ .,
                     method = "rpart",
                     data = train_set,
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002))
)
#############################################################
# What is the optimal value of the complexity parameter (cp)?
#    
# 0.028

# Explanation
# The optimal value of cp can be found using the following code:

#set.seed(10)
set.seed(10, sample.kind = "Rounding")    # simulate R 3.5
train_rpart <- train(Survived ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                     data = train_set)
train_rpart$bestTune

##################################################################
# What is the accuracy of the decision tree model on the test set?
#    
# 0.838 

# Explanation
# The accuracy can be calculated using the following code:
rpart_preds <- predict(train_rpart, test_set)
mean(rpart_preds == test_set$Survived)# 
######################################################################################################### 
# Question 11b: Classification tree model
# 
# Inspect the final model and plot the decision tree.
# 
# Which variables are used in the decision tree?
#   Select ALL that apply.
# 
# Survived
# (True)Sex
# (True)Pclass
# (True)Age
# (True)Fare
# Parch
# Embarked

# Explanation
# The decision tree can be made using the following code:
  
train_rpart$finalModel # inspect final model
# make plot of decision tree
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel)

####################################### 
mean(test_set$Survived[test_set$Sex == "male"] == 1)
mean(test_set$Survived[test_set$Sex == "male" &
                         test_set$Age < 3.5] == 1)
mean(test_set$Survived[test_set$Sex == "female"] == 1)

# 
######################################################################################################### 
# Question 11c: Classification tree model
# 
# Using the decision rules generated by the final model, predict whether the following individuals
# would survive.
# 
# A 28-year-old male
mean(test_set$Survived[test_set$Sex == "male" & 
                         test_set$Age >= 3.5] == 1)
# 0.145
# correct  would NOT survive
########################################  
# A female in the second passenger class
# 
mean(test_set$Survived[test_set$Sex == "female" & 
                         test_set$Pclass <= 2.5] == 1)
# 0.974
# correct  would survive
#############################################  
# A third-class female who paid a fare of $8
# 
mean(test_set$Survived[test_set$Sex == "female" & 
                         test_set$Pclass >= 2.5 &
                         test_set$Fare <= 23.35] == 1)
# 0.565
# correct  would survive
# 
###################################
# A 5-year-old male with 4 siblings
# 
mean(test_set$Survived[test_set$Sex == "male" & 
                         test_set$SibSp == 4 &
                         test_set$Age >= 3.5] == 1)
# 0
# correct  would Not survive
# 
#############################################
# A third-class female who paid a fare of $25
#
mean(test_set$Survived[test_set$Sex == "female" & 
                         test_set$Fare == 25] == 1)
# Nn
# correct  would Not survive
#
##################################################
# A first-class 17-year-old female with 2 siblings
# 
mean(test_set$Survived[test_set$Sex == "female" & 
                         test_set$Pclass <= 2.5 &
                         test_set$SibSp == 2 &
                         test_set$Age >= 3.5] == 1)
# 1
# would survive
#
####################################################
# A first-class 17-year-old male with 2 siblings
# 
mean(test_set$Survived[test_set$Sex == "male" & 
                         test_set$Age >= 3.5 & 
                         test_set$Pclass <= 2.5 &
                         test_set$SibSp == 2] == 1)
# 0
# would Not survive
# 
######################################################################################################### 
# Question 12: Random forest model
# 
# Set the seed to 14. Use the caret train() function with the rf method to train a random forest. 
# Test values of mtry = seq(1:7). Set ntree to 100.

#set.seed(14)
set.seed(14, sample.kind = "Rounding")    # simulate R 3.5
train_rf <- train(Survived ~ .,
                  data = train_set,
                  method = "rf",
                  ntree = 100,
                  tuneGrid = data.frame(mtry = seq(1:7)))
train_rf$bestTune
###################################### 
# What mtry value maximizes accuracy?
#3 
train_rf$bestTune

################################################################## 
# What is the accuracy of the random forest model on the test set?
#     0.86 
y_hat <- predict(train_rf, test_set)
confusionMatrix(data = y_hat, 
                reference = test_set$Survived)$overall["Accuracy"]
# 
# Use varImp() on the random forest model object to determine the importance of various predictors to 
# the random forest model.
# 
# What is the most important variable?
#
varImp(train_rf)$importance

#   Be sure to report the variable name exactly as it appears in the code.
