
###################################################################################################
#Question 14: K-nearest neighbors model
#
#Set the seed to 7, then train a k-nearest neighbors model on the training set using the 
#caret package. Try odd values of????from 3 to 21. Use the final model to generate predictions 
# on the test set.
# set.seed(7)
set.seed(7, sample.kind = "Rounding") # simulate R 3.5
tuning <- data.frame(k = seq(3, 21, 2))
train_knn <- train(train_x, train_y,
                   method = "knn", 
                   tuneGrid = tuning)
# What is the final value of????used in the model? 21
train_knn$bestTune

# What is the accuracy of the kNN model on the test set? 0.948
knn_preds <- predict(train_knn, test_x)
mean(knn_preds == test_y)

###################################################################################################
#Question 15a: Random forest model
#Set the seed to 9, then train a random forest model on the training set using the caret package. 
# Test mtry values of c(3, 5, 7, 9). Use the argument importance = TRUE so that feature importance
# can be extracted. Generate predictions on the test set.
# Note: please use c(3, 5, 7, 9) instead of seq(3, 9, 2) in tuneGrid.
set.seed(9, sample.kind = "Rounding")
library(Rborist)
train_rf <- train(train_x, train_y,
                  method = "rf", 
                  tuneGrid = data.frame(mtry = c(3, 5, 7, 9)),
                  importance = TRUE)
# What value of mtry gives the highest accuracy? 3
train_rf$bestTune

# What is the accuracy of the random forest model on the test set? 0.974
rf_preds <- predict(train_rf, test_x)
mean(rf_preds == test_y)

# What is the most important variable in the random forest model?  area_worst
# Be sure to enter the variable name exactly as it appears in the dataset.
varImp(train_rf)

###################################################################################################
# Question 15b: Random forest model

# Consider the top 10 most important variables in the random forest model.

# Which set of features is most important for determining tumor type?
# mean values
# standard errors
# (True)worst values

# Explanation
# varImp(train_rf) gives the importance of the various variables. When looking at the top 10 
# most important features, 6 of the top 10 (including the top 4!) are worst values.

###################################################################################################
#Question 16a: Creating an ensemble
#Create an ensemble using the predictions from the 7 models created in the previous 
#exercises: k-means, logistic regression, LDA, QDA, loess, k-nearest neighbors, and 
#random forest. Use the ensemble to generate a majority prediction of the tumor type 
#(if most models suggest the tumor is malignant, predict malignant).

# What is the accuracy of the ensemble prediction? 0.983
ensemble <- cbind(glm = glm_preds == "B", lda = lda_preds == "B", qda = qda_preds == "B", loess = loess_preds == "B", rf = rf_preds == "B", knn = knn_preds == "B", kmeans = kmeans_preds == "B")

ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, "B", "M")
mean(ensemble_preds == test_y)

###################################################################################################
# Question 16b: Creating an ensemble
# Make a table of the accuracies of the 7 models and the accuracy of the ensemble model.
# Which of these models has the highest accuracy?

# Logistic regression
# (True) LDA
# Loess
# Random forest
# Ensemble

models <- c("K means", "Logistic regression", "LDA", "QDA", "Loess", "K nearest neighbors", "Random forest", "Ensemble")
accuracy <- c(mean(kmeans_preds == test_y),
              mean(glm_preds == test_y),
              mean(lda_preds == test_y),
              mean(qda_preds == test_y),
              mean(loess_preds == test_y),
              mean(knn_preds == test_y),
              mean(rf_preds == test_y),
              mean(ensemble_preds == test_y))
data.frame(Model = models, Accuracy = accuracy)
