##################################################################################################
# Set the seed to 1, then create a data partition splitting brca$y and the scaled version of the 
# brca$x matrix into a 20% test set and 80% train using the following code:
# set.seed(1) if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]

# You will be using these training and test sets throughout the exercises in Parts 3 and 4. 
# Save your models as you go, because at the end, you'll be asked to make an ensemble prediction 
# and to compare the accuracy of the various models!

##################################################################################################
#Question 9: Training and test sets

#Check that the training and test sets have similar proportions of benign and malignant tumors.

# What proportion of the training set is benign? 0.628
mean(train_y == "B")

# What proportion of the test set is benign? 0.626
mean(test_y == "B")

###################################################################################################
#Question 10a: K-means Clustering
#The predict_kmeans() function defined here takes two arguments - a matrix of observations 
#x and a k-means object k - and assigns each row of x to a cluster from k.

predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}

#Set the seed to 3. Perform k-means clustering on the training set with 2 centers and
#assign the output to k. Then use the predict_kmeans() function to make predictions on 
#the test set.
set.seed(3, sample.kind = "Rounding")

# What is the overall accuracy?  0.922 or 0.896
k <- kmeans(train_x, centers = 2)
kmeans_preds <- ifelse(predict_kmeans(test_x, k) == 1, "B", "M")
mean(kmeans_preds == test_y)

###################################################################################################
#Question 10b: K-means Clustering
#What proportion of benign tumors are correctly identified?  0.986
sensitivity(factor(kmeans_preds), test_y, positive = "B")

#What proportion of malignant tumors are correctly identified? 0.814
sensitivity(factor(kmeans_preds), test_y, positive = "M")

###################################################################################################
#Question 11: Logistic regression model
#Fit a logistic regression model on the training set with caret::train() using all predictors. 
# Ignore warnings about the algorithm not converging. 
#Make predictions on the test set.
# What is the accuracy of the logistic regression model on the test set? 0.957
train_glm <- train(train_x, train_y, method = "glm")
glm_preds <- predict(train_glm, test_x)
mean(glm_preds == test_y)

###################################################################################################
#Question 12: LDA and QDA models
#Train an LDA model and a QDA model on the training set. Make predictions on the test set 
# using each model.
train_lda <- train(train_x, train_y, method = "lda")
train_qda <- train(train_x, train_y, method = "qda")
lda_preds <- predict(train_lda, test_x)
qda_preds <- predict(train_qda, test_x)
# What is the accuracy of the LDA model on the test set? 0.991
mean(lda_preds == test_y)

# What is the accuracy of the QDA model on the test set? 0.957
mean(qda_preds == test_y)

###################################################################################################

#Question 13: Loess model
#Set the seed to 5, then fit a loess model on the training set with the caret package. 
#You will need to install the gam package if you have not yet done so. Use the default 
#tuning grid. This may take several minutes; ignore warnings. Generate predictions on 
#the test set.
set.seed(5, sample.kind = "Rounding")
#install.packages("gam")
train_loess <- train(train_x, train_y, method = "gamLoess")
loess_preds <- predict(train_loess, test_x)

# What is the accuracy of the loess model on the test set? 0.983
mean(loess_preds == test_y)
