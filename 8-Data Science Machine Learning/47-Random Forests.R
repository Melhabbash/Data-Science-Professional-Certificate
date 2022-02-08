# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Classification with More than Two Classes and the Caret Package

## Classification with More than Two Classes

### Random Forests

library(randomForest)
fit <- randomForest(margin~., data = polls_2008) 
plot(fit)

polls_2008 %>%
     mutate(y_hat = predict(fit, newdata = polls_2008)) %>% 
     ggplot() +
     geom_point(aes(day, margin)) +
     geom_line(aes(day, y_hat), col="red")

library(rafalib)
# output .gif file
animation::saveGIF({
     set.seed(1)
     ntrees <- 50
     XLIM <- range(polls_2008$day)
     YLIM <- range(polls_2008$margin)
     
     sum <- rep(0,nrow(polls_2008))
     res <- vector("list", ntrees)
     
     for(i in 0:ntrees){
          mypar(1,1)
          if(i==0){
               with(polls_2008, plot(day, margin, pch = 1, main = "Data", xlim=XLIM,
                                     ylim=YLIM,
                                     xlab = "Days", ylab="Obama - McCain"))
          } else{
               ind <- sort(sample(1:nrow(polls_2008), replace = TRUE))
               tmp <- polls_2008[ind,]
               fit <- rpart(margin~day, data = tmp)
               pred <- predict(fit, newdata = tmp)
               res[[i]] <- data_frame(day = tmp$day, margin=pred)
               pred <- predict(fit, newdata = polls_2008)
               sum <- sum+pred
               avg <- sum/i
               with(tmp, plot(day,margin, pch=1, xlim=XLIM, ylim=YLIM, type="n",
                              xlab = "Days", ylab="Obama - McCain",
                              main=ifelse(i==1, paste(i, "tree"),paste(i, "trees"))))
               for(j in 1:i){
                    with(res[[j]], lines(day, margin, type="s", col="grey", lty=2))
               }
               with(tmp, points(day,margin, pch=1))
               with(res[[i]], lines(day, margin, type="s",col="azure4",lwd=2))
               lines(polls_2008$day, avg, lwd=3, col="blue")
          }
     }
     for(i in 1:5){
          mypar(1,1)
          with(polls_2008, plot(day, margin, pch = 1, main="Final", xlim=XLIM, ylim=YLIM,
                                xlab = "Days", ylab="Obama - McCain"))
          lines(polls_2008$day, avg, lwd=3, col="blue")
     }
}, movie.name = "rf.gif", ani.loop=0, ani.delay =50)
# plot
{set.seed(1)
ntrees <- 50
XLIM <- range(polls_2008$day)
YLIM <- range(polls_2008$margin)
sum <- rep(0,nrow(polls_2008))
res <- vector("list", ntrees)
mypar(2,3)
show <- c(1, 5, 25, 50) 
for(i in 0:ntrees){
     if(i==0){
          with(polls_2008, plot(day, margin, pch = 1, main = "Data", xlim=XLIM,
                                ylim=YLIM,
                                xlab = "Days", ylab="Obama - McCain"))
     } else{
          ind <- sort(sample(1:nrow(polls_2008), replace = TRUE))
          tmp <- polls_2008[ind,]
          fit <- rpart(margin~day, data = tmp)
          pred <- predict(fit, newdata = tmp)
          res[[i]] <- data_frame(day = tmp$day, margin=pred)
          pred <- predict(fit, newdata = polls_2008)
          sum <- sum+pred
          avg <- sum/i
          if(i %in% show){
               with(tmp, plot(day,margin, pch=1, xlim=XLIM, ylim=YLIM, type="n",
                              xlab = "Days", ylab="Obama - McCain",
                              main=ifelse(i==1, paste(i, "tree"),paste(i, "trees"))))
               for(j in 1:i){
                    with(res[[j]], lines(day, margin, type="s", col="grey", lty=2))
               }
               with(tmp, points(day,margin, pch=1))
               with(res[[i]], lines(day, margin, type="s",col="azure4",lwd=2))
               lines(polls_2008$day, avg, lwd=3, col="blue")
          }
     }
}
with(polls_2008, plot(day, margin, pch = 1, main="Final", xlim=XLIM, ylim=YLIM,
                      xlab = "Days", ylab="Obama - McCain"))
lines(polls_2008$day, avg, lwd=3, col="blue")}

library(randomForest)
train_rf <- randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]


# use cross validation to choose parameter
p1 <- plot_cond_prob(predict(train_rf, newdata = mnist_27$true_p, type = "prob")[,2]) +
     ggtitle("Random Forest")
p1

train_rf_2 <- train(y ~ .,
             method = "Rborist",
             tuneGrid = data.frame(predFixed = 2,
                                   minNode = c(3, 50)),
             data = mnist_27$train)
confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

p2 <- plot_cond_prob(predict(train_rf_2, newdata = mnist_27$true_p, type="prob")[,2]) +
     ggtitle("Random Forest")
p2
