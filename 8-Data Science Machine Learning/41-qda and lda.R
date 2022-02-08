# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Distance, Knn, Cross-validation, and Generative Models

### Generative Models

### qda and lda

# Load data
data("mnist_27")

# Estimate parameters from the data
params <- mnist_27$train %>% 
     group_by(y) %>% 
     summarize(avg_1 = mean(x_1), avg_2 = mean(x_2), 
               sd_1= sd(x_1), sd_2 = sd(x_2), 
               r = cor(x_1, x_2))
params

# Contour plots
mnist_27$train %>% mutate(y = factor(y)) %>% 
     ggplot(aes(x_1, x_2, fill = y, color=y)) + 
     geom_point(show.legend = FALSE) + 
     stat_ellipse(type="norm", lwd = 1.5)

# Fit model
library(caret)
train_qda <- train(y ~ ., method = "qda", data = mnist_27$train)

# Obtain predictors and accuracy
y_hat <- predict(train_qda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]

plot_cond_prob <- function(p_hat=NULL){
     tmp <- mnist_27$true_p
     if(!is.null(p_hat)){
          tmp <- mutate(tmp, p=p_hat)
     }
     tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
          geom_raster(show.legend = FALSE) +
          scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
          stat_contour(breaks=c(0.5),color="black")
}
p1 <- plot_cond_prob() + ggtitle("True conditional probability")
p1

# Draw separate plots for 2s and 7s
mnist_27$train %>% mutate(y = factor(y)) %>% 
     ggplot(aes(x_1, x_2, fill = y, color=y)) + 
     geom_point(show.legend = FALSE) + 
     stat_ellipse(type="norm") +
     facet_wrap(~y)


# LDA
params <- mnist_27$train %>% 
     group_by(y) %>% 
     summarize(avg_1 = mean(x_1), avg_2 = mean(x_2), sd_1= sd(x_1), sd_2 = sd(x_2), r = cor(x_1,x_2))
params <-params %>% mutate(sd_1 = mean(sd_1), sd_2=mean(sd_2), r=mean(r))
params 

tmp <- lapply(1:2, function(i){
     with(params[i,], MASS::mvrnorm(1000, mu = c(avg_1, avg_2), Sigma = matrix(c(sd_1^2, sd_1*sd_2*r, sd_1*sd_2*r, sd_2^2), 2, 2))) %>%
          as.data.frame() %>% 
          setNames(c("x_1", "x_2")) %>% 
          mutate(y  = factor(c(2,7)[i]))
})
tmp <- do.call(rbind, tmp)
mnist_27$train %>% mutate(y = factor(y)) %>% 
     ggplot() + 
     geom_point(aes(x_1, x_2, color=y), show.legend = FALSE) + 
     stat_ellipse(aes(x_1, x_2, color = y), data = tmp, type="norm", lwd = 1.5)

p2 <- plot_cond_prob(predict(train_lda, mnist_27$true_p, type = "prob")[,2]) +
     ggtitle("LDA")
p2

train_lda <- train(y ~ .,
                   method = "lda",
                   data = mnist_27$train)
y_hat <- predict(train_lda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]
