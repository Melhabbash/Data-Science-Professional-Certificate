# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Linear Regression for Prediction, Smoothing, and Working with Matrices

## Working with Matrices

### Filtering Columns Based on Summaries

library(matrixStats)
sds <- colSds(x)
qplot(sds, bins = "30", color = I("black"))

image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1])

x[ ,c(351,352)]

x[c(2,3),]

new_x <- x[ ,colSds(x) > 60]
dim(new_x)

class(x[,1])
dim(x[1,])

class(x[ , 1, drop=FALSE])
dim(x[, 1, drop=FALSE])

