# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Linear Regression for Prediction, Smoothing, and Working with Matrices

## Working with Matrices

### Indexing with Matrices and Binarizing the Data

mat <- matrix(1:15, 5, 3)
mat
as.vector(mat)

qplot(as.vector(x), bins = 30, color = I("black"))

new_x <- x
new_x[new_x < 50] <- 0

mat <- matrix(1:15, 5, 3)
mat[mat < 3] <- 0
mat

mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat < 12] <- 0
mat

bin_x <- x
bin_x[bin_x < 255/2] <- 0 
bin_x[bin_x > 255/2] <- 1

bin_X <- (x > 255/2)*1

rafalib::mypar(1,2)
rows <- 1:28
columns <- 1:28
image(rows, columns, matrix(-x[8,], 28, 28), main = "Original")
image(rows, columns, matrix(-bin_x[8,], 28, 28), main ="Binarized")
