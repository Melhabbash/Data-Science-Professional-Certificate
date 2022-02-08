# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Linear Regression for Prediction, Smoothing, and Working with Matrices

## Working with Matrices

### Converting a Vector to a Matrix

my_vector <- 1:15
mat <- matrix(my_vector, 5, 3)
mat

mat_t <- matrix(my_vector, 3, 5, byrow = TRUE)
mat_t

identical(t(mat), mat_t)

matrix(my_vector, 5, 5)

grid <- matrix(x[3,], 28, 28)

image(1:28, 1:28, grid)
image(1:28, 1:28, grid[, 28:1])

