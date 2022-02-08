# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Linear Regression for Prediction, Smoothing, and Working with Matrices

## Working with Matrices

### Vectorization for Matrices and Matrix Algebra Operations

(x - rowMeans(x)) / rowSds(x)

t(t(x) - colMeans(x))

X_mean_0 <- sweep(x, 2, colMeans(x))

x_mean_0 <- sweep(x, 2, colMeans(x))
x_standardized <- sweep(x_mean_0, 2, colSds(x), FUN = "/")

t(x) %*% x
crossprod(x)

solve(crossprod(x))

qr(x)
