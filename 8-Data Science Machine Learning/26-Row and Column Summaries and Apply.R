# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Linear Regression for Prediction, Smoothing, and Working with Matrices

## Working with Matrices

### Row and Column Summaries and Apply

sums <- rowSums(x)

avg <- rowMeans(x)

data_frame(labels = as.factor(y), row_averages = avg) %>% 
     qplot(labels, row_averages, data = ., geom = "boxplot") 

avgs <- apply(x, 1, mean)
sds <- apply(x, 2, sd)

