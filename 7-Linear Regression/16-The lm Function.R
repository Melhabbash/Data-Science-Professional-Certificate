# fit regression line to predict son's height from father's height
fit <- lm(son ~ father, data = galton_heights)
fit

# summary statistics
summary(fit)
