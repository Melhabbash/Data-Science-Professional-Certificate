# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Linear Regression for Prediction, Smoothing, and Working with Matrices

### Local Weighted Regression (loess)

span <- 21/diff(range(polls_2008$day))

tmp <- polls_2008 %>%
     crossing(center = polls_2008$day) %>%
     mutate(dist = abs(day - center)) %>%
     filter(rank(dist) / n() <= span) %>%
     mutate(weight = (1 - (dist / max(dist)) ^ 3) ^ 3)

tmp %>% 
     filter(center %in% c(-125, -55)) %>%
     ggplot(aes(day, margin)) +   
     scale_size(range = c(0, 3)) +
     geom_smooth(aes(group = center, weight = weight), 
                 method = "lm", se = FALSE) +
     geom_point(data = polls_2008, size = 3, alpha = .5, color = "grey") +
     geom_point(aes(size = weight)) +
     facet_wrap(~center)

library(broom)
fit <- loess(margin ~ day, degree=1, span = span, data=polls_2008)
loess_fit <- augment(fit)

p <- ggplot(tmp, aes(day, margin)) +
     scale_size(range = c(0, 3)) +
     geom_smooth(aes(group = center, frame = center, weight = weight), method = "lm", se = FALSE) +
     geom_point(data = polls_2008, size = 3, alpha = .5, color = "grey") +
     geom_point(aes(size = weight, frame = center)) +
     geom_line(aes(x=day, y = .fitted, frame = day, cumulative = TRUE),
               data = loess_fit, color = "red") +
     ggtitle("x0 = ")
gganimate(p, interval= .1)

total_days <- diff(range(polls_2008$day))
span <- 21/total_days

fit <- loess(margin ~ day, degree=1, span = span, data=polls_2008)

polls_2008 %>% mutate(smooth = fit$fitted) %>%
     ggplot(aes(day, margin)) +
     geom_point(size = 3, alpha = .5, color = "grey") +
     geom_line(aes(day, smooth), color="red")

spans <- c(.66, 0.25, 0.15, 0.10)
fits <- data_frame(span = spans) %>% 
     group_by(span) %>% 
     do(broom::augment(loess(margin ~ day, degree=1, span = .$span, data=polls_2008)))
tmp <- fits %>%
     crossing(span = spans, center = polls_2008$day) %>%
     mutate(dist = abs(day - center)) %>%
     filter(rank(dist) / n() <= span) %>%
     mutate(weight = (1 - (dist / max(dist)) ^ 3) ^ 3)
p <- ggplot(tmp, aes(day, margin)) +
     scale_size(range = c(0, 2)) +
     geom_smooth(aes(group = center, frame = center, weight = weight), method = "lm", se = FALSE) +
     geom_point(data = polls_2008, size = 2, alpha = .5, color = "grey") +
     geom_line(aes(x=day, y = .fitted, frame = day, cumulative = TRUE),
               data = fits, color = "red") +
     geom_point(aes(size = weight, frame = center)) +
     facet_wrap(~span) +
     ggtitle("x0 = ")
gganimate(p, interval= .1)

tmp %>% ggplot(aes(day, margin)) +
     geom_point(size = 2, alpha = .5, color = "grey") +
     geom_line(aes(day, .fitted), data = fits, color = "red") +
     facet_wrap(~span)

data.frame(x = seq(min(polls_2008$day), max(polls_2008$day), length.out = 100)) %>%
     mutate(w_0 = (1 - (abs(x-x_0)/21)^3)^3*I(abs(x-x_0)<=21)) %>%
     ggplot(aes(x, w_0)) +
     geom_line()

total_days <- diff(range(polls_2008$day))
span <- 28/total_days
fit_1 <- loess(margin ~ day, degree=1, span = span, data=polls_2008)
fit_2 <- loess(margin ~ day, span = span, data=polls_2008)

polls_2008 %>% mutate(smooth_1 = fit_1$fitted, smooth_2 = fit_2$fitted) %>%
     ggplot(aes(day, margin)) +
     geom_point(size = 3, alpha = .5, color = "grey") +
     geom_line(aes(day, smooth_1), color="red", lty = 2) +
     geom_line(aes(day, smooth_2), color="orange", lty = 1) 

polls_2008 %>% ggplot(aes(day, margin)) +
     geom_point() + 
     geom_smooth()

polls_2008 %>% ggplot(aes(day, margin)) +
     geom_point() + 
     geom_smooth(color="red",  span = 0.15,
                 method = "loess", method.args = list(degree=1))
