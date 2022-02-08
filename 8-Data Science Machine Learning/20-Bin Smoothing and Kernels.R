# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Linear Regression for Prediction, Smoothing, and Working with Matrices

### Bin Smoothing and Kernels

span <- 3.5
tmp <- polls_2008 %>%
     crossing(center = polls_2008$day) %>%
     mutate(dist = abs(day - center)) %>%
     filter(dist <= span) 
tmp %>% filter(center %in% c(-125, -55)) %>%
     ggplot(aes(day, margin)) +   
     geom_point(data = polls_2008, size = 3, alpha = 0.5, color = "grey") +
     geom_point(size = 2) +    
     geom_smooth(aes(group = center), 
                 method = "lm", formula=y~1, se = FALSE) +
     facet_wrap(~center)

# May have to download old version of gganimate
# Install using code below, then restart R session:
# library(devtools)
# install_github("thomasp85/gganimate", ref = "v0.1.1")
library(gganimate)
span <- 7
fit <- with(polls_2008, ksmooth(day, margin, kernel="box", x.points = day, bandwidth = span))
bin_fit <- data.frame(x = fit$x, .fitted=fit$y)
p <- tmp %>% 
     ggplot() +
     geom_smooth(aes(day, margin, group = center, frame = center), method = "lm", formula=y~1, se = FALSE) +
     geom_point(aes(day, margin), data = polls_2008, size = 3, alpha = .5, color = "grey") +
     geom_point(aes(day, margin, frame = center)) +
     geom_line(aes(x=x, y = .fitted, frame = x, cumulative = TRUE), data = bin_fit, color = "red") + 
     ggtitle("x0 = ")
gganimate(p, interval= .1)

span <- 7 
fit <- with(polls_2008, 
            ksmooth(day, margin, x.points = day, kernel="box", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
     ggplot(aes(day, margin)) +
     geom_point(size = 3, alpha = .5, color = "grey") + 
     geom_line(aes(day, smooth), color="red")

x_0 <- -125
data.frame(x = polls_2008$day) %>% mutate(w_0 = 1*I(abs(x - x_0)<=span/2)) %>%
     mutate(w_0 = w_0/sum(w_0)) %>%
     ggplot(aes(x, w_0)) +
     geom_step()

x_0 <- -125
tmp <- with(data.frame(day = seq(min(polls_2008$day), max(polls_2008$day), .25)), 
            ksmooth(day, 1*I(day == x_0), kernel = "normal", x.points = day, bandwidth = span))
data.frame(x = tmp$x, w_0 = tmp$y) %>%
     mutate(w_0 = w_0/sum(w_0)) %>%
     ggplot(aes(x, w_0)) +
     geom_line()

tmp <- polls_2008 %>%
     crossing(center = polls_2008$day) %>%
     mutate(dist = abs(day - center)) %>%
     filter(dist <= span) %>% 
     mutate(weight =  dnorm(dist, 0, span/2.54))%>%
     mutate(weight = weight/max(weight))
span <- 7
fit <- with(polls_2008, ksmooth(day, margin, kernel="normal", x.points = day, bandwidth = span))
bin_fit <- data.frame(x = fit$x, .fitted=fit$y)
p <- tmp %>%
     ggplot() +
     geom_smooth(aes(day, margin, group = center, weight = weight, frame = center), method = "lm", formula=y~1, se=FALSE) +
     geom_point(aes(day, margin), data = polls_2008, size = 3, alpha = .5, color = "grey") +
     geom_point(aes(day, margin, size = weight, frame = center), show.legend = FALSE) +   
     scale_size(range = c(0, 3)) +
     geom_line(aes(x=x, y = .fitted, frame = x, cumulative = TRUE), data = bin_fit, color = "red") + 
     ggtitle("x0 = ")
gganimate(p, interval= .1)

span <- 7
fit <- with(polls_2008, 
            ksmooth(day, margin,  x.points = day, kernel="normal", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
     ggplot(aes(day, margin)) +
     geom_point(size = 3, alpha = .5, color = "grey") + 
     geom_line(aes(day, smooth), color="red")
