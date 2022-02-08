# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Classification with More than Two Classes and the Caret Package

## Classification with More than Two Classes

### Trees Motivation

rafalib::mypar()
x <- seq(0,1,len=100)
y <- rep(1, 100)
plot(x,y, xlab="",ylab="", cex=0.25, yaxt="n", xaxt="n",type="n")
lines(x[c(15,35)], y[c(15,35)], col="blue",lwd=3)
points(x,y, cex = 0.25)
points(x[25],y[25],col="blue", cex = 0.5, pch=4)
text(x[c(15,35)], y[c(15,35)], c("[","]"))

tmp <- expand.grid(1:10, 1:10)
x <- tmp[,1]
y <- tmp[,2]
rafalib::mypar()
plot(x,y, xlab="",ylab="", cex=0.25, yaxt="n", xaxt="n",type="n")
polygon(c(x[25]-0.5, x[25]-0.5, x[25]+0.5, x[25]+0.5),
        c(y[25]-0.5, y[25]+0.5, y[25]+0.5, y[25]-0.5), col="blue")
points(x,y, cex = 0.25)
points(x[25],y[25], cex = 0.5, pch=4)

rafalib::mypar()
plot(x,y, xlab="",ylab="", cex=0.25, yaxt="n", xaxt="n",type="n")
polygon(c(x[25]-sqrt(10)/2, x[25]-sqrt(10)/2, x[25]+sqrt(10)/2, x[25]+sqrt(10)/2),
        c(y[25]-sqrt(10)/2, y[25]+sqrt(10)/2, y[25]+sqrt(10)/2, y[25]-sqrt(10)/2),
        col="blue")
points(x,y, cex = 0.25)
points(x[25],y[25], cex = 0.5, pch=4)

library(tidyverse)
p <- 1:100
qplot(p, .1^(1/p), ylim = c(0,1))