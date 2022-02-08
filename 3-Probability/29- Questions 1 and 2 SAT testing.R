# The SAT is a standardized college admissions test used in the United States. 
# The following two multi-part questions will ask you some questions about SAT testing.
# 
# This is a 6-part question asking you to determine some probabilities of what happens when 
# a student guessed for all of their answers on the SAT. Use the information below to 
# inform your answers for the following questions.
# 
# An old version of the SAT college entrance exam had a -0.25 point penalty for every 
# incorrect answer and awarded 1 point for a correct answer. The quantitative test 
# consisted of 44 multiple-choice questions each with 5 answer choices. Suppose 
# a student chooses answers by guessing for all questions on the test.

################################################################################
# Question 1a
# What is the probability of guessing correctly for one question?

p<-1/5

################################################################################
# Question 1b
# What is the expected value of points for guessing on one question?
# E[X] = ap + b(1 ??? p)
p<-1/5
a<-1
b<- -0.25
a*p+b*(1-p)
1*(1/5)+(-0.25)*(4/5)

################################################################################

# Question 1c
# What is the expected score of guessing on all 44 questions?

# E[X] = n*(ap + b(1 ??? p))
n<-44
p<-1/5
a<-1
b<- -0.25
n*(a*p+b*(1-p))
44*(1*(1/5)+(-0.25)*(4/5))

################################################################################

# Question 1d
# What is the standard error of guessing on all 44 questions?

# sqrt(n)*(abs(b-a)*sqrt(p(1-p)))
n<-44
p<-1/5
a<-1
b<- -0.25

sqrt(n)*abs(b-a)*sqrt(p*(1-p))
sqrt(44)*abs(-0.25-1)*sqrt((1/5)*(4/5))

################################################################################

# Question 1e
# Use the Central Limit Theorem to determine the probability that a guessing student 
# scores 8 points or higher on the test.
n<-44
p<-1/5
a<-1
b<- -0.25

mu<-n*(a*p+b*(1-p))
se<-sqrt(n)*abs(b-a)*sqrt(p*(1-p))
1-pnorm(8,mu,se)

################################################################################

# Question 1f
# Set the seed to 21, then run a Monte Carlo simulation of 10,000 students guessing 
# on the test.
# (IMPORTANT! If you use R 3.6 or later, you will need to use the command set.seed
# (x, sample.kind = "Rounding") instead of set.seed(x). Your R version will be printed 
# at the top of the Console window when you start RStudio.)
# What is the probability that a guessing student scores 8 points or higher?

set.seed(21)

n <- 44
B <- 10000
SAT <- function(n){
  X <- sample(c(1,-0.25), n, replace = TRUE, prob=c(1/5, 4/5))
  sum(X)
}
S<- replicate(B, SAT(n))
1-mean(S<8)

################################################################################

# The SAT was recently changed to reduce the number of multiple choice options from 
# 5 to 4 and also to eliminate the penalty for guessing.
# 
# In this two-part question, you'll explore how that affected the expected values for 
# the test.
# 
# Question 2a
# 0.0/1.0 point (graded)
# Suppose that the number of multiple choice options is 4 and that there is no penalty 
# for guessing - that is, an incorrect question gives a score of 0.
# 
# What is the expected value of the score when guessing on this new test?

# E(x)=n*(a*p+b*(1-p))
n<-44
p<-1/4
a<-1
b<- 0

n*(a*p+b*(1-p))

################################################################################

# Question 2b
# Consider a range of correct answer probabilities p <- seq(0.25, 0.95, 0.05) 
# representing a range of student skills.
# 
# What is the lowest p such that the probability of scoring over 35 exceeds 80%?

p <- seq(0.25, 0.95, 0.05)
n<-44
v<-1/4
a<-1
b<-0
score <- sapply(p, function(v){
  mu <- n*((a*v) + (b*(1-v)))
  se <- sqrt(n) * abs(b-a) * sqrt(v*(1-v))
  1-pnorm (35, mu, se)
})

min(p[which(score > 0.8)])
