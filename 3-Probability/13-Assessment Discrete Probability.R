library(gtools)
library(tidyverse)

# In the 200m dash finals in the Olympics, 8 runners compete for 3 medals (order matters). 
# In the 2012 Olympics, 3 of the 8 runners were from Jamaica and the other 5 were from 
# different countries. The three medals were all won by Jamaica (Usain Bolt, Yohan Blake, 
# and Warren Weir).
# Use the information above to help you answer the following four questions.
############################
# Question 1a
# 0.0/1.0 point (graded)
# How many different ways can the 3 medals be distributed across 8 runners?

# 8p3=8!/(8-3)!=(8*7*6)*5!/5!

 8*7*6 
 ############################
 # Question 1b
 # 0.0/1.0 point (graded)
 # How many different ways can the three medals be distributed among the 3 runners from 
 # Jamaica? 
 # 3p3=3*2*1/0!=3*2*1/1=6
 #############################

# Question 1c
# 0.0/1.0 point (graded)
# What is the probability that all 3 medals are won by Jamaica?
 
# pr(Jamaica)=(3/8)*(2/7)*(1/6)=1/(7*8)
1/(7*8)

#############################
# 
# Question 1d
# 0.0/1.0 point (graded)
# Run a Monte Carlo simulation on this vector representing the countries of the 8 runners 
# in this race:
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")

# For each iteration of the Monte Carlo simulation, within a replicate() loop, select 3 
# runners representing the 3 medalists and check whether they are all from Jamaica. 
# Repeat this simulation 10,000 times. Set the seed to 1 before running the loop.
# 
# Calculate the probability that all the runners are from Jamaica.
set.seed(1)
sample(runners,3,replace = FALSE)
B <- 10000   # number of iteration
winners <- replicate(B, {
        result<-sample(runners,3,replace = FALSE)==c("Jamaica","Jamaica","Jamaica")
        result
        sum(result)>2
        })
        

tab <- table(winners)    # make a table of outcome counts
tab    # view count table
prop.table(tab)

###########################################

# Question 2: Restaurant management
# 
# Use the information below to answer the following five questions.
# 
# A restaurant manager wants to advertise that his lunch special offers enough choices to eat different meals every 
# day of the year. He doesn't think his current special actually allows that number of choices, but wants to change 
# his special if needed to allow at least 365 choices.

# A meal at the restaurant includes 1 entree, 2 sides, and 1 drink. He currently offers a choice of 1 entree from 
# a list of 6 options, a choice of 2 different sides from a list of 6 options, and a choice of 1 drink from 
# a list of 2 options.


# Question 2a
# How many meal combinations are possible with the current menu?

# meal={entree,sides,drink}
# 1 entrees from 6 options , 2 sides from 6 options, 1 from 2 options

# 6c1*6c2*2c1+6*15*2+180
choose(6,1)*choose(6,2)*choose(2,1)


##########################################

# Question 2b
# The manager has one additional drink he could add to the special.
# 
# How many combinations are possible if he expands his original special to 3 drink options?

# meal={entree,sides,drink}
# 1 entrees from 6 options , 2 sides from 6 options, 1 from 3 options
choose(6,1)*choose(6,2)*choose(3,1)

#########################################

# Question 2c
# The manager decides to add the third drink but needs to expand the number of options. 
# The manager would prefer not to change his menu further and wants to know if he can meet his goal by letting 
# customers choose more sides.

# meal={entree,sides,drink}
# 1 entrees from 6 options , 3 sides from 6 options, 1 from 3 options
choose(6,1)*choose(6,3)*choose(3,1)

#########################################

# Question 2d
# The manager is concerned that customers may not want 3 sides with their meal. He is willing to increase 
# the number of entree choices instead, but if he adds too many expensive options it could eat into profits. 
# He wants to know how many entree choices he would have to offer in order to meet his goal.
# 
# - Write a function that takes a number of entree choices and returns the number of meal combinations possible 
# given that number of entree options, 3 drink choices, and a selection of 2 sides from 6 options.
# 
# - Use sapply() to apply the function to entree option counts ranging from 1 to 12.
# 
# What is the minimum number of entree options required in order to generate more than 365 combinations?


# How many meal combinations are there if customers can choose from 6 entrees, 3 drinks, and select 3 sides
# from the current 6 options?

# meal={entree,sides,drink}
# 1 entrees from x options , 2 sides from 6 options, 1 from 3 options
# x>=6
x<-seq(6,10,1)
options<- function(x){
        choose(x,1)*choose(6,2)*choose(3,1)
}
sapply(x, options)

x<-9
######################################

# Question 2e
# 
# The manager isn't sure he can afford to put that many entree choices on the lunch menu and thinks it would be 
# cheaper for him to expand the number of sides. He wants to know how many sides he would have to offer to meet 
# his goal of at least 365 combinations.
# 
# - Write a function that takes a number of side choices and returns the number of meal combinations possible 
# given 6 entree choices, 3 drink choices, and a selection of 2 sides from the specified number of side choices.
# 
# - Use sapply() to apply the function to side counts ranging from 2 to 12.
# 
# What is the minimum number of side options required in order to generate more than 365 combinations?

# meal={entree,sides,drink}
# 1 entrees from 6 options , 2 sides from x options, 1 from 3 options
# x>=6
x<-seq(6,10,1)
options<- function(x){
        choose(6,1)*choose(x,2)*choose(3,1)
}
sapply(x, options)

x<-7
##############################################################################################

# Questions 3 and 4: Esophageal cancer and alcohol/tobacco use, part 1

# Case-control studies help determine whether certain exposures are associated with outcomes such as developing 
# cancer. The built-in dataset esoph contains data from a case-control study in France comparing people with 
# esophageal cancer (cases, counted in ncases) to people without esophageal cancer (controls, counted in ncontrols)
# that are carefully matched on a variety of demographic and medical characteristics. The study compares alcohol 
# intake in grams per day (alcgp) and tobacco intake in grams per day (tobgp) across cases and controls grouped 
# by age range (agegp).
# 
# The dataset is available in base R and can be called with the variable name esoph:
head(esoph)

# You will be using this dataset to answer the following four multi-part questions (Questions 3-6).
# 
# You may wish to use the tidyverse package:

library(tidyverse)

# The following three parts have you explore some basic characteristics of the dataset.
# 
# Each row contains one group of the experiment. Each group has a different combination of age, alcohol 
# consumption, and tobacco consumption. The number of cancer cases and number of controls 
# (individuals without cancer) are reported for each group.

# Question 3a
# 
# How many groups are in the study?
esoph

###############################################

# Question 3b
# How many cases are there?
# Save this value as all_cases for later problems.
all_cases<-sum(esoph$ncases) 
all_cases


##############################################

# Question 3c
# How many controls are there?
# Save this value as all_controls for later problems.

all_controls<-sum(esoph$ncontrols) 
all_controls

#########################################################

# The following four parts ask you to explore some probabilities within this dataset related to alcohol 
# and tobacco consumption.

# Question 4a
# What is the probability that a subject in the highest alcohol consumption group is a cancer case?
esoph %>% filter(alcgp == max(esoph$alcgp)) %>% 
        summarize(sum_cases=sum(ncases), tot=sum(ncontrols) + sum(ncases), probability=sum_cases/tot) 

################################################
# Question 4b
# What is the probability that a subject in the lowest alcohol consumption group is a cancer case?
esoph %>% filter(alcgp == min(esoph$alcgp)) %>% 
        summarize(sum_cases=sum(ncases), tot=sum(ncontrols) + sum(ncases), probability=sum_cases/tot) 

###############################################

# Question 4c
# Given that a person is a case, what is the probability that they smoke 10g or more a day?
tot_cases<-sum(esoph$ncases)
tot_cases
smoking10_cases<- sum(esoph$ncases[esoph$tobgp!= "0-9g/day"])
smoking10_cases

prop<-smoking10_cases/tot_cases
prop

################################################

# Question 4d
# Given that a person is a control, what is the probability that they smoke 10g or more a day?

tot_control<-sum(esoph$ncontrols)
tot_control
smoking10_control<- sum(esoph$ncontrols[esoph$tobgp!= "0-9g/day"])
smoking10_control

prop<-smoking10_control/tot_control
prop
###############################################

# Questions 5 and 6: Esophageal cancer and alcohol/tobacco use, part 2
# The following four parts look at probabilities related to alcohol and tobacco 
# consumption among the cases.

# Question 5a
# For cases, what is the probability of being in the highest alcohol group?
all_cases <- sum(esoph$ncases)
highst_alcohol<- sum(esoph$ncases[esoph$alcgp == max(esoph$alcgp)])
prob_alcohol_cases<-highst_alcohol/all_cases
prob_alcohol_cases
##############################################

# Question 5b
# For cases, what is the probability of being in the highest tobacco group?
all_cases <- sum(esoph$ncases)
highst_tobacco<- sum(esoph$ncases[esoph$tobgp == max(esoph$tobgp )])
prob_tobacco_cases<-highst_tobacco/all_cases
prob_tobacco_cases
#############################################

# Question 5c
# 
# For cases, what is the probability of being in the highest alcohol group and the 
# highest tobacco group?

all_cases <- sum(esoph$ncases)
highst_alcohol_and_tobacco<- sum(esoph$ncases[esoph$tobgp == max(esoph$tobgp ) &  esoph$alcgp == max(esoph$alcgp)])
prob_alcohol_and_tobacco_cases<-highst_alcohol_and_tobacco/all_cases
prob_alcohol_and_tobacco_cases
#############################################
# For cases, what is the probability of being in the highest alcohol group or the 
# highest tobacco group?
all_cases <- sum(esoph$ncases)
highst_alcohol_or_tobacco<- sum(esoph$ncases[esoph$tobgp == max(esoph$tobgp ) |  esoph$alcgp == max(esoph$alcgp)])
prob_alcohol_or_tobacco_cases<-highst_alcohol_or_tobacco/all_cases
prob_alcohol_or_tobacco_cases
############################################

# The following six parts look at probabilities related to alcohol and tobacco 
# consumption among the controls and also compare the cases and the controls.

# Question 6a

# For controls, what is the probability of being in the highest alcohol group?
all_controls <- sum(esoph$ncontrols)
highst_alcohol<- sum(esoph$ncontrols[esoph$alcgp == max(esoph$alcgp)])
prob_alcohol_controls<-highst_alcohol/all_controls
prob_alcohol_controls
###########################################

# Question 6b
# How many times more likely are cases than controls to be in the highest alcohol group?
prob_alcohol_cases/prob_alcohol_controls        
###########################################

# Question 6c
# For controls, what is the probability of being in the highest tobacco group?
all_controls <- sum(esoph$ncontrols)
highst_tobacco<- sum(esoph$ncontrols[esoph$tobgp == max(esoph$tobgp )])
prob_tobacco_controls<-highst_tobacco/all_controls
prob_tobacco_controls
###########################################

# Question 6d
# For controls, what is the probability of being in the highest alcohol group and 
# the highest tobacco group?
all_controls <- sum(esoph$ncontrols)
highst_alcohol_and_tobacco<- sum(esoph$ncontrols[esoph$tobgp == max(esoph$tobgp ) &  esoph$alcgp == max(esoph$alcgp)])
prob_alcohol_and_tobacco_controls<-highst_alcohol_and_tobacco/all_controls
prob_alcohol_and_tobacco_controls
##########################################

# Question 6e
# For controls, what is the probability of being in the highest alcohol group or 
# the highest tobacco group?
all_controls <- sum(esoph$ncontrols)
highst_alcohol_and_tobacco<- sum(esoph$ncontrols[esoph$tobgp == max(esoph$tobgp ) |  esoph$alcgp == max(esoph$alcgp)])
prob_alcohol_or_tobacco_controls<-highst_alcohol_and_tobacco/all_controls
prob_alcohol_or_tobacco_controls
##########################################

# Question 6f
# 0.0/1.0 point (graded)
# How many times more likely are cases than controls to be in the highest alcohol 
# group or the highest tobacco group?

prob_alcohol_or_tobacco_cases/prob_alcohol_or_tobacco_controls

