library(tidyverse)
library(ggrepel)
library(dslabs)
ds_theme_set()
data(murders)
head(murders)
# import US election results data
data(polls_us_election_2016)
head(results_us_election_2016)
tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)
tab

################################################################################
# Question 1
# 
# You have created data frames tab1 and tab2 of state population and election 
# data, similar to our module videos:
tab1 <- tab[c(1:3,8:9),c(1, 4)]  
tab1
# state   	     population
# Alabama             4779736
# Alaska     	         710231
# Arizona    	        6392017
# Delaware     	     897934
# District of Columbia 601723

tab2<- tab[c(1:3,5:7),c(1, 6)]
tab2
# state  electoral_votes
# Alabama      9
# Alaska       3
# Arizona     11
# California  55
# Colorado     9
# Connecticut  7

dim(tab1)
 # 5 2

dim(tab2)
 # 6 2

# What are the dimensions of the table dat, created by the following command?
  
  
dat <- left_join(tab1, tab2, by = "state")
dim(dat)
dat
# 3 rows by 3 columns
# 5 rows by 2 columns
# 5 rows by 3 columns
# 6 rows by 3 columns

# Answer
# Correct:When we use a left_join command, all rows in the left-hand table 
# (in this case, tab1) are retained in the final table, so we expect to have five rows. 
# In addition, columns from both tables will be included in the final "dat" table so we 
# expect to have three columns.

###############################################################################

# Question 2
# 
# We are still using the tab1 and tab2 tables shown in question 1. What join 
# command would create a new table "dat" with three rows and two columns?
  
  
  
dat <- right_join(tab1, tab2, by = "state")
dat

dat <- full_join(tab1, tab2, by = "state") 
dat

dat <- inner_join(tab1, tab2, by = "state") 
dat

# True
dat <- semi_join(tab1, tab2, by = "state") 
dat

# Answer
# Correct:The semi_join command takes tab1 and limits it to states that are also in 
# tab2, without adding the additional columns in tab2. This gives us three rows 
# (states in both tables) and two columns (state and population, the two columns in tab1).

##############################################################################

# Question 3
# Which of the following are real differences between the join and bind functions?
# Please select all correct answers.

# (True) Binding functions combine by position, while join functions match by variables.
# (True) Joining functions can join datasets of different dimensions, but the bind 
# functions must match on the appropriate dimension (either same row or column numbers).
# (True) Bind functions can combine both vectors and dataframes, while join functions 
# work for only for dataframes.
# The join functions are a part of the dplyr package and have been optimized for speed, 
# while the bind functions are inefficient base functions.


###############################################################################

# Question 4
# We have two simple tables, shown below, with columns x and y:

df1<- matrix(c("a","a","b","a"),ncol=2,byrow=TRUE)
# colnames(df1) <- c("x","y")
# rownames(df1)<- c("","")
df1= as.table(df1)
df1

# x     y    
# a     a    
# b     a    

df2<- matrix(c("a","a","a","b"),ncol=2,byrow=TRUE)
# colnames(df2) <- c("x","y")
# rownames(df2)<- c("","")
df2= as.table(df2)
df2

# x     y    
# a     a    
# a     b  

# Which command would result in the following table?
  
#   > final
# x     y    
# b     a   


final <- union(df1, df2)
final

final <- setdiff(df1, df2)
final

final <- setdiff(df2, df1)
final

final <- intersect(df1, df2)
final



##############################################################################

# Introduction to Questions 5-7
# Install and load the Lahman library. This library contains a variety of datasets 
# related to US professional baseball. We will use this library for the next few 
# questions and will discuss it more extensively in the Regression course. For now, 
# focus on wrangling the data rather than understanding the statistics.
# 
# The Batting data frame contains the offensive statistics for all baseball players 
# over several seasons.  Filter this data frame to define top as the top 10 home run 
# (HR) hitters in 2016:
  
  library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
# Also Inspect the Master data frame, which has demographic information for all players:
  
Master %>% as_tibble()

###############################################################################
# Question 5
# Use the correct join or bind function to create a combined table of the names and 
# statistics of the top 10 home run (HR) hitters for 2016. This table should have the 
# player ID, first name, last name, and number of HR for the top 10 players. Name this 
# data frame top_names.
# 
# Identify the join or bind that fills the blank in this code to create the correct 
# table:
  
  
  # top_names <- top %>% ___________________ %>%
  # select(playerID, nameFirst, nameLast, HR)

top_names <- top %>%left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)
top_names
# Which bind or join function fills the blank to generate the correct table?
# rbind(Master)
# cbind(Master)
# (True)left_join(Master)
# right_join(Master)
# full_join(Master)
# anti_join(Master)


###############################################################################
# Question 6
# Inspect the Salaries data frame. Filter this data frame to the 2016 salaries, 
# then use the correct bind join function to add a salary column to the top_names 
# data frame from the previous question. Name the new data frame top_salary. 
# Use this code framework:
  
  
  # top_salary <- Salaries %>% filter(yearID == 2016) %>%
  # ______________ %>%
  # select(nameFirst, nameLast, teamID, HR, salary)

top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)

top_salary
# Which bind or join function fills the blank to generate the correct table?
# rbind(top_names)
# cbind(top_names)
# left_join(top_names)
# (True) right_join(top_names)
# full_join(top_names)
# anti_join(top_names)



###############################################################################
# Question 7
# Inspect the AwardsPlayers table. Filter awards to include only the year 2016.

# How many players from the top 10 home run hitters won at least one award in 2016?
top <- Batting %>% filter(yearID == 2016) %>% arrange(desc(HR)) %>% # arrange by descending HR count 
  slice(1:10) %>% inner_join(AwardsPlayers) %>% group_by(playerID) %>% 
  tally() %>% select(playerID) 
top %>% as_tibble() 

 # 3

# How many players won an award in 2016 but were not one of the top 10 home run 
# hitters in 2016?

awards <- AwardsPlayers %>% filter(yearID == 2016) %>% 
  group_by(playerID) %>% tally() %>% select(playerID) 
awards %>% as_tibble() 
setdiff(awards, top)

# 44
