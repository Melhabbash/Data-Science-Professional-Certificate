
# Introduction: Questions 1-3
# Load the following web page, which contains information about Major League Baseball 
# payrolls, into R: https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm 

 library(rvest)
 url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
 h <- read_html(url)

# We learned that tables in html are associated with the table node.  
# Use the html_nodes() function and the table node type to extract the first table. 
# Store it in an object nodes:
  
  nodes <- html_nodes(h, "table")
  
# The html_nodes() function returns a list of objects of class xml_node. 
# We can see the content of each one using, for example, the html_text() function. 
# You can see the content for an arbitrarily picked component like this:
  
  html_text(nodes[[8]])
  
# If the content of this object is an html table, we can use the html_table() function 
# to convert it to a data frame:
  
  html_table(nodes[[8]])
  
# You will analyze the tables from this HTML page over questions 1-3.

#######################################################################################
  
# Question 1
# Many tables on this page are team payroll tables, with columns for rank, team, 
# and one or more money values.

# Convert the first four tables in nodes to data frames and inspect them.

# Which of the first four nodes are tables of team payroll?
# Check all correct answers. Look at table content, not column names.
  lapply(nodes[1:10], html_table)

# None of the above
# Table 1
# (True)Table 2
# (True)Table 3
# Table 4


#####################################################################################
# Question 2
# For the last 3 components of nodes, which of the following are true? 
#   (Check all correct answers.)
# Check all correct answers.
  
  length_n <- length(nodes)
  length_n
  length_n-3
  html_table(nodes[length_n])
  lapply(nodes[(length_n-3):length_n], html_table)
# 
# (True) All three entries are tables.
# All three entries are tables of payroll per team.
# (True) The last entry shows the average across all teams through time, not payroll per team.
# None of the three entries are tables of payroll per team.


# #####################################################################################
# Question 3
# Create a table called tab_1 using entry 10 of nodes. Create a table called tab_2 using 
# entry 19 of nodes.
# 
# Note that the column names should be c("Team", "Payroll", "Average"). You can see that 
# these column names are actually in the first data row of each table, and that tab_1 has 
# an extra first column No. that should be removed so that the column names for 
# both tables match.
# 
# Remove the extra column in tab_1, remove the first row of each dataset, and change 
# the column names for each table to c("Team", "Payroll", "Average"). 
# Use a full_join() by the Team to combine these two tables.
  tab_1<- html_table(nodes[10])
  tab_1<-as.data.frame(tab_1)
  tab_1
  tab_1<- tab_1[-1,-1]
  tab_1
  names(tab_1) <- c("Team", "Payroll", "Average")
  tab_1
  tab_2<- html_table(nodes[19])
  tab_2<-as.data.frame(tab_2)
  tab_2
  tab_2<- tab_2[-1,]
  tab_2
  names(tab_2) <- c("Team", "Payroll", "Average")
  tab_2
  result<-full_join(tab_1,tab_2, by = "Team") 
  result
  nrow(result)
  # How many rows are in the joined data table?
   
# #######################################################################################
# Introduction: Questions 4 and 5
# The Wikipedia page on opinion polling for the Brexit referendum External link, 
# in which the United Kingdom voted to leave the European Union in June 2016, contains 
# several tables. One table contains the results of all polls regarding the 
# referendum over 2016:
#   
# Wikipedia table of Brexit poll results in 2016. The name of the first column 
# is "Date(s) conducted". There are several other columns, including percentages of 
# poll respondents choosing "Remain" or "Leave", the pollster, and the poll type 
# (online/telephone).
# 
# Use the rvest library to read the HTML from this Wikipedia page 
# (make sure to copy both lines of the URL):
#   
library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"

#######################################################################################
# Question 4
# Assign tab to be the html nodes of the "table" class.
h <- read_html(url)
tab <- html_nodes(h, "table")
nodes <- html_nodes(h, "table")
nodes
length(nodes)
# How many tables are in this Wikipedia page?
# 40 

#######################################################################################
# Question 5
# Inspect the first several html tables using html_table() with the argument fill=TRUE 
# (you can read about this argument in the documentation). Find the first table that 
# has 9 columns with the first column named "Date(s) conducted".
#

f<-html_table(tab[[1]],fill = TRUE)
names(f)

f<-html_table(tab[[2]],fill = TRUE)
names(f)

f<-html_table(tab[[3]],fill = TRUE)
names(f)

f<-html_table(tab[[4]],fill = TRUE)
names(f)

f<-html_table(tab[[5]],fill = TRUE)
names(f)

# What is the first table number to have 9 columns where the first column is named 
# "Date(s) conducted"?

# 5



