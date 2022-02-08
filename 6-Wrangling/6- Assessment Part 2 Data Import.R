
# In this part of the assessment, you will import real datasets and learn more 
# about useful arguments to readr functions. You will encounter common issues 
# that arise when importing raw data. This part of the assessment will require 
# you to program in R.

# Use the readr package in the tidyverse library:
  
library(tidyverse)

#############################################################################
# Question 14
# 0.0/1.0 point (graded)
# Inspect the file at the following URL:
#   
# https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data 
# Which readr function should be used to import this file?
# read_table()
# read_csv()
# read_csv2()
# read_tsv()
# None of the above

url<- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
data<- read_csv(url)

##################################################################
Question 15
0.0/1.0 point (graded)
Check the documentation for the readr function you chose in the previous question to learn about its arguments. Determine which arguments you need to the file from the previous question:
  
  url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
Does this file have a header row? Does the readr function you chose need any additional arguments to import the data correctly?
  Yes, there is a header. No arguments are needed.
Yes, there is a header. The header=TRUE argument is necessary.
Yes, there is a header. The col_names=TRUE argument is necessary.
No, there is no header. No arguments are needed.
No, there is no header. The header=FALSE argument is necessary.
No, there is no header. The col_names=FALSE argument is necessary.
unanswered
SaveSave your answer
Submit
You have used 0 of 2 attemptsSome problems have options such as save, reset, hints, or show answer. These options follow the Submit button.
Question 16
0.0/2.0 points (graded)
Inspect the imported data from the previous question.

How many rows are in the dataset?
  unanswered 

How many columns are in the dataset?
  unanswered 

SaveSave your answer
Submit
You have used 0 of 10 attempts