library(dplyr)
# Question 1
# Which of the following is NOT an application of string parsing?
#   
#   
# Removing unwanted characters from text.
# Extracting numeric values from text.
# (True) Formatting numbers and characters so they can easily be displayed in deliverables like papers 
# and presentations.
# Splitting strings into multiple values.

# Answer
# Correct:Formatting text and numbers for deliverables is not an application of string parsing. 
# String parsing is used as part of the data wrangling process.

##############################################################################################
# Question 2
# Which of the following commands would not give you an error in R?
#   
# (True)cat(" LeBron James is 6'8\" ")
cat(" LeBron James is 6'8\" ")
# 
# cat(' LeBron James is 6'8" ')

# 
# cat(` LeBron James is 6'8" `)
# 
# cat(" LeBron James is 6\'8" ")
#         

# Answer
# Correct:This would correctly print out your string. Because the string is enclosed in double 
# quotes, (""), you must use an escape character before the inches symbol (").

#############################################################################################
# Question 3
# Which of the following are advantages of the stringr package over string processing functions 
# in base R? Select all that apply.

#  Base R functions are rarely used for string processing by data scientists so it's not 
# worth learning them.
# (True) Functions in stringr all start with "str_", which makes them easy to look up using 
# autocomplete.
# (True) Stringr functions work better with pipes.
# (True) The order of arguments is more consistent in stringr functions than in base R.

#############################################################################################
# Question 4
# You have a data frame of monthly sales and profits in R:
# 
# head(dat)
# A tibble: 5 x 3

# Month     Sales     Profit 
# <chr>     <chr>     <chr>  
# January   $128,568  $16,234
# February  $109,523  $12,876
# March     $115,468  $17,920
# April     $122,274  $15,825
# May       $117,921  $15,437
# 
Month<- c("January","February","March","April","May" ) 
Sales<- c("$128,568","$109,523","$115,468","$122,274","$117,921")
Profit<- c("$16,234","$12,876","$17,920","$15,825","$15,437")
dat<-data.frame(Month,Sales,Profit)
dat
class(dat)
# Which of the following commands could convert the sales and profits columns to numeric? 
# Select all that apply.
   dat %>% mutate_at(2:3, parse_number)
   dat
   dat %>% mutate_at(2:3, as.numeric)
   dat
   dat %>% mutate_all(parse_number)
   dat
   dat %>% mutate_at(2:3, funs(str_replace_all(., c("\\$|,"), ""))) %>% 
          mutate_at(2:3, as.numeric)
   dat
#         

