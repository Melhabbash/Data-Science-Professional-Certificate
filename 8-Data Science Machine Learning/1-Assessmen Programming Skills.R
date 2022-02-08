# This problem set is designed to check whether you have the basic R knowledge to follow 
# along with this course. 
# 
# This does not count towards your grade, but it can help you decide whether you have 
# the background knowledge to succeed in this course at the moment. 
# If you have followed the HarvardX Data Science series so far, these should be familiar 
# programming tasks.
# 
# If you struggle with this assessment, you can learn these skills by revisiting earlier 
# courses in the series, starting with R Basics.
# 
# This problem set uses the heights dataset from the dslabs package, which consists of actual 
# heights (in inches) of students in 3 Harvard biostatistics courses.
# 
# Install the dslabs package from CRAN, then load the dslabs package into your workspace with the 
# library() command.
# 
# After loading the package, load the dataset heights into your workspace:
  
  data(heights)
  heights
################################################################################  
# Q1: Object Classes
  install.packages("dslabs")
  library(dslabs)
  data(heights)
  class(heights)
  class(heights$sex)
  class(heights$height)
  class("Male")
  class(75.00000)
  
#   Match each object to its corresponding class.
#   
#   heights dataset
#data.frame
  
#   sex column
#factor vector
  
#   height column
#numeric vector
  
#   "Male"
#character string
  
#   75.00000
#numeric   
################################################################################    
# Q2: Object Dimensions
#   How many rows are in this dataset?
  nrow(heights)
#1050   
################################################################################   
# Q3: Indexing - 1
#   What is the height in row 777?
  heights$height[777]
# 61
################################################################################ 
# Q4: Indexing - 2
#   Which of these pieces of code returns the sex in row 777?
# 
  heights$sex[777] #true
  heights[1, 777]
  heights[777,1]   #true
# 
################################################################################   
# Q5: Maximum and Minimum
#   What is the maximum height in inches?
max(heights$height)  
#   
#   Which row has the minimum height?
  which.min(heights$height) 
# 1032
################################################################################   
# Q6: Summary Statistics
#   What is the mean height in inches?
mean(heights$height)     
#68.32301   
#   What is the median height in inches?
median(heights$height)   
# 68.5  
################################################################################   
#   Q7: Conditional Statements- 1
#   What proportion of individuals in the dataset are male?
sum(heights$sex == 'Male') / length(heights$sex)
# or
mean(heights$sex == "Male")
# 77.33333
################################################################################ 
#   Q8: Conditional Statements - 2
#   How many individuals are taller than 78 inches (roughly 2 meters)?
sum(heights$height > 78)
# 9
################################################################################   
#   Q9: Conditional Statements - 3
#   How many females in the dataset are taller than 78 inches?
x <- heights$height[heights$sex=="Female"]
sum(x>78)
