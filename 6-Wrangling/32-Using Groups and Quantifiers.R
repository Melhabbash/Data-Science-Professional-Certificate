# Case 1
# For case 1, if we add a '0 to, for example, convert all 6 to 6'0, then our pattern will match. 
# This can be done using groups using the following code:
  
yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes, no)
s
str_replace(s, "^([4-7])$", "\\1'0")

# The pattern says it has to start (^), be followed with a digit between 4 and 7, and then 
# end there ($). The parenthesis defines the group that we pass as \\1 to the replace regex.
############################################################################################
# Cases 2 and 4

# We can adapt this code slightly to handle case 2 as well which covers the entry 5'. 
# Note that the 5' is left untouched by the code above. This is because the extra ' makes the 
# pattern not match since we have to end with a 5 or 6. 
# To handle case 2, we want to permit the 5 or 6 to be followed by no or one symbol for feet. 
# So we can simply add '{0,1} after the ' to do this. 
# We can also use the none or once special character ?. 
# As we saw previously, this is different from * which is none or more. We now see that this code 
# also handles the fourth case as well:

str_replace(s, "^([56])'?$", "\\1'0")

# Note that here we only permit 5 and 6 but not 4 and 7. This is because heights of exactly 5 and 
# exactly 6 feet tall are quite common, so we assume those that typed 5 or 6 really meant 
# either 60 or 72 inches. However, heights of exactly 4 or exactly 7 feet tall are so rare that, 
# although we accept 84 as a valid entry, we assume that a 7 was entered in error.
################################################################################################

# Case 3
# We can use quantifiers to deal with  case 3. These entries are not matched because the inches 
# include decimals and our pattern does not permit this. We need allow the second group to include 
# decimals and not just digits. This means we must permit zero or one period . followed by zero 
# or more digits. So we will use both ? and *. Also remember that for this particular case, 
# the period needs to be escaped since it is a special character (it means any character except 
# a line break).
# So we can adapt our pattern, currently ^[4-7]\\s*'\\s*\\d{1,2}$, to permit a decimal at the end:
  
pattern <- "^[4-7]\\s*'\\s*(\\d+\\.?\\d*)$"

###############################################################################################
# Case 5
# Case 5, meters using commas, we can approach similarly to how we converted the x.y to x'y. 
# A difference is that we require that the first digit is 1 or 2:

yes <- c("1,7", "1, 8", "2, " )
no <- c("5,8", "5,3,2", "1.7")
s <- c(yes, no)
s
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")

# We will later check if the entries are meters using their numeric values.

################################################################################################
# Trimming
# In general, spaces at the start or end of the string are uninformative. These can be 
# particularly deceptive because sometimes they can be hard to see:

s <- "Hi "
cat(s)
identical(s, "Hi")
# This is a general enough problem that there is a function dedicated to removing them: str_trim.

str_trim("5 ' 9 ")
# To upper and to lower case
# One of the entries writes out numbers as words: Five foot eight inches. 
# Although not efficient, we could add 12 extra str_replace to convert zero to 0, one to 1, 
# and so on. To avoid having to write two separate operations for Zero and zero, One and one, etc.,
# we can use the str_to_lower() function to make all words lower case first:

s <- c("Five feet eight inches")
str_to_lower(s)
# Putting it into a function
# We are now ready to define a procedure that handles converting all the problematic cases.

# We can now put all this together into a function that takes a string vector and tries to convert 
# as many strings as possible to a single format. Below is a function that puts together the 
# previous code replacements:

convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
  str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
  str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
  str_trim() #remove extra space
}

# We can also write a function that converts words to numbers:
  
  words_to_numbers <- function(s){
    str_to_lower(s) %>%  
      str_replace_all("zero", "0") %>%
      str_replace_all("one", "1") %>%
      str_replace_all("two", "2") %>%
      str_replace_all("three", "3") %>%
      str_replace_all("four", "4") %>%
      str_replace_all("five", "5") %>%
      str_replace_all("six", "6") %>%
      str_replace_all("seven", "7") %>%
      str_replace_all("eight", "8") %>%
      str_replace_all("nine", "9") %>%
      str_replace_all("ten", "10") %>%
      str_replace_all("eleven", "11")
  }

  # Now we can see which problematic entries remain:
  
  converted <- problems %>% words_to_numbers %>% convert_format
remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]
