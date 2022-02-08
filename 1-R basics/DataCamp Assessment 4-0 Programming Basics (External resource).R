
x <- c(1,2,-3,4)
if(all(x>0)){
  print("All Positives")
} else{
  print("Not All Positives")
}

#Conditional continued
#Which of the following expressions is always FALSE when at least one entry of 
#a logical vector x is TRUE? 
#You can try examples in the R console.
x<- c(TRUE,TRUE)
all(x)
any(x)
any(!x)
all(!x)

#ifelse
#The function nchar tells you how many characters long a character vector is. 
#For example:
char_len <- nchar(murders$state)
head(char_len)
#The function ifelse is useful because you convert a vector of logicals into 
#something else. 
#For example, some datasets use the number -999 to denote NA. 
#A bad practice! You can convert the -999 in a vector to NA using the 
#following ifelse call:
  
x <- c(2, 3, -999, 1, 4, 5, -999, 3, 2, 9)
ifelse(x == -999, NA, x)
#If the entry is -999 it returns NA, otherwise it returns the entry.

ifelse(nchar(murders$state)>8,new_names<-murders$abb,new_names<-murders$state)



