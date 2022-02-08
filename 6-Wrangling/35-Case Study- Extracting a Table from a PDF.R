# One of the datasets provided in dslabs shows scientific funding rates by gender in the Netherlands:
library(tidyverse)  
library(dslabs)
data("research_funding_rates")
research_funding_rates 

# The data come from a paper External link published in the prestigious journal PNAS. However, 
# the data are not provided in a spreadsheet; they are in a table in a PDF document. 
# We could extract the numbers by hand, but this could lead to human error. 
# Instead we can try to wrangle the data using R.

#############################################################################################
# Downloading the data
# We start by downloading the PDF document then importing it into R using the following code:
  
library("pdftools")
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
txt
file.remove(temp_file)

# If we examine the object text we notice that it is a character vector with an entry for each 
# page. So we keep the page we want using the following code:
  
raw_data_research_funding_rates <- txt[2]

# The steps above can actually be skipped because we include the raw data in the dslabs package 
# as well:
  
data("raw_data_research_funding_rates")
#################################################################################################
# 
# Looking at the download
# Examining this object,
# 
raw_data_research_funding_rates %>% head
# 
# we see that it is a long string. Each line on the page, including the table rows, 
# is separated by the symbol for newline: \n.
# 
# We can therefore can create a list with the lines of the text as elements:
  
  tab <- str_split(raw_data_research_funding_rates, "\n")
  
# Because we start off with just one element in the string, we end up with a list with just 
# one entry:
# 
tab <- tab[[1]]
# 
# By examining this object,
# 
tab %>% head
# 
# we see that the information for the column names is the third and fourth entires:
  
the_names_1 <- tab[3]
the_names_2 <- tab[4]
# 
# In the table, the column information is spread across two lines. We want to create one vector 
# with one name for each column. We can do this using some of the functions we have just learned.

# ##############################################################################################
# Extracting the table data
# Let's start with the first line:
# 
the_names_1
# 
# We want to remove the leading space and everything following the comma. We can use regex for 
# the latter. Then we can obtain the elements by splitting using the space. 
# We want to split only when there are 2 or more spaces to avoid splitting success rate. 
# So we use the regex \\s{2,} as follows:

the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_1
# 
# Now let's look at the second line:
# 
  the_names_2
# 
# Here we want to trim the leading space and then split by space as we did for the first line:
  
  the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2
# 
# Now we can join these to generate one name for each column:
  
  tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names
# 
# Now we are ready to get the actual data. By examining the tab object, we notice that the 
# information is in lines 6 through 14. We can use str_split() again to achieve our goal:
  
new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% head()

# We can see that the objects are identical:
  
identical(research_funding_rates, new_research_funding_rates)
