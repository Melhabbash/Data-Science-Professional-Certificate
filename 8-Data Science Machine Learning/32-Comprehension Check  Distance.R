# 3. Nearest Neighbors  ---------------------------------------------------------------------


# 3.1 Distance ------------------------------------------------------------

#Question1

# Load the following dataset:
  
library(dslabs)
data("tissue_gene_expression")

# This dataset includes a matrix x:
dim(tissue_gene_expression$x)

# This matrix has the gene expression levels of 500 genes from 189 biological samples representing seven 
# different tissues. The tissue type is stored in y:
table(tissue_gene_expression$y)


# Which of the following lines of code computes the Euclidean distance between each observation and 
# stores it in the object d?

# d <- dist(tissue_gene_expression$x, distance='maximum')
# 
# d <- dist(tissue_gene_expression)
# 
#   (True)d <- dist(tissue_gene_expression$x)
# 
# d <- cor(tissue_gene_expression$x)

#########################################################################################################

#Question 2
# Using the dataset from Q1, compare the distances between observations 1 and 2 (both cerebellum),
# observations 39 and 40 (both colon), and observations 73 and 74 (both endometrium).
# 
# Distance-wise, are samples from tissues of the same type closer to each other than tissues of 
# different type?


ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]

image(as.matrix(d))
d_matrix <- as.matrix(d)
d_matrix[1:2,1:2]
d_matrix[39:40,39:40]
d_matrix[73:74,73:74]

#Answer : 
#Yes, the samples from the same tissue type are closest to each other
##########################################################################################################

#Question3
#Make a plot of all the distances using the image function to see if the pattern you observed in Q2 is general.
#Which code would correctly make the desired plot?

# image(d)
#  
#  (True) image(as.matrix(d))
# 
# d
# 
# image()



image(as.matrix(d))
#image(d_matrix)[order(tissue_gene_expression$y):order(tissue_gene_expression$y)]

# Explanation
# When we examine the plot, we do see that the pattern holds and that samples from the same tissue 
# are closest to each other, although there do appear to be some additional close distances between
# tissue types as well.