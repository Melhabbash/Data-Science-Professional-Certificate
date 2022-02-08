###################################################################################################

#Question 6: PCA: proportion of variance
#Perform a principal component analysis of the scaled matrix.
pca <- prcomp(x_scaled)
which(pca$importance[3,] > 0.7)[1]
#What proportion of variance is explained by the first principal component?  0.443
summary(pca)    # see PC1 Cumulative Proportion  
#How many principal components are required to explain at least 90% of the variance? 0.9101
summary(pca)     # first value of Cumulative Proportion that exceeds 0.9: 7

###################################################################################################
#Question 7: PCA: plotting PCs
#Plot the first two principal components with color representing tumor type (benign/malignant).
# Which of the following is true?

# Malignant tumors tend to have smaller values of PC1 than benign tumors.
# (True) Malignant tumors tend to have larger values of PC1 than benign tumors.
# Malignant tumors tend to have smaller values of PC2 than benign tumors.
# Malignant tumors tend to have larger values of PC2 than benign tumors.
# There is no relationship between the first two principal components and tumor type.

#plot t just the first 2 principal components.
data.frame(pca$x[,1:2], type = brca$y) %>%
  ggplot(aes(PC1, PC2, color = type)) +
  geom_point()
# From the plot, you can see that the benign tumors tend to have smaller values of PC1 and that 
# the malignant tumors have larger values of PC1. PC2 values have a similar spread for both 
# benign and malignant tumors.

###################################################################################################

#Question 8: PCA: PC boxplot
# Make a boxplot of the first 10 PCs grouped by tumor type.
# Which PCs are significantly different enough by tumor type that there is no overlap in the 
# interquartile ranges (IQRs) for benign and malignant samples?
# Select ALL that apply.

# (True) PC1
# PC2
# PC3
# PC4
# PC5
# PC6
# PC7
# PC8
# PC9
# PC10

data.frame(type = brca$y, pca$x[,1:10]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot()

# When you look at the boxplot, you can see that the IQRs overlap for PCs 2 through 10 but 
# not for PC1.

###################################################################################################

