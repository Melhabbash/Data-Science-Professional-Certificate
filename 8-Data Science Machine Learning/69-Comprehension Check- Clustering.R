###################################################################################################
# These exercises will work with the tissue_gene_expression dataset, which is part of the dslabs 
# package.
###################################################################################################

# Q1
# Load the tissue_gene_expression dataset. Remove the row means and compute the distance between 
# each observation. Store the result in d.

# Which of the following lines of code correctly does this computation?
  
# d <- dist(tissue_gene_expression$x)
# 
# d <- dist(rowMeans(tissue_gene_expression$x))
# 
# d <- dist(rowMeans(tissue_gene_expression$y))
# 
# (True) d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))

d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))
###################################################################################################
# Question 2
# Make a hierarchical clustering plot and add the tissue types as labels. You will observe
# multiple branches. 
# Q: Which tissue type is in the branch farthest to the left? Liver
  
# Explanation
# The plot can be made using the following code:
  
  h <- hclust(d)
 plot(h)

###################################################################################################
# Question 3
# Run a k-means clustering on the data with K=7. Make a table comparing the identified clusters 
# to the actual tissue types. Run the algorithm several times to see how the answer changes. 
# Q: What do you observe for the clustering of the liver tissue?
# A: Liver is split into two clusters (one large and one small) about 60% of the time. The other 40% of the time it is either in a single cluster or in three clusters at roughly equal frequency.

cl <- kmeans(tissue_gene_expression$x, centers = 7)
table(cl$cluster, tissue_gene_expression$y)


###################################################################################################
# Question 4
# Select the 50 most variable genes. Make sure the observations show up in the columns, that 
# the predictor are centered, and add a color bar to show the different tissue types.
# Hint: use the ColSideColors argument to assign colors.
# Also, use col=RColorBrewer::brewer.pal(11,"RdBu") for a better use of colors. 
# Part of the code is provided for you here:

library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
#BLANK

# Which line of code should replace #BLANK in the code above?
# 
# (True) heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)
# 
# heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = rev(colors))
# 
# heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = sample(colors))
# 
# heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = sample(colors))


library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
#BLANK
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)
#BLANK