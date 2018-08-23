rm(list = ls())

#loading the data set
df = read.csv(file = 'Mall_Customers.csv')
X = df[4:5]
###################################
# K-Means Clustering
###################################
# Using the elbow method to find the number of clusters
set.seed(1)
wcss = vector() #within cluster sum of squares
for (i in 1:10) {
  wcss[i] = sum(kmeans(X, i)$withinss)
}

plot(1:10, wcss, type = 'b', main = paste('The Elbow Method'), xlab = 'Number of clusters', ylab = 'WCSS')

# Fitting K-Means to the dataset
set.seed(101)
kmeans = kmeans(x = X, centers = 5)
y_kmeans = kmeans$cluster

# Visualising the clusters
#install.packages('cluster')
library(cluster)
clusplot(X, y_kmeans, lines = 0, shade = TRUE, color = TRUE, labels = 2, plotchar = FALSE, span = TRUE,
         main = paste('Clusters of customers'),xlab = 'Annual Income', ylab = 'Spending Score')

##########################################################################
#Hierarchical Clustering
##########################################################################

# Using the dendrogram to find the optimal number of clusters
dendrogram = hclust(d = dist(X, method = 'euclidean'), method = 'ward.D')
plot(dendrogram, main = paste('Dendrogram'), xlab = 'Customers',ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the dataset
hc = hclust(d = dist(X, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 5)

clusplot(X, y_hc, lines = 0, shade = TRUE, color = TRUE, labels = 2, plotchar = FALSE, span = TRUE,
         main = paste('Clusters of customers'),xlab = 'Annual Income', ylab = 'Spending Score')
