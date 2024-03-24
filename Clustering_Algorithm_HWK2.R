title: "Clustering_Algorithm"
author: "Elizabeth Dada"
date: "03-15-2024"

## Step 1: Set Up
#load the necessary pakages
library(cluster)     # For k-means clustering
library(factoextra)  # For visualization and silhouette analysis
library(utils)
library(stats)
library(tidyverse)


## Step 2: LOAD DATA Load and Prepare the Data
# Load required downloaded dataset
X3cluster_data <- read.csv("~/Documents/ELEG_6380_Homework2/3cluster_data.csv")

## Read in dataset
base::file.choose(X3cluster_data)
utils::read.csv(file.choose(X3cluster_data), header = TRUE)


## Plot the Data
## plot the data
df <- X3cluster_data
plot( X3cluster_data)

## Plot the cluster data in color
plot(X3cluster_data, col = fitK$cluster)

# Remove any rows with missing values
X3cluster_data <- na.omit(X3cluster_data)


# Scale the variables (if needed)
# X3cluster_scaled <- scale(X3cluster_data[, c("X", "Y")])

# Normalizing Data Distribution: Scaling and Fitting
X3cluster_dataScaled <- scale(X3cluster_data[, -3])
X3cluster_dataScaled


# Create a scatter plot
plot(X3cluster_scaled, main = "Scatter Plot",
     xlab = "X-axis", 
     ylab = "Y-axis")
points(centers_scaled, col = "red", pch = 19, cex = 2)


### QUESTION 1: PERFORM K-MEANs CLUSTERING ALGORITHM

# Using kmeans() function to perform K-means clustering
# Perform K-means clustering with K=3
k <- 3

# 'kmeans_result' should contain the cluster assignments (kmeans_result$cluster)
# and the data points (df[, c("X", "Y)])
kmeans_result <- kmeans(df[, c("X", "Y")], centers = k, nstart = 150)

# Compute Euclidean distances
dist_matrix <- dist(data[, c("X", "Y")], method = "euclidean")

# Perform k-means clustering with 3 clusters
k_means_result <- kmeans(as.matrix(distance_matrix), centers = 3, nstart = 25)
print(k_means_result) 

## Iterate and choose K = 3
k <- list()
for(i in 1:10){
  k[[i]] <- kmeans(X3cluster_dataScaled, i)
}

# choose k = 3
k


# Fitting the model select k=3
fitK <- kmeans(X3cluster_dataScaled, 3)
fitK

# Create a scatter plot
fviz_cluster(kmeans_result, data = X3cluster_scaled,
             geom = "point", ellipse.type = "norm",
             ggtheme = theme_minimal())
  
# compute sum of square error
  betweenss_totss <- list()
  for (i in 1:10){
    betweenss_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss
  }
  
# plot the Sum of squares 
  plot(1:10, betweenss_totss, type = "b",
       ylab = "Between SS / Total SS", 
       xlab = "Clusters (k)") 


## PLOT K-MEANS CLUSTERS RESULTs

# Plot the data points colored by cluster
library(ggplot2)
ggplot(X3cluster_data, aes(x = X, y = Y, color = factor(cID))) + geom_point(size = 3) + 
  scale_color_manual(values = c("red", "blue", "green")) + 
  labs(title = "K-means Clustering (K = 3)",
       x = "X",
       y = "Y") +
  theme_minimal()

# Fit the k-means model
# df is a numeric matrix with appropriate columns
# Set the desired number of clusters
k <- 3  


kmean_result <- kmeans(df, centers = k)

# Get the Cluster Centers (centroids)
centers <- paste("Cluster", 0: (k-1))
# Set row names for the centers 
centers <- paste("Cluster", 0:(k-1))

# Print the cluster centers
print(centers)


## Question 1A: Compute Silhouette Score 
# Compute silhouette scores for each sample
silhouette_scores <- silhouette(kmeans_result$cluster, dist(df[, c("X", "Y")]))

# Calculate the Average silhouette width for the clustering results
silhouette_avg <- mean(silhouette_scores[, 3]) 
# Print the average silhouette width for the clustering results
cat("Average Silhouette Width:", silhouette_avg, "\n")


## Question 1B: Compute Entropy 
# compute entropy as defined in the lecture notes
cluster_counts <- table(kmeans_result$cluster)
total_samples <- nrow(df)
entropy <- sum((cluster_counts / total_samples) * log2(cluster_counts / total_samples))
#Print the entropy
cat("Entropy:", entropy, "\n")


### QUESTION 2: PERFORMING HIERARCHICAL CLUSTERING

# load cluster package
library(cluster)

## Single Linkage
# Perform single linkage hierarchical clustering
single_linkage_result <- hclust(dist(X3cluster_data[, c("X", "Y")]), method = "single")
# Plot the dendrogram
plot(single_linkage_result, main = "Single Linkage Dendrogram")

## Average Linkage
# Perform Average linkage hierarchical clustering
average_linkage_result <- hclust(dist(X3cluster_data[, c("X", "Y")]), method = "average")
# Plot the dendrogram
plot(average_linkage_result, main = "Average Linkage Dendrogram")

## Complete Linkage
# Perform complete linkage hierarchical clustering
complete_linkage_result <- hclust(dist(X3cluster_data[, c("X", "Y")]), method = "complete")
# Plot the dendrogram
plot(complete_linkage_result, main = "Complete Linkage Dendrogram")


### QUESTION 2A:Silhouette Scores for the hierarchical Clestering Samples and Average Silhouette Width

# cut the dendrogram to obtain clusters
# Adjust the height parameter (h) to control the number of clusters
complete_linkage_result <- hclust(dist(X3cluster_data[, c("X", "Y")]), method = "complete")
plot(complete_linkage_result, main = "Complete Linkage Dendrogram")



### QUESTION 2:  HIERARCHICAL CLUSTERING (using combined code)
# Perform hierarchical clustering using different linkage methods
hc_single <- hclust(dist(data[, c("X", "Y")]), method = "single")
hc_average <- hclust(dist(data[, c("X", "Y")]), method = "average")
hc_complete <- hclust(dist(data[, c("X", "Y")]), method = "complete")
plot(single_linkage_result, main = "Single Linkage Dendrogram")
plot(average_linkage_result, main = "Average Linkage Dendrogram")
plot(complete_linkage_result, main = "Complete Linkage Dendrogram")

## QUESTION #2A: SILHOUETTE SCORE AND AVERAGE SILHOUETTE WIDTH
# Compute silhouette scores for each sample
sil_single <- silhouette(cutree(hc_single, k = 3), dist(data[, c("X", "Y")]))
sil_average <- silhouette(cutree(hc_average, k = 3), dist(data[, c("X", "Y")]))
sil_complete <- silhouette(cutree(hc_complete, k = 3), dist(data[, c("X", "Y")]))

# Calculate average silhouette widths
avg_sil_single <- mean(sil_single[, "sil_width"])
avg_sil_average <- mean(sil_average[, "sil_width"])
avg_sil_complete <- mean(sil_complete[, "sil_width"])

# Print results for each method
cat("Single Linkage - Average Silhouette Width:", avg_sil_single, "\n")
cat("Average Linkage - Average Silhouette Width:", avg_sil_average, "\n")
cat("Complete Linkage - Average Silhouette Width:", avg_sil_complete, "\n")

## QUESTION 2B: ENTROPY
# Compute Enthropy as defined in the lecture notes
cat("Entropy:", entropy, "\n")


### QUSTION 3: DETERMINING THE BEST CLUSTERING RESULT.


## K-means Clustering:
# The average silhouette width computed for K-means clustering is 0.4772539.
# The entropy for K-means clustering is -1.584389 

## Hierarchical Clustering:
# The average silhouette width for hierarchical clustering is 0.4619243.
# The entropy for hierarchical clustering is also -1.584389. 

# Now, to compare the two methods:

# Comparing the Average Silhouette Width:
# A higher silhouette width indicates better separation between clusters.
# Both K-means and hierarchical clustering have similar silhouette widths, 
# but k-means clustering method has a slightly higher value of silhouette width 
# than the hierarchical clustering method.
# Therefore, in terms of silhouette width, k-means clustering method 
# performs slightly better.

# Comparing the Entropies:
# Entropy measures the disorder or randomness within clusters.
# Both K-means and hierarchical clustering have the same entropy value.
# Therefore, in terms of entropy, there is no significant difference between 
# the two clustering methods.

## Justification:

# Since the silhouette width is slightly higher for K-means clustering method,
# this suggests that k-means clustering method is a better separation of clusters.
# Additionally, hierarchical clustering has the same entropy as K-means, 
# indicating similar cluster quality.
# Considering both factors, k-means clustering algorithm is the preferred 
# choice for the X3cluster_data.



