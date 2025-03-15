library(dplyr)
library(tidyverse)
library(factoextra)
library(cluster)
library(ggplot2)
library(plotly)
library(dbscan)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data_path <- "Mall_Customers.csv"

data_preprocessing <- function(data_path) {
  dataframe <- read.csv(data_path)
  return(dataframe)
}

dataframe = data_preprocessing(data_path)
dataframe
dim(dataframe)


colnames(dataframe) <- c("CustomerID", "Genre", "Age", "Annual_Income", "Spending_Score")

dataframe <- dataframe %>%
  mutate(Genre = ifelse(Genre == "Male", 0, 1))

clustering_data <- dataframe %>%
  select(Genre, Annual_Income, Spending_Score)


num_columns <- sapply(clustering_data, is.numeric)

dataframe
clustering_data_scaled <- clustering_data
clustering_data_scaled[, num_columns] <- scale(clustering_data[, num_columns])

dimension_before_outlier_det = dim(clustering_data_scaled)

# Outlier analysis & detection
clustering_data_scaled <- clustering_data_scaled %>%
  select(Annual_Income, Spending_Score)

dbscan_result <- dbscan(clustering_data_scaled, eps = 0.5, minPts = 8)

dataframe$Anomaly <- ifelse(dbscan_result$cluster == 0, "Outlier", "Not Outlier")

dataframe_no_outliers <- dataframe %>%
  filter(Anomaly == "Not Outlier")

ggplot(dataframe, aes(x = Annual_Income, y = Spending_Score, color = Anomaly)) +
  geom_point(size = 3) +
  labs(title = "Outlier Detection using DBSCAN",
       x = "Annual Income",
       y = "Spending Score") +
  scale_color_manual(values = c("Outlier" = "red", "Not Outlier" = "steelblue")) +
  theme_minimal()


dataframe_no_outliers <- dataframe_no_outliers %>%
  select(-Anomaly)

clustering_data_no_outliers <- dataframe_no_outliers %>%
  select(Genre, Annual_Income, Spending_Score)

clustering_data_no_outliers_scaled <- clustering_data_no_outliers
clustering_data_no_outliers_scaled[] <- scale(clustering_data_no_outliers)
head(clustering_data_no_outliers_scaled)

# N_start used
clustring_data_scaled = clustering_data_no_outliers_scaled
kmeans_result <- kmeans(clustering_data_no_outliers_scaled, centers = 4, nstart = 25)
clustering_data_no_outliers_scaled$Cluster <- kmeans_result$cluster


# Silhouette Scores
silhouette_scores <- silhouette(kmeans_result$cluster, dist(clustering_data_no_outliers_scaled))
fviz_silhouette(silhouette_scores, 
                title = "Silhouette Plot for K-means Clustering") +
  theme_minimal() + 
  theme(axis.text.x = element_blank())

# Centroids
centroids <- as.data.frame(kmeans_result$centers)
centroids$Cluster <- as.factor(1:nrow(centroids))
centroids <- centroids[!duplicated(centroids), ]

# 3D Plot
plot <- plot_ly(clustering_data_no_outliers_scaled, 
                x = ~Annual_Income, 
                y = ~Spending_Score, 
                z = ~Genre,  
                color = ~as.factor(Cluster), 
                type = "scatter3d", 
                mode = "markers") %>%
  add_trace(data = centroids,
            x = ~Annual_Income,
            y = ~Spending_Score,
            z = ~Genre,  
            type = "scatter3d",
            mode = "markers",
            marker = list(size = 10, color = 'black', symbol = 'x'),
            name = "X - Centroid",  
            showlegend = FALSE) %>%  
  layout(title = "3D Clustering KMeans with Centroids",
         scene = list(xaxis = list(title = "Annual Income"),
                      yaxis = list(title = "Spending Score"),
                      zaxis = list(title = "Genre")))
plot