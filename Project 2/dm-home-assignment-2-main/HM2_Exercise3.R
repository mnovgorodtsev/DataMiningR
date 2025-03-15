# Density based methods
library(ggplot2)
set.seed(123)

# Creating 2D datasets
easy_separable_df <- data.frame(
  x = c(rnorm(50, mean = 2.9), rnorm(50, mean = 8.2)),
  y = c(rnorm(50, mean = 3.2), rnorm(50, mean = 7.5))
)

easy_separable_df$cluster <- factor(c(rep(1, 50), rep(2, 50)))

# Dataset 2: Less separable clusters
less_easy_df <- data.frame(
  x = c(rnorm(50, mean = 5.2), rnorm(50, mean = 10), rnorm(50, mean = 15)),
  y = c(rnorm(50, mean = 7.8), rnorm(50, mean = 10), rnorm(50, mean = 11))
)

less_easy_df$cluster <- factor(c(rep(1, 50), rep(2, 50), rep(3, 50)))


# Visualizing Dataset 1
ggplot(easy_separable_df, aes(x = x, y = y, color = cluster)) +
  geom_point() +
  ggtitle("Dataset 1: Easy Separable Data") +
  scale_color_manual(values = c("lightcoral", "blue")) +  
  theme_minimal() +
  labs(color = "CLASS")

# Visualizing Dataset 2
ggplot(less_easy_df, aes(x = x, y = y, color = cluster)) +
  geom_point() +
  ggtitle("Dataset 2: Less Easy Separable Data") +
  scale_color_manual(values = c("lightcoral", "blue", "green")) +  
  theme_minimal() +
  labs(color = "CLASS")


# DBSCAN implementation

dis_euclidean = function(x, y) {
  x1 = x[1]
  x2 = x[2]
  y1 = y[1]
  y2 = y[2]
  
  distance = sqrt((x1 - y1)^2 + (x2 - y2)^2)
  
  return(distance)
}

check_area = function(point, data, epsilon, n_min_points){
  near_points = c()
  rows_number = nrow(data)
  for(i in 1:rows_number){
    distance = dis_euclidean(data[i, ], point)
    
    if(distance <= epsilon){
      near_points = c(near_points, i)
      }
    }
  
    if(length(near_points) >= n_min_points){
      return(near_points)
    }
    else{
      return(NULL)
    }
}


db_scan <- function(data, epsilon, n_min_points) {
  rows_number = nrow(data)
  labels = rep(0, rows_number)  # 0 - is unvisited
  class_id = 0
  
  for(i in 1:rows_number) {
    if(labels[i] == 0) {  
      near_points = check_area(data[i, ], data, epsilon, n_min_points)
      
      if(!is.null(near_points)) {
        class_id = class_id + 1  
        labels[i] = class_id
        points_to_check = near_points  

        while(length(points_to_check) > 0) {
          current_point = points_to_check[1]
          points_to_check = points_to_check[-1]  
          
          if(labels[current_point] == 0 || labels[current_point] == -1) {
            labels[current_point] = class_id 
            
            new_points_nearby = check_area(data[current_point, ], data, epsilon, n_min_points)
            if(!is.null(new_points_nearby)) {
              points_to_check = unique(c(points_to_check, new_points_nearby))  
            }
          }
        }
      } else {
        labels[i] = -1  #noise
      }
    }
  }
  
  return(labels)
}

# Parameters
epsilon = 1
n_min_points = 8

# Testing easy df

labels = db_scan(easy_separable_df, epsilon, n_min_points)
easy_separable_df$label = as.factor(labels)

ggplot(easy_separable_df, aes(x = x, y = y, color = label)) +
  geom_point() +
  ggtitle("Clustering with DBSCAN - Easy") +
  theme_minimal()  +
  labs(subtitle = paste("epsilon =", epsilon, "n_min_points =", n_min_points))


# Testing less easy df

labels = db_scan(less_easy_df, epsilon, n_min_points)
less_easy_df$label = as.factor(labels)

ggplot(less_easy_df, aes(x = x, y = y, color = label)) +
  geom_point() +
  ggtitle("Clustering with DBSCAN - Less Easy") +
  theme_minimal()  +
  labs(subtitle = paste("epsilon =", epsilon, "n_min_points =", n_min_points))