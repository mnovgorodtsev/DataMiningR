library(scatterplot3d)

set.seed(123)
# Easy separable dataset
X1 = rnorm(100, mean = 10, sd = 0.5)
Y1 = rnorm(100, mean = 7, sd = 0.5)
Z1 = rnorm(100, mean = 8, sd = 0.5)
CLASS1 = rep(1, 100)

X2 = rnorm(100, mean = 20, sd = 0.5)
Y2 = rnorm(100, mean = 14, sd = 0.5)
Z2 = rnorm(100, mean = 10, sd = 0.5)
CLASS2 = rep(2, 100)

X3 = rnorm(100, mean = 5, sd = 0.5)
Y3 = rnorm(100, mean = 4, sd = 0.5)
Z3 = rnorm(100, mean = 10, sd = 0.5)
CLASS3 = rep(3, 100)


X1 = c(X1, X2, X3)
Y1 = c(Y1, Y2, Y3)
Z1 = c(Z1, Z2, Z3)
easy_data <- data.frame(X = X1, Y = Y1, Z = Z1)

CLASS = c(CLASS1, CLASS2, CLASS3)

color_palette = c("red", "green", "blue")
colors = color_palette[CLASS]

scatterplot3d(X1, Y1, Z1, color = colors, pch = 19, main = "Easy separable data #1", xlab = "X", ylab = "Y", zlab = "Z")
legend("topright", legend = c("Class 1", "Class 2", "Class 3"), col = color_palette, pch = 19)


# Hard separable dataset
X1 = rnorm(50, mean = 21, sd = 1.5)
Y1 = rnorm(50, mean = 9, sd = 0.5)
Z1 = rnorm(50, mean = 7, sd = 0.5)
CLASS1 = rep(1, 50)

X2 = rnorm(50, mean = 20, sd = 1.5)
Y2 = rnorm(50, mean = 8, sd = 0.5)
Z2 = rnorm(50, mean = 9, sd = 0.5)
CLASS2 = rep(2, 50)

X3 = rnorm(50, mean = 22, sd = 0.5)
Y3 = rnorm(50, mean = 10, sd = 1.5)
Z3 = rnorm(50, mean = 10, sd = 0.5)
CLASS3 = rep(3, 50)


X2 = c(X1, X2, X3)
Y2 = c(Y1, Y2, Y3)
Z2 = c(Z1, Z2, Z3)

hard_data <- data.frame(X = X2, Y = Y2, Z = Z2)
CLASS = c(CLASS1, CLASS2, CLASS3)

color_palette = c("red", "green", "blue")
colors = color_palette[CLASS]

scatterplot3d(X2, Y2, Z2, color = colors, pch = 19, main = "Hard separable data #1", xlab = "X", ylab = "Y", zlab = "Z")
legend("topright", legend = c("Class 1", "Class 2", "Class 3"), col = color_palette, pch = 19)

# Euclidean distance function
dis_euclidean = function(x, y) {
  sqrt(sum((x - y)^2))
}

# K-Means Algorithm
k_means_algorithm <- function(data, iteration = 100, k = 3) {
  chosen_k_points = data.frame()
  for (i in 1:k) {
    random_index = sample(1:nrow(data), 1)
    chosen_k_points = rbind(chosen_k_points, data[random_index, ])
  }
  cluster_assignments = rep(0, nrow(data))
  
  for (iter in 1:iteration) {
    print('Iterations in process...')  
    for (i in 1:nrow(data)) {
      distances = sapply(1:k, function(j) {
        dis_euclidean(data[i, ], chosen_k_points[j, ])
      })
      cluster_assignments[i] = which.min(distances)
    }
    
    for (j in 1:k) {
      points_in_cluster = data[cluster_assignments == j, ]
      if (nrow(points_in_cluster) > 0) {
        chosen_k_points[j, ] = colMeans(points_in_cluster)
      }
    }
  }
  
  return(list(centroids = chosen_k_points, clusters = cluster_assignments))
}

# Plot function for scatterplot3d
plot_clusters_3d_scatterplot3d <- function(data, clusters, title) {
  color_palette = c("red", "green", "blue")
  colors = color_palette[clusters]
  
  scatterplot3d(data$X, data$Y, data$Z,
                color = colors, pch = 19,
                main = title,
                xlab = "X-axis", ylab = "Y-axis", zlab = "Z-axis")
  
  legend("topright", legend = c("Cluster 1", "Cluster 2", "Cluster 3"),
         col = color_palette, pch = 19)
}

# Test K-Means
#output_list = k_means_algorithm(easy_data, iteration = 50, k = 4)
#plot_clusters_3d_scatterplot3d(easy_data, output_list$clusters, "K-Means Clustering")

# EM Algorithm
# Expectation - Maximization algorithm 
normal_density <- function(data, mean, covariance) {
  d <- ncol(data)
  diff <- sweep(data, 2, mean)
  inv_cov <- solve(covariance)
  det_cov <- det(covariance)
  norm_const <- 1 / ((2 * pi)^(d / 2) * sqrt(det_cov))
  exp_term <- apply(diff, 1, function(x) exp(-0.5 * t(x) %*% inv_cov %*% x))
  return(norm_const * exp_term)
}

em_algorithm <- function(data, k = 3, max_iter = 40, tol = 1e-6) {
  
  num_features <- ncol(data)
  num_points <- nrow(data)
  initial_indices <- sample(1:num_points, k)
  
  pi_k <- rep(1 / k, k)
  mu_k <- data[initial_indices, , drop = FALSE]
  
  sigma_k <- vector("list", k)
  for (x in 1:k) {
    sample_indices <- sample(1:num_points, num_points / k)
    sigma_k[[x]] <- cov(data[sample_indices, , drop = FALSE]) 
  }
  
  Q <- numeric(max_iter)
  iteration_counter <- 2
  
  for (iter in 1:max_iter) {
    # E-step: Calculate responsibilities
    responsibilities <- matrix(0, num_points, k)
    for (x in 1:k) {
      density_values <- normal_density(data, mean = mu_k[[x]], covariance = sigma_k[[x]])
      responsibilities[, x] <- pi_k[x] * density_values
    }
    
    responsibilities_sum <- rowSums(responsibilities)
    responsibilities <- responsibilities / responsibilities_sum
    
    clusters <- apply(responsibilities, 1, which.max)
    
    for (x in 1:k) {
      weight_sum <- sum(responsibilities[, x])
      pi_k[x] <- weight_sum / num_points
      
      weighted_data <- responsibilities[, x] * data
      mu_k[[x]] <- colSums(weighted_data) / weight_sum
      
      sigma_k[[x]] <- cov.wt(data, wt = responsibilities[, x], center = mu_k[[x]])$cov
    }
    
    Q[iteration_counter] <- sum(log(responsibilities_sum))
    
    
    if (abs(Q[iteration_counter] - Q[iteration_counter - 1]) < tol) {
      break
    }
    
    # adding the animation for each 5
    if (iter <= 25 && iter %% 5 == 0) {
      scatterplot3d(data[,1], data[,2], data[,3], color = clusters, pch = 16,
                    main = paste("Iteration:", iter), xlab = "X", ylab = "Y", zlab = "Z", angle = 55)
    }
    iteration_counter <- iteration_counter + 1
  }
  
  result <- data.frame(data, cluster_id = clusters)
  return(result)
}

# EM testing
result_em <- em_algorithm(easy_data, k = 3)
plot_clusters_3d_scatterplot3d(easy_data, result_em$cluster_id, "EM Algorithm Clustering")
