library(plotly)

fun_hopkins_statistics = function(dataset, m = 30){
  coords_data = dataset
  
  coords_data <- scale(coords_data)
  
  real_points <- coords_data[sample(1:nrow(coords_data), m), ]
  
  min_vals <- apply(coords_data, 2, min)
  max_vals <- apply(coords_data, 2, max)
  
  random_points <- matrix(runif(m * ncol(coords_data), min = min_vals, max = max_vals), ncol = ncol(coords_data))
  
  w_distances <- apply(real_points, 1, function(point) {
    min(sqrt(rowSums((coords_data - point)^2)))
  })
  
  u_distances <- apply(random_points, 1, function(point) {
    min(sqrt(rowSums((coords_data - point)^2)))
  })
  
  H <- sum(u_distances) / (sum(u_distances) + sum(w_distances))
  
  return(H)
}

set.seed(123)

cluster1 <- cbind(rnorm(150, mean = 20, sd = 0.5), 
                   rnorm(150, mean = 30, sd = 0.5), 
                   rnorm(150, mean = 30, sd = 0.5))
cluster2 <- cbind(rnorm(150, mean = 20, sd = 0.5), 
                   rnorm(150, mean = 10, sd = 0.5), 
                  rnorm(150, mean = 30, sd = 0.5))


data_separable <- data.frame(rbind(cluster1, cluster2))
colnames(data_separable) <- c("x", "y", "z")

plot_ly(data_separable, x = ~x, y = ~y, z = ~z, color = I('lightgreen'),
        type = 'scatter3d', mode = 'markers',
        marker = list(size = 4, 
                      color = 'lightgreen', 
                      line = list(color = 'black', width = 1))) %>%
  layout(title = 'Data 3D Presentation')

hopkins_value <- fun_hopkins_statistics(data_separable, m = 100)

cat("Hopkins Statistic:", hopkins_value, "\n")


calculate_hopkins_for_2_features <- function(data, feature1, feature2, m = 100) {
  pair_data <- data[, c(feature1, feature2), drop = FALSE]  
  
  hopkins_value <- fun_hopkins_statistics(pair_data, m = m)
  return(hopkins_value)
}

features <- colnames(data_separable)
pairs <- combn(features, 2, simplify = FALSE)

hopkins_results <- list()

for (pair in pairs) {
  hopkins_results[[paste(pair, collapse = "-")]] <- calculate_hopkins_for_2_features(data_separable, pair[1], pair[2])
}

for (pair in names(hopkins_results)) {
  cat("Hopkins Statistic for", pair, ":", hopkins_results[[pair]], "\n")
}


plot_feature_pairs <- function(data) {

  label <- c(rep(1, nrow(data) / 2), rep(2, nrow(data) / 2))
  data$label <- as.factor(label)

  combos <- list(c("x", "y"), c("x", "z"), c("y", "z"))

  for (combo in combos) {

    p <- ggplot(data, aes_string(x = combo[1], y = combo[2], color = "label")) +
      geom_point(alpha = 0.6) +
      labs(
        x = combo[1],
        y = combo[2],
        title = paste("Scatter plot of", combo[1], "and", combo[2])
      ) +
      scale_color_manual(values = c("red", "blue"), name = "Cluster") +
      theme_minimal() +
      theme(legend.title = element_blank(),
            panel.grid.major = element_line(color = "grey", size = 0.5),
            panel.grid.minor = element_line(color = "grey", size = 0.25))

    print(p)
  }
}
plot_feature_pairs(data_separable)


calculate_hopkins_for_feature_pairs <- function(data) {
  combos <- list(c("x", "y"), c("x", "z"), c("y", "z"))

  hopkins_results <- list()

  for (combo in combos) {
    data_subset <- data[, combo]

    hopkins_value <- fun_hopkins_statistics(data_subset)

    hopkins_results[[paste(combo[1], combo[2], sep = "_")]] <- hopkins_value
  }

  return(hopkins_results)
}

hopkins_results <- calculate_hopkins_for_feature_pairs(data_separable)
print(hopkins_results)

