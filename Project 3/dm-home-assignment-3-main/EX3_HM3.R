set.seed(123)

X <- 1:3000
Y <- rep(NA, length(X))

Y[X >= 1 & X <= 700] <- rnorm(sum(X >= 1 & X <= 700), mean = 20, sd = 0.63)
Y[X >= 701 & X <= 1400] <- rnorm(sum(X >= 701 & X <= 1400), mean = 19, sd = 0.5)
Y[X >= 1401 & X <= 1550] <- rnorm(sum(X >= 1401 & X <= 1550), mean = 18, sd = 0.35)
Y[X >= 1551 & X <= 1650] <- rnorm(sum(X >= 1551 & X <= 1650), mean = 20, sd = 0.45)
Y[X >= 1651 & X <= 1700] <- rnorm(sum(X >= 1651 & X <= 1700), mean = 19, sd = 0.5)
Y[X >= 1700 & X <= 1800] <- rnorm(sum(X >= 1700 & X <= 1800), mean = 21, sd = 0.75)
Y[X >= 1800 & X <= 2000] <- rnorm(sum(X >= 1800 & X <= 2000), mean = 22.5, sd = 1)
Y[X >= 2000 & X <= 2100] <- rnorm(sum(X >= 2000 & X <= 2100), mean = 23.5, sd = 0.5)
Y[X >= 2100 & X <= 2500] <- rnorm(sum(X >= 2100 & X <= 2500), mean = 24.5, sd = 0.7)
Y[X >= 2500 & X <= 2900] <- rnorm(sum(X >= 2500 & X <= 2900), mean = 25, sd = 0.55)
Y[X >= 2900 & X <= 3000] <- rnorm(sum(X >= 2900 & X <= 3000), mean = 26.5, sd = 1)

data <- data.frame(X = X, Y = Y)
colors <- colorRampPalette(c("orange","red", "blue", "green", "purple","black"))(10)
data$color <- NA
data$color[X >= 1 & X <= 700] <- colors[1]
data$color[X >= 701 & X <= 1400] <- colors[2]
data$color[X >= 1401 & X <= 1550] <- colors[3]
data$color[X >= 1551 & X <= 1650] <- colors[4]
data$color[X >= 1651 & X <= 1700] <- colors[5]
data$color[X >= 1701 & X <= 1800] <- colors[6]
data$color[X >= 1801 & X <= 2000] <- colors[7]
data$color[X >= 2001 & X <= 2100] <- colors[8]
data$color[X >= 2101 & X <= 2500] <- colors[9]
data$color[X >= 2501 & X <= 3000] <- colors[10]

plot(data$X, data$Y, type = "p", col = data$color, xlab = "X", ylab = "Y", 
     main = "Data visualization with colors", pch = 16, cex = 0.7)
grid()

data <- data.frame(X = X, Y = Y, color = data$color)
data


res_sampling_no_drift <- function(stream_dataset, size = 300) {
  count <- 1
  output_list <- vector("list", size)
  
  for (i in 1:length(stream_dataset)) {
    probability <- size / i
    if (count <= size) {
      output_list[count] <- stream_dataset[i]
    } else {
      j <- sample(1:size, 1)
      if (runif(1) <= probability) {
        replaced <- output_list[[j]]
        output_list[[j]] <- stream_dataset[i]
      }
    }
    count <- count + 1
  }
  
  return(unlist(output_list))
}

res_sampling_with_drift <- function(stream_dataset, size = 300) {
  count <- 1
  output_list <- vector("list", size)
  
  for (i in 1:length(stream_dataset)) {
    probability <- min(1, log(i) / log(i + 1)) 
    
    if (count <= size) {
      output_list[count] <- stream_dataset[i]
    } 
    else {
      j <- sample(1:size, 1)
      if (runif(1) <= probability) {
        replaced <- output_list[[j]]
        output_list[[j]] <- stream_dataset[i]
      }
    }
    
    count <- count + 1
  }
  
  return(unlist(output_list))
}

add_one_point <- function(stream_dataset, size = 300) {
  for (i in 1:length(stream_dataset)) {
    
  }
}


stream_dataset <- 1:nrow(data)

sampled_indices_all <- res_sampling_no_drift(stream_dataset, size = 3000)
sampled_data_all <- data[sampled_indices_all, ]
plot(sampled_data_all$X, sampled_data_all$Y, col = sampled_data_all$color, pch = 16, cex = 0.7,
     xlab = "X", ylab = "Y")
grid()

sampled_indices_small_no_drift <- res_sampling_no_drift(stream_dataset, size = 300)
sampled_data_small_no_drift <- data[sampled_indices_small_no_drift, ]
plot(sampled_data_small_no_drift$X, sampled_data_small_no_drift$Y, col = sampled_data_small_no_drift$color, pch = 16, cex = 0.7,
     xlab = "X", ylab = "Y")
grid()

sampled_indices_drift <- res_sampling_with_drift(stream_dataset, size = 300)
sampled_data_drift <- data[sampled_indices_drift, ]
plot(sampled_data_drift$X, sampled_data_drift$Y, col = sampled_data_drift$color, pch = 16, cex = 0.7,
     xlab = "X", ylab = "Y")
grid()


sampled_data_all

plot_distribution <- function(reservoir) {
  reservoir$color <- factor(reservoir$color, levels = unique(reservoir$color))
  
  colors <- colorRampPalette(c("orange", "red", "blue", "green", "purple", "black"))(10)
  
  p2 <- ggplot(reservoir, aes(x = x, fill = as.factor(color))) +
    geom_histogram(bins = 30, alpha = 0.9, position = "identity") + 
    scale_fill_manual(values = unique(reservoir$color)) + 
    labs(title = " ", fill = "Time Period") +
    theme_minimal()
  
  return(p2)
}



# distribution
ggplot(data = sampled_data_all, aes(x = X)) +
  geom_histogram(fill = "navy", color = "white", bins = 10) +
  xlab("X") +
  ylab("Frequency") +
  theme_minimal()

ggplot(data = sampled_data_small_no_drift, aes(x = X)) +
  geom_histogram(fill = "navy", color = "white", bins = 10) +
  xlab("X") +
  ylab("Frequency") +
  theme_minimal()

ggplot(data = sampled_data_drift, aes(x = X)) +
  geom_histogram(fill = "navy", color = "white", bins = 10) +
  xlab("X") +
  ylab("Frequency") +
  theme_minimal()
