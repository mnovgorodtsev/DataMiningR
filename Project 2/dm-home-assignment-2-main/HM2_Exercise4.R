library(caret)
library(ggplot2)
library(rgl)

split_data <- function(data, train_size = 0.8) {
  n <- nrow(data)
  train_index <- sample(1:n, size = round(train_size * n))
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  return(list(train_data = train_data, test_data = test_data))
}

transformation_2d_to_3d <- function(data) {
  data <- transform(data, z = ifelse(class == 1, 1, -1))  # Class 1 gets z=1, Class 2 gets z=-1
  return(data)
}

log_regression_2d <- function(data_2d) {
  split <- split_data(data_2d, train_size = 0.8)
  train_data <- split$train_data
  test_data <- split$test_data
  
  model <- glm(class ~ x + y, data = train_data, family = "binomial")
  
  predicted_probs <- predict(model, newdata = test_data, type = "response")
  predicted_classes <- ifelse(predicted_probs > 0.5, 2, 1)
  predicted_classes <- as.factor(predicted_classes)
  
  correct_predictions <- predicted_classes == test_data$class
  accuracy_value <- mean(correct_predictions)
  
  conf_matrix <- table(Predicted = predicted_classes, Actual = test_data$class)
  
  precision <- sum(diag(conf_matrix)) / sum(conf_matrix)
  recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  print(paste("2D accuracy:", round(accuracy_value, 2)))
  print("Confusion matrix:")
  print(conf_matrix)
  
  ggplot(data_2d, aes(x = x, y = y, color = class)) +
    geom_point(alpha = 0.6) +
    stat_function(
      fun = function(x) {
        coef <- coef(model)
        (-coef[1] - coef[2] * x) / coef[3]
      },
      color = "black",
      size = 1
    ) +
    scale_color_manual(values = c("blue", "red"), name = "CLASS") +
    theme_minimal()
}

log_regression_3d <- function(data) {
  train_index <- createDataPartition(data$class, p = 0.85, list = FALSE)
  train_data_3d <- data[train_index, ]
  test_data_3d <- data[-train_index, ]
  
  model_3d <- train(class ~ x + y + z, data = train_data_3d, method = "glm", family = "binomial")
  
  predictions_3d <- predict(model_3d, newdata = test_data_3d)
  accuracy_3d <- mean(predictions_3d == test_data_3d$class)
  conf_matrix_3d <- confusionMatrix(predictions_3d, test_data_3d$class)
  
  print(paste("3D accuracy:", round(accuracy_3d, 2)))
  print(conf_matrix_3d)
  
  coef_3d <- coef(model_3d$finalModel)
  
  x_seq <- seq(min(data$x), max(data$x), length.out = 50)
  y_seq <- seq(min(data$y), max(data$y), length.out = 50)
  grid <- expand.grid(x = x_seq, y = y_seq)
  grid$z <- (-coef_3d[1] - coef_3d[2] * grid$x - coef_3d[3] * grid$y) / coef_3d[4]
  
  colors <- c("blue", "red")
  col_values <- colors[data$class]
  plot3d(data$x, data$y, data$z, col = col_values, 
         type = "s", size = 0.75, xlab = "X", ylab = "Y", zlab = "Z")
  surface3d(unique(grid$x), unique(grid$y), matrix(grid$z, nrow = length(x_seq), 
                                                   ncol = length(y_seq)), 
            color = "black", alpha = 0.5)
}

set.seed(122)

# generating data
n_points = 600
n_class <- n_points / 2

# halfmoons
t1 <- seq(0, pi, length.out = n_class)  
x1 <- 1.5 * cos(t1) + rnorm(n_class, sd = 0.15) 
y1 <- 1.5 * sin(t1) + rnorm(n_class, sd = 0.12)  

t2 <- seq(pi, 2 * pi, length.out = n_class)  
x2 <- 1.5 * cos(t2) + rnorm(n_class, mean = 1.7, sd = 0.17)  
y2 <- 1.5 * sin(t2) + rnorm(n_class, mean = 1, sd = 0.14)  

X <- rbind(cbind(x1, y1), cbind(x2, y2))
labels <- c(rep(1, n_class), rep(2, n_class))  

# convert to dataframe
data_2d = data.frame(x = X[, 1], y = X[, 2], class = as.factor(labels))

# regressions
log_regression_2d(data_2d)
data_3d <- transformation_2d_to_3d(data_2d)
log_regression_3d(data_3d)
