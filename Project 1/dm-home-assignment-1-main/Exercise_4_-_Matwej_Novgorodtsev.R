set.seed(123)
points = 500
b = 1.5

x1 = sample(10:20, points, replace = TRUE)
x2 = sample(5:15, points, replace = TRUE)
x3 = sample(12:22, points, replace = TRUE)
x4 = sample(3:12, points, replace = TRUE)
x5 = sample(0:10, points, replace = TRUE)
x6 = sample(1:15, points, replace = TRUE)

x = data.frame(x1, x2, x3, x4, x5, x6)

y = 0.01 * x$x1 + 0.43 * x$x2 + 0.6 * x$x3 + 0.002 * 
  x$x4 + 0 * x$x5 + 0.12 * x$x6 + b + rnorm(points, sd = 0.1)

data = data.frame(x, y)

  
forward_elimination = function(X, y, significance_level = 0.05) {
  
  X_columns = colnames(X) 
  output_features = c()  
  
  while (length(X_columns) > 0) {
    best_feature = 0
    best_p_value = 1e10 
    
    for (feature in X_columns) {
      model = lm(y ~ ., data = data.frame(y, X[, c(output_features, feature), drop = FALSE]))
      
      # calculating p value for current columns
      p_value = summary(model)$coefficients[feature, 4] 
      
      if (p_value < best_p_value) {
        best_p_value = p_value
        best_feature = feature
      }
    }
    
    if (best_p_value < significance_level) {
      output_features = c(output_features, best_feature)
      X_columns = X_columns[X_columns != best_feature]
    } else {
      break
    }
  }
  
  return(output_features)
}

  
  X = data[, -ncol(data)]
  y = data$y             
  
  
  best_features = forward_elimination(X, y)
  cat("Chosen features:", best_features)

  
  final_model = lm(y ~ ., data = data[, c("y", best_features)])
  
  # printing the summary of the final model
  summary(final_model)