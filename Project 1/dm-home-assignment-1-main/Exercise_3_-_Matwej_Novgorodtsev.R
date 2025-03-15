set.seed(123)

# Data generation (visualization of this is in assignment)
X1 = sample(10:20, 50, replace = TRUE)
Y1 = sample(5:15, 50, replace = TRUE)
Z1 = sample(3:12, 50, replace = TRUE)
CLASS1 = rep(1, 50)

X2 = sample(15:25, 50, replace = TRUE)
Y2 = sample(10:20, 50, replace = TRUE)
Z2 = sample(7:15, 50, replace = TRUE)
CLASS2 = rep(2, 50)

X3 = sample(12:22, 50, replace = TRUE)
Y3 = sample(8:18, 50, replace = TRUE)
Z3 = sample(5:13, 50, replace = TRUE)
CLASS3 = rep(3, 50)

df1 = data.frame(X = X1, Y = Y1, Z = Z1, class = CLASS1)
df2 = data.frame(X = X2, Y = Y2, Z = Z2, class = CLASS2)
df3 = data.frame(X = X3, Y = Y3, Z = Z3, class = CLASS3)

merged_df = rbind(df1, df2, df3)
train_df = merged_df

# entropy function from exercise 1
calculate_entropy = function(data) {
  target = data[[ncol(data)]]
  uniq_val = unique(target)
  entropy = 0
  
  for (val in uniq_val) {
    prob = sum(target == val) / length(target)
    entropy = entropy - prob * log2(prob)
  }
  
  return(entropy)
}

# Data splitting function
split_data = function(df, column_name, threshold) {
  left_split = df[df[[column_name]] <= threshold, ]
  right_split = df[df[[column_name]] > threshold, ]
  split_list = list(left = left_split, right = right_split)
  return(split_list)
}

# information gain function
calculate_information_gain = function(data, left_split, right_split) {
  total_entropy = calculate_entropy(data)
  left_entropy_value = calculate_entropy(left_split)
  right_entropy_value = calculate_entropy(right_split)
  
  total_size = nrow(data)
  left_proportion = nrow(left_split) / total_size
  right_proportion = nrow(right_split) / total_size
  
  information_gain = total_entropy - (left_proportion * left_entropy_value + 
                                        right_proportion * right_entropy_value)
  return(information_gain)
}

# finding best split
find_best_split = function(data) {
  best_gain = -99999
  optimal_column = NULL
  optimal_split_value = NULL
  optimal_splits = NULL
  
  feature_names = colnames(data)[-ncol(data)]
  
  evaluate_splits = function(feature, split_value) {
    split_results = split_data(data, feature, split_value)
    gain = calculate_information_gain(data, split_results$left, split_results$right)
    return(gain)
  }
  
  for (feature in feature_names) {
    unique_values = unique(data[[feature]])
    
    for (value in unique_values) {
      current_gain = evaluate_splits(feature, value)
      
      if (current_gain > best_gain) {
        best_gain = current_gain
        optimal_column = feature
        optimal_split_value = value
        optimal_splits = split_data(data, feature, value)
      }
    }
  }
  
  return(list(
    best_column = optimal_column,
    best_split_value = optimal_split_value,
    splits = optimal_splits
  ))
}

check_final_conditions = function(data, target, depth, max_depth) {
  conditions = c(
    depth >= max_depth,               
    length(unique(target)) == 1       
  )
  
  return(any(conditions))
}

return_most_common_class = function(target) {
  target_counts = table(target)
  
  most_frequent_class = as.numeric(names(target_counts)[which.max(target_counts)])
  
  prediction_list = list(prediction = most_frequent_class)
  
  return(prediction_list)
}

build_tree = function(data, depth = 0, max_depth = 3) {
  # last column values are our y
  target = data[[ncol(data)]]
  
  final_condition = check_final_conditions(data, target, depth, max_depth)
  
  if (final_condition) {
    prediction_list = return_most_common_class(target)
    return(prediction_list)
  }
  
  best_split = find_best_split(data)
  
  left_branch = build_tree(best_split$splits$left, depth + 1, max_depth)
  right_branch = build_tree(best_split$splits$right, depth + 1, max_depth)
  
  return(list(feature = best_split$best_column, best_split_value = best_split$best_split_value, left = 
                left_branch, right = right_branch))
}

# single value prediction
predict_tree = function(tree, instance) {
  while (is.null(tree$prediction)) {
    if (instance[[tree$feature]] <= tree$best_split_value) {
      tree = tree$left
    } else {
      tree = tree$right
    }
  }
  return(tree$prediction)
}

# whole dataset prediction
predict_tree_batch = function(tree, data) {
  predictions = vector("numeric", nrow(data))
  
  for (i in seq_len(nrow(data))) {
    predictions[i] = predict_tree(tree, data[i, ])
  }
  
  return(predictions)
}

# metrics calculation
calculate_metrics = function(conf_matrix) {
  accuracy = sum(diag(conf_matrix)) / sum(conf_matrix)
  metrics = data.frame(Class = 1:nrow(conf_matrix), Precision = NA, Recall = NA, F1 = NA)
  
  for (class in 1:nrow(conf_matrix)) {
    TP = conf_matrix[class, class]  
    FP = sum(conf_matrix[, class]) - TP  
    FN = sum(conf_matrix[class, ]) - TP  
    
    precision = ifelse((TP + FP) == 0, 0, TP / (TP + FP))
    recall = ifelse((TP + FN) == 0, 0, TP / (TP + FN))
    f1_score = ifelse((precision + recall) == 0, 0, 2 * (precision * recall) / (precision + recall))
    
    metrics[metrics$Class == class, ] <- c(class, precision, recall, f1_score)
  }
  
  return(list(accuracy = accuracy, metrics = metrics))
}

cross_validation = function(data, k_folds = 5, max_depth = 3) {
  shuffled_data = data[sample(nrow(data)), ]
  fold_size = floor(nrow(shuffled_data) / k_folds)
  
  accuracy_list = c()
  metrics_list = list()
  
  for (i in 1:k_folds) {
    test_indices = ((i - 1) * fold_size + 1):(i * fold_size)
    test_data = shuffled_data[test_indices, ]
    train_data = shuffled_data[-test_indices, ]
    
    tree = build_tree(train_data, max_depth = max_depth)
    predictions = predict_tree_batch(tree, test_data)
    
    conf_matrix = table(Predicted = predictions, Actual = test_data$class)
    
    metrics_results = calculate_metrics(conf_matrix)
    accuracy_list = c(accuracy_list, metrics_results$accuracy)
    metrics_list[[i]] = metrics_results$metrics
  }
  
  mean_accuracy = mean(accuracy_list)
  mean_metrics = Reduce(function(x, y) {x + y}, metrics_list) / k_folds
  
  return(list(mean_accuracy = mean_accuracy, mean_metrics = mean_metrics))
}

cv_results = cross_validation(train_df, k_folds = 5, max_depth = 3)

print(paste("Mean Accuracy across folds:", round(cv_results$mean_accuracy, 3)))
print(cv_results$mean_metrics)
