# Exercise 2.Feature selection.
library(ggplot2)
set.seed(123) 

# high entropy df
high_entropy_df = data.frame(
  X1 = sample(5:8, 50, replace = TRUE),
  X2 = sample(2:10, 50, replace = TRUE),
  Y = sample(0:1, 100, replace = TRUE)
)

low_entropy_df = data.frame(
  X1 = sample(5:8, 50, replace = TRUE),
  X2 = sample(2:10, 50, replace = TRUE),
  Y = rep(0, 50)
)
# Adding one row with new Y value to present low Entropy
new_row = c(X1 = 5, X2 = 5, Y = 1)
low_entropy_df <- rbind(low_entropy_df, new_row)

# Entropy function
calculate_entropy = function(df_column){
  uniq_val = unique(df_column)
  length = length(uniq_val)
  rows = length(df_column)
  entropy = 0
  
  for (val in uniq_val) {
    prob = sum(df_column == val) / rows
    entropy = entropy - prob * log2(prob)
  }
  
  return(entropy)
}


entropy_score1 = calculate_entropy(df_column = high_entropy_df$Y)
cat('Entropy score: ',entropy_score1)

entropy_score2 = calculate_entropy(df_column = low_entropy_df$Y)
cat('Entropy score: ',entropy_score2)


# Fisher score

calculate_fisher = function(X, y){
  n_features = ncol(X)
  classes = unique(y)
  classes_unique = length(classes)
  fisher_scores = numeric(n_features)
  
  for (i in 1:n_features) {
    feature = X[, i]
    
    mean_global = mean(feature)
    between_class_var = 0
    within_class_var = 0
    
    for (class in classes) {
      feature_class = feature[y == class]
      mean_class = mean(feature_class)
      n_class = length(feature_class)
      
      # variation beetween class
      between_class_var = between_class_var + n_class * (mean_class - mean_global)^2
      
      # inclass variation
      within_class_var = within_class_var + sum((feature_class - mean_class)^2)
    }
    
    fisher_scores[i] = between_class_var / within_class_var
  }
  
  return(fisher_scores)
}

X = matrix(rnorm(100 * 5), ncol = 5)
y = sample(c(1, 4), 100, replace = TRUE) 


f_scores = calculate_fisher(X, y)

print(f_scores)

high_entropy_df$Dataset <- "High Entropy"
low_entropy_df$Dataset <- "Low Entropy"


combined_df <- rbind(high_entropy_df, low_entropy_df)


ggplot(combined_df, aes(x = 1:nrow(combined_df), y = Y, color = Dataset)) +
  geom_point() +
  labs(title = "Column Values for High and Low Entropy DataFrames",
       x = "Index",
       y = "Y Values") +
  theme_minimal()


  
  
  