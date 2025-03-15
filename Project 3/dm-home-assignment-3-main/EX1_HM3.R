library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(cluster)
library(e1071)
library(caret)
library(e1071)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

words_to_delete = c(
  "take", "put", "do", "come", "see", "want", "give", "talk",
  "become", "try", "ask", "call", "seem", "leave", "keep", "let", "run", "work", "play",
  "create", "turn", "hold", "allow", "hear", "tell", "show", "add", "believe",
  "did", "does", "should", "would", "might", "must", "could", "yes", "good", "just", "well", "also", "one",
  "much",
  # Added manually after analysis
  "like", "yes", "good", "just", "well", "also", "one","much"
)

train_dataframe <- read.csv("./EX1_Text_source/fake_news/combined_data.csv",
                            header = FALSE, as.is = TRUE, sep = ',',
                            nrows = 10000,
                            col.names = c("label", "text"))

head(train_dataframe, 10)

category_dict <- list(
  "0" = "Normal",
  "1" = "Spam"
)

# Text preprocessing
corpus = VCorpus(VectorSource(train_dataframe$text))
corpus.cleared = tm_map(corpus, content_transformer(tolower))
corpus.cleared = tm_map(corpus.cleared, removeWords, stopwords("english"))
corpus.cleared = tm_map(corpus.cleared, removeNumbers)
corpus.cleared = tm_map(corpus.cleared, removePunctuation)
corpus.cleared = tm_map(corpus.cleared, removeWords, words_to_delete)
corpus.cleared = tm_map(corpus.cleared, stemDocument)
corpus.cleared = tm_map(corpus.cleared, content_transformer(function(x) gsub("\\b\\w{1,2}\\b", "", x)))
corpus.cleared = tm_map(corpus.cleared, stripWhitespace)


#Calculating average length of vector
dtm = DocumentTermMatrix(corpus.cleared)
dtm.dense = removeSparseTerms(dtm, 0.9)
print(dim(dtm.dense))
dtm_matrix <- as.matrix(dtm.dense)
dtm_df <- as.data.frame(dtm_matrix)
term_matrix <- as.matrix(dtm.dense)
terms <- colnames(term_matrix)
word_lengths <- nchar(terms)
average_word_length <- mean(word_lengths)
cat("Average word length in documents: ", average_word_length, "\n")


#Analysis of text
dtm.dense = removeSparseTerms(dtm, 0.93)
dtm_matrix <- as.matrix(dtm.dense)
word_freqs <- sort(colSums(dtm_matrix), decreasing = TRUE)
top_words <- head(word_freqs, 20)
head(word_freqs, 20)

top_words_df <- data.frame(
  word = names(top_words),
  freq = top_words
)

ggplot(top_words_df, aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 20 most popular words",
       x = "Words",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# TF-IDF
subset_dtm <- dtm[1:300, ]
dtm_tfidf <- weightTfIdf(subset_dtm)
dtm_matrix_tfidf <- as.matrix(dtm_tfidf)
word_freqs_tfidf <- sort(colSums(dtm_matrix_tfidf), decreasing = TRUE)

top_words_tfidf <- head(word_freqs_tfidf, 20)
top_words_df_tfidf <- data.frame(
  word = names(top_words_tfidf),
  freq = top_words_tfidf
)

set.seed(123)
ggplot(top_words_df_tfidf, aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 20 most important words (TF-IDF)",
       x = "Words",
       y = "TF-IDF Weight") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Spliting data
set.seed(123)
train_index <- createDataPartition(train_dataframe$label, p = 0.8, list = FALSE)

train_data <- dtm_df[train_index + 1, ]
test_data <- dtm_df[-train_index - 1, ]
train_labels <- train_dataframe$label[train_index + 1]
test_labels <- train_dataframe$label[-train_index - 1]

train_labels

# Training
svm_model <- svm(train_data, as.factor(train_labels), kernel = "linear")

# Choosing best model
predictions <- predict(svm_model, test_data)

confusion_matrix <- table(Predicted = predictions, Actual = test_labels)

# Accuracy
accuracy <- sum(predictions == test_labels) / length(test_labels)
print(confusion_matrix)
print(paste("Accuracy:", accuracy))

# PCA
pca <- prcomp(train_data, center = TRUE, scale. = TRUE)
train_data_pca <- pca$x[, 1:2]  
test_data_pca <- predict(pca, newdata = test_data)[, 1:2]
svm_model_pca <- svm(train_data_pca, as.factor(train_labels), kernel = "linear", cost = 1, scale = FALSE)
predictions <- predict(svm_model_pca, test_data_pca)

predictions

confusion_matrix <- table(Predicted = predictions, Actual = test_labels)
accuracy <- sum(predictions == test_labels) / length(test_labels)
print(confusion_matrix)
print(paste("Accuracy:", accuracy))

x_min <- min(train_data_pca[, 1]) - 1
x_max <- max(train_data_pca[, 1]) + 1
y_min <- min(train_data_pca[, 2]) - 1
y_max <- max(train_data_pca[, 2]) + 1

grid <- expand.grid(
  Dim1 = seq(x_min, x_max, length.out = 200),
  Dim2 = seq(y_min, y_max, length.out = 200)
)

grid_predictions <- predict(svm_model_pca, grid)
grid$Category <- grid_predictions

# Boundary
g1 <- ggplot() +
  geom_tile(data = grid, aes(x = Dim1, y = Dim2, fill = Category), alpha = 0.3) +
  geom_point(data = data.frame(Dim1 = train_data_pca[, 1], Dim2 = train_data_pca[, 2], Category = train_labels),
             aes(x = Dim1, y = Dim2, color = Category), size = 2) +
  labs(title = "Decision Boundary", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal() +
  theme(legend.position = "right")
print(g1)

# Centroids
centroids <- aggregate(cbind(Dim1, Dim2) ~ Category,
                       data = data.frame(Dim1 = train_data_pca[, 1], Dim2 = train_data_pca[, 2], Category = train_labels),
                       FUN = mean)

g2 <- ggplot(data.frame(Dim1 = train_data_pca[, 1], Dim2 = train_data_pca[, 2], Category = train_labels),
             aes(x = Dim1, y = Dim2, color = Category)) +
  geom_point(alpha = 0) +
  geom_point(data = centroids, aes(x = Dim1, y = Dim2, color = Category), size = 4, shape = 4, stroke = 2) +
  labs(title = "Centroids", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal() +
  theme(legend.position = "right")
print(g2)