library(ggplot2)
library(dplyr)
library(arules)
library(igraph)
library(arulesViz)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data_path <- "Super_Store_data.csv"

data_preprocessing <- function(data_path) {
  dataframe <- read.csv(data_path)
  dataframe <- dataframe[, !(colnames(dataframe) %in% c("Postal.Code", "Region", "Ship.Date", "Customer.Name", "State", "Profit", "Discount", "Segment"))]
  dataframe$Order.Date <- as.Date(dataframe$Order.Date, format = "%m/%d/%Y")
  dataframe <- dataframe[order(dataframe$Order.Date), ]
  dataframe$Month_Year <- format(dataframe$Order.Date, "%m/%Y")
  dataframe$Product_ID_Numeric <- as.integer(factor(dataframe$Product.ID, levels = unique(dataframe$Product.ID)))
  return(dataframe)
}


dataframe <- data_preprocessing(data_path)
head(dataframe, 5)

dataframe <- dataframe %>%
  arrange(Sales) %>%
  slice(-(1:25)) %>% 
  arrange(desc(Sales)) %>%
  slice(-(1:25))

first_date <- min(dataframe$Order.Date, na.rm = TRUE)
last_date <- max(dataframe$Order.Date, na.rm = TRUE)
cat("First date:", format(first_date, "%m/%d/%Y"), "Last date:", format(last_date, "%m/%d/%Y"))


dataframe
aggregated_data <- aggregate(Sales ~ Order.Date, data = dataframe, sum)
head(aggregated_data)

ggplot(aggregated_data, aes(x = Order.Date, y = Sales)) +
  geom_line(color = "blue") +               
  labs(title = "Sales by day", 
       x = "Date", 
       y = "Sum of sales") +
  theme_minimal()       


aggregated_data <- dataframe %>%
  group_by(Month_Year) %>%
  summarise(Sales = sum(Sales)) %>%
  arrange(Month_Year)  

aggregated_data <- aggregated_data %>%
  mutate(Date = as.Date(paste("01", Month_Year, sep = "/"), format = "%d/%m/%Y")) %>%
  arrange(Date)  
head(aggregated_data)

Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "en_US.UTF-8")
ggplot(aggregated_data, aes(x = Date, y = Sales, group = 1)) +
  geom_line(color = "blue") +           
  geom_point(color = "red") +
  labs(title = "Sales Over Time", 
       x = "Month/Year", 
       y = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "3 months")

aggregated_data

model <- lm(Sales ~ Date, data = aggregated_data)

aggregated_data$predicted_sales <- predict(model, newdata = aggregated_data)

MAE <- mean(abs(aggregated_data$Sales - aggregated_data$predicted_sales))

Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", "en_US.UTF-8")
ggplot(aggregated_data, aes(x = Date, y = Sales)) +
  geom_line(color = "blue") +           
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "green", linetype = "dashed") + 
  labs(title = paste("Sales Over Time with Linear Trend - MAE:", round(MAE, 2)), 
       x = "Month/Year", 
       y = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")

# Assocation Pattern Mining
dataframe <- data_preprocessing(data_path)
head(dataframe, 3)

dataframe$Product.Name <- iconv(dataframe$Product.Name, from = "latin1", to = "UTF-8")
dataframe$Product.Name <- gsub("[^[:alnum:][:space:]]", "", dataframe$Product.Name)
dataframe$Product.Name <- gsub("\\s+", " ", dataframe$Product.Name)
dataframe$Product.Name <- tolower(dataframe$Product.Name)

transactions_list <- dataframe %>%
  dplyr::group_by(Order.ID) %>%
  dplyr::summarise(Products = list(unique(Product_ID_Numeric))) %>%
  dplyr::pull(Products)

transactions <- as(transactions_list, "transactions")

head(transactions_list, 10)
head(transactions_list)
transactions_list <- lapply(transactions_list, unique)
transactions <- as(transactions_list, "transactions")
transactions


summary(transactions)

rules <- apriori(transactions, 
                 parameter = list(supp = 0.0001, conf = 0.7, minlen = 2))

summary(rules)

inspect(head(rules, 10))

rules_df <- as(rules, "data.frame")
rules_sorted <- rules_df[order(rules_df$lift, decreasing = TRUE), ]
head(rules_sorted, 10)

plot(rules, method = "graph", control = list(type = "items"))


rules_df <- as(rules, "data.frame")
rules_sorted <- rules_df[order(rules_df$lift, decreasing = TRUE), ]
top_rules <- head(rules_sorted, 10)

ggplot(top_rules, aes(x = reorder(rules, lift), y = lift)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Association Rules",
       x = "Rule",
       y = "Strength of Association")

rules_df

# Graph presentation
rules_df$lhs <- gsub("[{}]", "", sapply(strsplit(as.character(rules_df$rules), "=>"), `[`, 1))
rules_df$rhs <- gsub("[{}]", "", sapply(strsplit(as.character(rules_df$rules), "=>"), `[`, 2))


rules_df <- head(rules_df, 20)


# Graph visualization
edges <- data.frame(from = rules_df$lhs, to = rules_df$rhs)
g <- graph_from_data_frame(edges, directed = TRUE)


vertex_colors <- brewer.pal(9, "Set3")
vertex_colors <- "lightblue"
edge_colors <- "gray50"
vertex_sizes <- 15 + degree(g) * 2  
edge_widths <- 1.5  

plot(g, 
     vertex.size = vertex_sizes,
     vertex.label.cex = 1.2,
     vertex.color = vertex_colors,
     vertex.frame.color = "white",  
     vertex.label.color = "black",
     edge.arrow.size = 0.9,
     edge.width = edge_widths,
     edge.color = edge_colors,
     main = "Enhanced Visualization of Association Rules",
     layout = layout_with_fr(g))
