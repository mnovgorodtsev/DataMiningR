library(igraph)

# Implement your own functions in R to compute:
# 1 Local clustering coefficient.
# 2 Degree centrality.
# 3 Degree prestige.
# 4 Gregariousness of a node.
# 5 Closeness centrality and proximity prestige.
# 6 Betweenness Centrality
# 7 Common neighbor based measure.
# 8 Jaccard Measure

# Loading the csv files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dataset_edges = read.csv("./EX2_Datasets/Dataset1-Media-Example-EDGES.csv",
                         header = T, as.is = T, 
                         col.names = c("from", "to","weight","height"))
dataset_nodes = read.csv("./EX2_Datasets/Dataset1-Media-Example-NODES.csv",
                         header = T, as.is = T, 
                         col.names = c("id", "media","media.type","type.label",
                                       "audience.size"))
dataset_edges
dataset_nodes

g <- graph_from_data_frame(dataset_edges, vertices = dataset_nodes, directed = FALSE)
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
g

# Functions implementation
# 1 Local clustering coefficient
g <- graph_from_data_frame(dataset_edges, vertices = dataset_nodes, directed = FALSE)
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
g

local_clustering_coeff <- function(g) {
  lcc_values <- numeric(length(V(g)))  
  
  # for all media
  for (v in V(g)) {
    neighbors <- neighbors(g, v)  
    k <- length(neighbors)  
    
    # less than 2 no triangle
    if (k < 2) {
      lcc_values[v] <- 0  
    } else {
      edges_between_neighbors <- 0  
      for (i in 1:(k-1)) {  
        for (j in (i+1):k) {
          if (are.connected(g, neighbors[i], neighbors[j])) {  
            edges_between_neighbors <- edges_between_neighbors + 1  
          }
        }
      }
      # calculating local
      lcc_values[v] <- (2 * edges_between_neighbors) / (k * (k - 1)) 
    }
  }
  return(lcc_values)  
}

val = local_clustering_coeff(g)
V(g)$lcc <- val
result <- data.frame(
  vertex = V(g)$name,
  LCC = V(g)$lcc
)

# Using ready function to compare values
lcc_igraph <- transitivity(g, type = "local")
result$LCC_igraph <- lcc_igraph
print(result)

plot(g, vertex.size = 23, 
     vertex.label = V(g)$name, 
     vertex.color = heat.colors(100)[as.numeric(cut(V(g)$lcc, breaks = 100))])


# 2 Degree centrality
degree_centrality = function(g) {
  degree_values = degree(g, mode = "all")  
  n = vcount(g)
  degree_centralities = degree_values / (n - 1)  
  
  return(data.frame(
    node = V(g)$name,
    degree_centrality = degree_centralities
  ))
}

g <- graph_from_data_frame(dataset_edges, vertices = dataset_nodes, directed = FALSE)
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

degree_results <- degree_centrality(g)

igraph_degree_centrality <- degree(g, mode = "all") / (vcount(g) - 1)

degree_results$igraph_degree_centrality <- igraph_degree_centrality[degree_results$node]

print(degree_results)


# 3. Degree prestige.

g <- graph_from_data_frame(dataset_edges, vertices = dataset_nodes, directed = FALSE)
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

degree_prestige <- function(g) {
  adjacency_matrix <- as_adjacency_matrix(g, sparse = FALSE)
  
  in_degree <- colSums(adjacency_matrix)
  n <- vcount(graph)
  prestige <- in_degree / (n - 1)
  
  return(prestige)
}

prestige_values <- degree_prestige(g)
print(prestige_values)

# ready function
degree_values_igraph <- degree(g, mode = "all")  
prestige_values_igraph <- degree_values_igraph / (vcount(g) - 1)

print("Own implementation:")
print(prestige_values)
print("Igraph:")
print(prestige_values_igraph)

# 4 Gregariousness of a node.
gregariousness = function(g) {
  n <- vcount(g)
  adjacency_matrix <- as_adjacency_matrix(g, sparse = FALSE)
  out_degree <- rowSums(adjacency_matrix)
  gregariousness <- out_degree / (n - 1)
  
  return(gregariousness)
}

# plot graph
g <- graph_from_data_frame(dataset_edges, vertices = dataset_nodes, directed = FALSE)
class(g)
# simplify
g <- simplify(g, remove.loops = TRUE, remove.multiple = TRUE)
undirected = as_undirected(g)
gregariousness_values = gregariousness(g)
igraph_gregariousness = degree(g, mode="out", normalized = TRUE)
print(gregariousness_values)
print(igraph_gregariousness)




# 5 Closeness centrality and proximity prestige.

g <- graph_from_data_frame(dataset_edges, vertices = dataset_nodes, directed = FALSE)
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

closeness_centrality <- function(g) {
  dist_matrix <- distances(g)
  closeness <- apply(dist_matrix, 1, function(x) {1 / sum(x[x != Inf])})
  return(closeness)
}

proximity_prestige <- function(g) {
  n <- vcount(g)
  prestige_values <- numeric(n)
  
  for (k in 1:n) {
    influence_k <- neighbors(g, k, mode = "out")
    impact <- 0
    
    for (i in influence_k) {
      single_distance <- distances(g, i, k, mode = "out")
      
      if (is.finite(single_distance)) {
        num_neighbors <- length(neighbors(g, i, mode = "out"))
        impact <- impact + num_neighbors / single_distance
      }
    }
    prestige_values[k] <- impact / (n - 1)
  }
  
  return(prestige_values)
}

closeness_centrality_values <- closeness_centrality(g)
proximity_prestige_values <- proximity_prestige(g)

result_df <- data.frame(
  closeness_centrality = closeness_centrality_values,
  proximity_prestige = proximity_prestige_values
)

head(result_df)


# 6 Betweenness Centrality
g <- graph_from_data_frame(dataset_edges, vertices = dataset_nodes, directed = FALSE)
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

betweenness_centrality <- function(g) {
  n <- vcount(g)
  betweenness <- numeric(n)
  vertex_ids <- V(g)  
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (i != j) {  
        sp <- shortest_paths(g, from = vertex_ids[i], to = vertex_ids[j], output = "vpath")
        if (length(sp$vpath[[1]]) > 2) {  
          for (vertex in sp$vpath[[1]][-c(1, length(sp$vpath[[1]]))]) {
            betweenness[as.numeric(vertex)] <- betweenness[as.numeric(vertex)] + 1
          }
        }
      }
    }
  }
  return(betweenness)
}


betweenness_values <- betweenness_centrality(g)

result_df = data.frame(betweenness_centrality = betweenness_values)
result_df$betweenness_centrality <- betweenness_values

betweenness_igraph <- betweenness(g, directed = FALSE, weights = E(g)$weight)
result_df$betweenness_igraph <- betweenness_igraph

head(result_df)


# 7 Common neighbor based measure.
common_neighbor_measure <- function(edges, i, j) {
  i_neigh <- with(edges, unique(c(to[from == i], from[to == i])))
  j_neigh <- with(edges, unique(c(to[from == j], from[to == j])))
  common_neighbors <- intersect(i_neigh, j_neigh)
  return(length(common_neighbors))
}

common_neighbor_measure(dataset_edges, 's05', 's06')


# 8 Jaccard measure
jaccard_measure <- function(edges, i, j) {
  i_neigh <- unique(c(edges$to[edges$from == i], edges$from[edges$to == i]))
  j_neigh <- unique(c(edges$to[edges$from == j], edges$from[edges$to == j]))
  
  common_neighbors <- length(intersect(i_neigh, j_neigh))
  total_neighbors  <- length(unique(c(i_neigh, j_neigh)))
  
  return(common_neighbors / total_neighbors )
}


results <- data.frame(from = character(), to = character(),
                      common_neighbors = numeric(),
                      jaccard = numeric(),
                      stringsAsFactors = FALSE)

for (i in 1:(nrow(dataset_nodes)-1)) {
  for (j in (i+1):nrow(dataset_nodes)) {
    node_i <- dataset_nodes$id[i]
    node_j <- dataset_nodes$id[j]
    
    common_neighbors <- common_neighbor_measure(dataset_edges, node_i, node_j)
    jaccard <- jaccard_measure(dataset_edges, node_i, node_j)
    
    results <- rbind(results, data.frame(from = node_i, to = node_j,
                                         common_neighbors = common_neighbors,
                                         jaccard = jaccard))
  }
}

# Ready implementation from igraph
graph <- graph_from_data_frame(dataset_edges, directed = FALSE)

igraph_jaccard <- similarity(graph, method = "jaccard")
results$igraph_jaccard <- NA 

for (row in 1:nrow(results)) {
  node_i <- results$from[row]
  node_j <- results$to[row]
  
  index_i <- which(V(graph)$name == node_i)
  index_j <- which(V(graph)$name == node_j)
  results$igraph_jaccard[row] <- igraph_jaccard[index_i, index_j]
}
head(results)

