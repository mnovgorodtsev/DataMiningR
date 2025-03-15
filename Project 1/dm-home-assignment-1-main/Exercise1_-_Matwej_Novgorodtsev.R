# Exercise 1. Distance functions.

# Euclidean distance
dis_euclidean = function(x, y) {
  x1 = x[1]
  x2 = x[2]
  y1 = y[1]
  y2 = y[2]
  
  distance = sqrt((x1 - y1)^2 + (x2 - y2)^2)
  
  return(distance)
}

x = c(-2, 5)
y = c(6, 7)

distance1  = dis_euclidean(x,y)
distance1

# Manhattan distance
dis_manhattan = function(x, y){
  x1 = x[1]
  x2 = x[2]
  y1 = y[1]
  y2 = y[2]
  
  distance = (abs(x1-y1) + abs(x2-y2))
  
  return(distance)
}

x = c(-6, -8)
y = c(12, 11)

distance2 = dis_manhattan(x, y)
distance2

# Chebyshev distance
# biggest distance on single axis
dis_chebyshev = function(x, y){
  x1 = x[1]
  x2 = x[2]
  y1 = y[1]
  y2 = y[2]
  
  distance = max(c(abs(x1-y1),abs(x2-y2)))
  return(distance)
}

x = c(1,7)
y = c(13,14)

distance3 = dis_chebyshev(x, y)
distance3

# Minkovsky distance
dis_minkovsky = function(x, y, p){
  # when:
  # p = 1 it is Manhattan distance
  # p = 2 it is Euclidean distance
  # p > 2 it is Minkovsky distance
  vec_lenght = length(x)
  distance = 0
  for (i in 1:vec_lenght) {
    distance = distance + ((x[i] - y[i])^p)
  }
  distance = distance^(1/p)
  return(distance)
}

x = c(1,7,10)
y = c(-2,2,4)
p = 3
distance4 = dis_minkovsky(x, y, p)
distance4