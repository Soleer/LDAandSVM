library(ggplot2)

#testdatensatz Gauß 2D
set.seed(0)
test <-
  data.frame(
    x = rnorm(100, 2, 0.8),
    y = rnorm(100, 2, 0.8),
    class = rep('red', 100)
  )
test2 <-
  data.frame(
    x = rnorm(100, 0, 0.8),
    y = rnorm(100, 0, 0.8),
    class = rep('blue', 100)
  )
test <- rbind(test, test2)
testplot <- ggplot()
testplot <-
  testplot + geom_jitter(data = test, aes(x, y, color = class))
testplot


mu_est <- function(data, results) {
  data <- as.data.frame(data)
  classes <- unique(results)
  t <- table(results)
  mu <- sapply(classes, function(class) {
    colMeans(data[results == class,])
  })
  mu <- t(mu)
  rownames(mu) <- as.character(classes)
  return(mu)
}

sigma_est <- function(data, results) {
  G <- unique(results)
  K <- length(G)
  N <- length(results)
  mu <- mu_est(data, results)
  n <- dim(data)[2]
  Bn <- diag(0,ncol=n,nrow=n)
  sapply(1:K, function(k) {
    apply(data[results == G[k], ], 1, function(x) {
      Bn <<- Bn + ((x - mu[k, ]) %*% t(x - mu[k, ]))
    })
  })
  return(Bn/(N-K))
}
  
mu <- mu_est(test[c(1,2)], test$class)
sigma_est(test[c(1,2)], test$class)

Omega <- diag(3, nrow(sigma_est(test[c(1,2)], test$class)))

h <- function(x){
  x
}

dist_to_class <- function(x, data, results, class) {
  class_index <- which(as.character(results) == class)
  Mu_class <- mu_est(data[class_index, ], results[class_index])
  Sigma_class <- sigma_est(data[class_index, ], result[class_index])
  Matrix <- Sigma_class + Omega
  
  x_mu <- as.vector(h(x) - h(Mu_class))
  print(x_mu)
  Dist <- t(x_mu) %*% solve(Matrix) %*% x_mu
  Dist[1,1]
}

y <- Dist_to_class(c(1,2), test[c(1,2)], test$class, "red")

class_distances <- function(x, data, results){
  classes <- as.vector(unique(results))
  M <- matrix(NA, ncol = length(classes), nrow = 1)
  colnames(M) <- classes
  for(i in seq_along(classes)){
    M[1, i] <- Dist_to_class(x, data, results, classes[i])
  }
  return(M)
}

Class_distances(c(1,2), test[c(1, 2)], test$class)

closest_class <- function(x, data, results){
  M <- class_distances(x, data, results)
  class_index <- which.min(M[1, ])
  colnames(M)[class_index]
}

closest_class(c(1,2), test[c(1, 2)], test$class)


generate_grid <- function(x_min = 0, x_max = 0, length = 0){
  if(length < 0){
    stop("length must be positive or zero")
  }
  if(x_max < x_min){
    stop("Upper limit must be larger than lower limit")
  }
  
  x_rep = numeric(0)
  y_rep = numeric(0)
  
  
  for(i in 0:length){
    x <- x_min + (x_max - x_min)/length * i
    x_rep <- c(x_rep, rep(x, times = (length+1)))
  }
  
  y <- seq(x_min, x_max, length.out = (length+1))
  y_rep <- rep(y, times = (length+1))
  
  Grid <- matrix(c(x_rep, y_rep), nrow = ((length+1)^2), ncol = 2)
  Grid <- as.data.frame(Grid)
  
}

Grid <- generate_grid(-5, 5, 20)

grid_classes <- function(Grid, data, results){
  classes <- character(nrow(Grid))
  for(i in 1:nrow(Grid)){
    classes[i] <- closest_class(unlist(Grid[i, ]), data, results)
  }
  return(classes)
}

Grid_class <- grid_classes(Grid, test[c(1,2)], test$class)
Grid <- cbind(Grid, Grid_class)
colnames(Grid) <- c("x", "y", "class")

testplot <- ggplot()
testplot <-
  testplot + geom_jitter(data = test, aes(x, y, color = class), height = 0, width = 0) + geom_jitter(data = Grid, aes(x, y, color = class), shape = 43, height = 0, width = 0)
testplot

plot(Grid[, 1], Grid[, 2], pch = 3)
points(test[, 1], test[, 2])

# Vergleiche alle x mit allen mu
# Index so setzen, dass die Klassenspezifischen Kovarianzmatrizen ausrechenbar rauskomme
# Basisexpansion schreiben

