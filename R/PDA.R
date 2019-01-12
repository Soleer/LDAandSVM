library(ggplot2)
library(gridExtra)
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


h <- function(x){
  if(is.vector(x)){
    len <- length(x)
    for(i in 1:len){
      for(j in i:len){
        y <- x[i] * x[j]
        x <- c(x, y)
      }
    }
  }
  if(is.data.frame(x)){
    cols <- ncol(x)
    name <- names(x)
    for(i in 1:cols){
      for(j in i:cols){
        y <- x[, i] * x[, j]
        name <- c(name, paste(name[i], "*" , name[j]))
        x <- cbind(x, y)
        colnames(x) <- name
      }
    }
  }
  return(x)
}

test_expanded <- cbind(h(test[c(1, 2)]), test$class)
names(test_expanded)[ncol(test_expanded)] <- "class"
testplot <-
  make_plot(test[1:2], test$class, type = QDA, x = c(-5, 5), y = c(-5, 5), ppu = 5)

testplot



dist_to_class <- function(x,data, results, class) {
  class_index <- which(as.character(results) == class)
  Mu_class <- mu_est(data[class_index, ], results[class_index])
  Sigma_class <- sigma_est(data[class_index, ], result[class_index])
  Matrix <- Sigma_class + Omega
  
  x_mu <- as.vector(h(x) - h(Mu_class))

  Dist <- t(x_mu) %*% solve(Matrix) %*% x_mu
  Dist[1,1]
}

sigma_class <- function(data, mu= colMeans(data)){
  
  n <- dim(data)[2]
  Bn <- diag(0, ncol = n, nrow = n)
  apply(data, 1, function(x) {
    Bn <<- Bn + ((x - mu) %*% t(x - mu))
  })
  return(Bn/(dim(data)[1]-1))
}

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

sigma <- sigma_est(test[1:2], test_expanded$class)

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

pi_est <- function(results) {
  vec <- unique(results)
  n <- length(results)
  t <- table(results)
  return(sapply(vec, function(x)
    t[as.character(x)] / n))
}

LDA <- function(data, results) {
  G <- unique(results)
  K <- length(G)
  p <- log(pi_est(results))
  mu <- mu_est(data, results)
  sigma <- solve(sigma_est(data, results))
  delta <- function(x) {
    result <- sapply(1:K, function(k) {
      (x %*% sigma %*% mu[k, ] - 1 / 2 * mu[k, ] %*% sigma %*% mu[k, ])
    }) + p
    return(result)
  }
  return(delta)
}

QDA <- function(data, results) {
  G <- unique(results)
  K <- length(G)
  p <- log(pi_est(results))
  print(p)
  mu <- mu_est(data, results)
  sigma_list <- lapply(1:K, function(k) {
    sigma_class(data[results==G[k],],mu[k])
  })
  sigma_inv <- lapply(sigma_list,solve)
  delta <- function(x) {
    result <- sapply(1:K, function(k) {
      -1 / 2 * log(det(sigma_list[[k]])) - 1 / 2 * t(x - mu[k, ]) %*% sigma_inv[[k]] %*% (x - mu[k, ])
    }) + p
    return(result)
  }
  return(delta)
}

LDA_exp <- function(data, results){
  data_exp <- h(data)
  G <- unique(results)
  K <- length(G)
  p <- log(pi_est(results))
  mu <- mu_est(data, results)
  sigma <- solve(sigma_est(data_exp, results))
  delta <- function(x) {
    result <- sapply(1:K, function(k) {
      (h(x) %*% sigma %*% h(mu[k, ]) - 1 / 2 * h(mu[k, ]) %*% sigma %*% h(mu[k, ]))
    }) + p
    return(result)
  }
  return(delta)
}

QDA_exp <- function(data, results) {
  data_exp <- h(data)
  G <- unique(results)
  K <- length(G)
  p <- log(pi_est(results))
  print(p)
  mu <- mu_est(data, results)
  sigma_list <- lapply(1:K, function(k) {
    sigma_class(data_exp[results==G[k],],mu[k])
  })
  sigma_inv <- lapply(sigma_list,solve)
  delta <- function(x) {
    result <- sapply(1:K, function(k) {
      -1 / 2 * log(det(sigma_list[[k]])) - 1 / 2 * t(h(x) - h(mu[k, ])) %*% sigma_inv[[k]] %*% (h(x) - h(mu[k, ]))
    }) + p
    return(result)
  }
  return(delta)
}

PDA <- function(data, results) {
  data_exp <- h(data)
  G <- unique(results)
  K <- length(G)
  #p <- log(pi_est(results))
  mu <- mu_est(data, results)
  sigma_list <- lapply(1:K, function(k) {
    sigma_class(data_exp[results==G[k],],mu[k])
  })
  Matrix <- lapply(sigma_list, function(x) solve(x + diag(0, nrow=nrow(x), ncol=ncol(x))))
  
  delta <- function(x) {
    result <- sapply(1:K, function(k) {
      - (t(as.vector(h(x) - h(mu[k, ]))) %*% Matrix[[k]] %*% (as.vector(h(x) - h(mu[k, ])))) # Minus the function so that max is the searched value
    }) 
    return(result)
  }
  return(delta)
}

classify <- function(uresults, f) {
  classfunction <- function(x) {
    return(uresults[which.max(f(x))])
  }
  return(classfunction)
}

make_plot <- function(data,
                      results,
                      type = LDA,
                      y = c(-5, 5),
                      x = c(-5, 5),
                      ppu = 10,
                      owntitle) {
  #prama
  f <- type(data, results)
  uresults <- unique(results)
  classfun <- classify(uresults, f)
  n <- length(uresults)
  xtimes <- (x[2] - x[1]) * ppu
  ytimes <- (y[2] - y[1]) * ppu
  mainplotdata <- cbind(data, results)
  mainplot <- ggplot() + xlim(x[1],x[2])+ylim(y[1],y[2])
  mainplot <-
    mainplot + geom_jitter(
      data = mainplotdata,
      aes(x, y, color = results),
      shape = 16,height = 0,width = 0
    )
  #background
  background <-
    data.frame(x = rep(seq(x[1], x[2], length.out = xtimes), times = ytimes),
               y = c(sapply(seq(y[1], y[2], length.out = ytimes), function(x)
                 rep(x, times = xtimes))))
  background$class <- apply(background, 1, classfun)
  mainplot <- mainplot + geom_jitter(
    data = background,
    aes(x, y, color = class),
    shape = 3,
    height = 0,width = 0
  ) + ggtitle(owntitle)
  #Lines???
  
  
  
  return(mainplot)
}

make_test <- function(ninputs = 100,
                      nclasses = 2,
                      sigma = 0.6,
                      x = c(-5, 5),
                      y = c(-5, 5)) {
  nsigma <- length(sigma)
  xcoord <- sample(x[1]:x[2], nclasses, replace = FALSE)
  ycoord <- sample(y[1]:y[2], nclasses, replace = FALSE)
  print(1)
  test <-
    data.frame(
      x = rnorm(ninputs, xcoord[1], sigma[1]),
      y = rnorm(ninputs, ycoord[1], sigma[1]),
      class = rep('A', times = ninputs)
    )
  sapply(2:nclasses, function(i) {
    class <- LETTERS[i]
    test <<-
      rbind(test, data.frame(
        x = rnorm(ninputs, xcoord[i], sigma[i]),
        y = rnorm(ninputs, ycoord[i], sigma[i]),
        class = rep(class, times = ninputs)
      ))
  })
  return(test)
}


x <- c(-10, 10)
y <- c(-10, 10)
sig <- c(1, 1, 1, 1)
test <- make_test(100, nclasses = 4, sigma = sig )
testplot <-
  make_plot(test[c('x', 'y')], test$class, type =  LDA, x, y, ppu = 5, owntitle = "LDA")

testplot1 <-
  make_plot(test[c('x', 'y')], test$class, type =  QDA, x, y, ppu = 5, owntitle = "QDA")

testplot2 <-
  make_plot(test[c('x', 'y')], test$class, type =  LDA_exp, x, y, ppu = 5, owntitle = "LDA_exp")

testplot3 <-
  make_plot(test[c('x', 'y')], test$class, type =  QDA_exp, x, y, ppu = 5, owntitle = "QDA_exp")

testplot4 <-
  make_plot(test[c('x', 'y')], test$class, type =  PDA, x, y, ppu = 5, owntitle = "PDA")

grid.arrange(testplot,testplot1,testplot2,testplot3,testplot4, nrow=3, ncol=2)

########################################################################################
# 
# y <- Dist_to_class(c(1,2), test[c(1,2)], test$class, "red")
# 
# class_distances <- function(x, data, results){
#   classes <- as.vector(unique(results))
#   M <- matrix(NA, ncol = length(classes), nrow = 1)
#   colnames(M) <- classes
#   for(i in seq_along(classes)){
#     M[1, i] <- Dist_to_class(x, data, results, classes[i])
#   }
#   return(M)
# }
# 
# Class_distances(c(1,2), test[c(1, 2)], test$class)
# 
# closest_class <- function(x, data, results){
#   M <- class_distances(x, data, results)
#   class_index <- which.min(M[1, ])
#   colnames(M)[class_index]
# }
# 
# closest_class(c(1,2), test[c(1, 2)], test$class)
# 
# 
# generate_grid <- function(x_min = 0, x_max = 0, length = 0){
#   if(length < 0){
#     stop("length must be positive or zero")
#   }
#   if(x_max < x_min){
#     stop("Upper limit must be larger than lower limit")
#   }
#   
#   x_rep = numeric(0)
#   y_rep = numeric(0)
#   
#   
#   for(i in 0:length){
#     x <- x_min + (x_max - x_min)/length * i
#     x_rep <- c(x_rep, rep(x, times = (length+1)))
#   }
#   
#   y <- seq(x_min, x_max, length.out = (length+1))
#   y_rep <- rep(y, times = (length+1))
#   
#   Grid <- matrix(c(x_rep, y_rep), nrow = ((length+1)^2), ncol = 2)
#   Grid <- as.data.frame(Grid)
#   
# }
