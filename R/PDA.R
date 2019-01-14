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



basis_exp <- function(type){
  if(type == "quad"){
    return(quad <- function(x){
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
            })
  }
  if(type == "cube"){
    return(cube <- function(x) {
              quad <- basis_exp("quad")
              quad_x <- quad(x)
              if (is.vector(x)) {
                len <- length(x)
                for (i in 1:len) {
                  for (j in i:len) {
                    for (k in j:len) {
                      y <- x[i] * x[j] * x[k]
                      x <- c(x, y)
                    }
                  }
                }
                x <- c(quad_x, x[(len+1) : length(x)])
              }
              if (is.data.frame(x)) {
                len <- ncol(x)
                name <- names(x)
                for (i in 1:len) {
                  for (j in i:len) {
                    for (k in j:len) {
                      y <- x[, i] * x[, j] * x[, k]
                      name <-
                        c(name, paste(name[i], "*" , name[j], "*", name[k]))
                      x <- cbind(x, y)
                      colnames(x) <- name
                    }
                  }
                }
                x <- cbind(quad_x, x[, (len+1) : ncol(x)])
              }
              return(x)
            })
  }
}

h <- basis_exp("cube")
h(c(1,2))
h(test[1:10, c(1,2)])

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
<<<<<<< HEAD


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
=======
>>>>>>> b46035dd176ffe62ea67a1677506a8e75311671d

pi_est <- function(results) {
  vec <- unique(results)
  n <- length(results)
  t <- table(results)
  return(sapply(vec, function(x)
    t[as.character(x)] / n))
}

LDA <- function(data, results, ...) {
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

QDA <- function(data, results, ...) {
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

LDA_exp <- function(data, results, base){
  h <- basis_exp(base)
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

QDA_exp <- function(data, results, base) {
  h <- basis_exp(base)
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

PDA <- function(data, results, base) {
  h <- basis_exp(base)
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
<<<<<<< HEAD
      t(h(x) - h(mu[k, ])) %*% Matrix[[k]] %*% h(x) - h(mu[k, ])
    }) + p
=======
      - (t(as.vector(h(x) - h(mu[k, ]))) %*% Matrix[[k]] %*% (as.vector(h(x) - h(mu[k, ])))) # Minus the function so that max is the searched value
    }) 
>>>>>>> b46035dd176ffe62ea67a1677506a8e75311671d
    return(result)
  }
  return(delta)
}

<<<<<<< HEAD

=======
>>>>>>> b46035dd176ffe62ea67a1677506a8e75311671d
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
                      owntitle, 
                      base = NA) {
  #prama
  f <- type(data, results, base)
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
  make_plot(test[c('x', 'y')], test$class, type =  LDA_exp, base = "cube", x, y, ppu = 5, owntitle = "LDA_exp")

testplot3 <-
  make_plot(test[c('x', 'y')], test$class, type =  QDA_exp, base = "cube", x, y, ppu = 5, owntitle = "QDA_exp")

testplot4 <-
  make_plot(test[c('x', 'y')], test$class, type =  PDA, base = "cube", x, y, ppu = 5, owntitle = "PDA")

<<<<<<< HEAD
=======
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
<<<<<<< HEAD


## falsche Basiserweiterungen

# # basis_pol <- function(x){
# #   if(is.vector(x)){
# #     len <- length(x)
# #     for(i in 1:len){
# #       for(j in i:len){
# #         for(k in j:len){
# #           y <- x[i] * x[j] * x[k]
# #           x <- c(x, y) 
# #         }
# #       }
# #     }
# #   }
# #   if(is.data.frame(x)){
# #     cols <- ncol(x)
# #     name <- names(x)
# #     for(i in 1:cols){
# #       for(j in i:cols){
# #         for(k in j:cols){
# #           y <- x[, i] * x[, j] * x[, k]
# #           name <- c(name, paste(name[i], "*" , name[j], "*", name[k]))
# #           x <- cbind(x, y)
# #           colnames(x) <- name 
# #         }
# #       }
# #     }
# #   }
# #   return(x)
# # }
# 
# mult_bases <- function(base, exp){
#   if(is.vector(base) && is.vector(exp)){
#     base_len <- length(base)
#     exp_len <- length(exp)
#     x <- numeric(0)
#     doubles <- t(as.matrix(c(0, 0)))
#     for(i in 1:base_len){
#       for(j in 1:exp_len){
#         if(matrix_row(doubles, c(i, j)) == TRUE || matrix_row(doubles, c(j, i)) == TRUE){
#           ## Doppelnennungen ueberspringen
#         }
#         else{
#           doubles <- rbind(doubles, c(i, j))
#           y <- base[i] * exp[j]
#           x <- c(x, y)
#         }
#       }
#     }
#     return(x)
#   }
#   if(is.data.frame(base) && is.data.frame(exp)){
#     base_names <- names(base)
#     exp_names <- names(exp)
#     name <- character(0)
#     doubles <- t(as.matrix(c(0, 0)))
#     x <- numeric(length(base))
#     for(i in 1:length(base)){
#       for(j in 1:length(exp)){
#         if(matrix_row(doubles, c(i, j)) == TRUE || matrix_row(doubles, c(j, i)) == TRUE){
#           #Doppelnennungen ueberspringen
#         }
#         else{
#           doubles <- rbind(doubles, c(i, j))
#           name <- c(name, paste(base_names[i], "*", exp_names[j]))
#           y <- base[i] * exp[j]
#           x <- cbind(x, y)
#         }
#       }
#     }
#     as.data.frame(x)
#     print(doubles)
#     x <- x[, -1]
#     colnames(x) <- name
#     return(x)
#   }
# }
# 
# 
# matrix_row<- function(mat, vec){
#   for(n in 1:nrow(mat)){
#     if(sum(as.vector(mat[n, ]) == vec) == length(vec)){
#       return(TRUE)
#     }
#   }
#   return(FALSE)
# }
# 
# basis_pol <- function(base, d = 3){
#   y <- base
#   if(d <= 1){
#     stop("Polynomial basis expansion must of degree >= 2")
#   }
#   if(d >=2){
#     exp <- mult_bases(base, base)
#     for(i in 2:d){
#       y <- cbind(y, exp)
#       exp <- mult_bases(base, exp)
#     }
#     return(y)
#   }
# }
# 
# basis_pol(test[1:10, c(1, 2)], d = 2)
=======
>>>>>>> b46035dd176ffe62ea67a1677506a8e75311671d
>>>>>>> 5600cc1a6e0b06c630ba64f34f382bea57b2f3e3
