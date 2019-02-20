#Estimators for expanded data only
#not for users

mu_exp <- function(data, set) {
  data <- as.data.frame(data)
  classes <- set$classes
  mu <- lapply(classes, function(class) {
    colMeans(data[set$results == class,])
  })
  names(mu) <- set$classnames
  return(mu)
}

sigma_class_exp <- function(data, mu = colMeans(data)) {
  n <- dim(data)[2]
  Bn <- diag(0, ncol = n, nrow = n)
  apply(data, 1, function(x) {
    Bn <<- Bn + tcrossprod(x - mu)
  })
  return(Bn / (dim(data)[1] - 1))
}

mu_est <- function(data, results, G) {
  lapply(G, function(class) {
    colMeans(data[results == class, ])
  })
}

sigma_est <- function(set) {
  G <- set$classes
  K <- set$n_classes
  N <- set$n_obs
  mu <- set$mean
  n <- set$dim
  Bn <- diag(0, ncol = n, nrow = n)
  sapply(1:K, function(k) {
    apply(set$data[set$results == G[k], ], 1, function(x) {
      Bn <<- Bn + tcrossprod(x - mu[[k]])
    })
  })
  return(Bn / (N - K))
}


sigma_exp <- function(data, results) {
  G <- unique(results)
  K <- length(G)
  N <- length(results)
  mu <- mu_est(data, results, G)
  n <- dim(data)[2]
  Bn <- diag(0, ncol = n, nrow = n)
  sapply(1:K, function(k) {
    apply(data[results == G[k],], 1, function(x) {
      Bn <<- Bn + tcrossprod(x - mu[[k]])
    })
  })
  return(Bn / (N - K))
}

sigma_bet_exp <- function(data, results) {
  G <-
    unique(results)                    ##vector with all unique classnames in it. Nothing mentioned twice
  mu <-
    mu_est(data, results)             ##Gets the Class centroids of each class (the expected value)
  total_mean <-
    colMeans(data)            ##The total centroid of all data points (expected value of everything)
  N <- length(G)                          ##Number of unique classes
  B_i <-
    lapply(1:N, function(i) {
      ##Calculates the bewtween class covariance Matrix in two steps:
      length(results[results == G[i]]) * tcrossprod(mu[i,] - total_mean) ##1. Calculating a class specific part of the covariance matrix for every class
    })
  B <-
    Reduce(`+`, B_i) / (length(results) - N) ##Adding all parts, which are in one big list, together and dividing by a standarizing factor
  return(B)
}

small_sigma_est <- function(set){ 
  
  results <- set$results
  mu <- set$mean
  K <- set$n_classes
  N <- set$n_obs 
  classes <- set$classes
  
  #return (1) #TODO
  
  if (N != K) {
    print(set)
    sumOfClasses <- sapply(
      classes,
      FUN = function(k) {
        print(set)
        
        #data_of_class <- set$get_data_by_class(k)
        data_of_class <- set$data[results == k,]
        mu_of_k <-  mu[[k]]
        
        
        v <- apply(data_of_class, 1, function(x) {
          ((x - mu_of_k) ^ 2)
        })
        
        s <- sum(v)
        return(s)
      }
    )
    
    small_sigma <- mean(sumOfClasses) / (N - K)
  } else{
    
    small_sigma <- 0
  }

  return(small_sigma)
    
}

<<<<<<< HEAD

=======
testSmallSigma <- function() {
  N <- 5
  G<- 3
  test_set <- make_testset(N, G)
  
  test_that("smallSigma makes sense", {
    small_sigma_est(test_set)
    #TODO 
  })
}
>>>>>>> 2c7eae01fd701c5f3fc26c1f1784bb1989fa2ef2
