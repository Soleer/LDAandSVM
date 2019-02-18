#Estimators
#'mue_est
#'
#'given a dataframe with Parameters of Observations and a second dataframe with the corresponding Classes
#'mu_es returns a Matrix with the mean vectors of the classes as rows.
#'
#'@param data Dataframe of Parameters for all Observations
#'@param results Vector of corresponding Classes to the Data
#'@return A Matrix with the mean vectors of the classes as rows
mu_exp <- function(data, set) {
  data <- as.data.frame(data)
  classes <- set$classes
  mu <- lapply(classes, function(class) {
    colMeans(data[set$results == class, ])
  })
  names(mu) <- set$classnames
  return(mu)
}
#'sigma_class
#'
#'given a dataframe with parameters of Observations of one Class sigma_class returns the
#'covariance matrix of the Data.
#'
#'@param data Dataframe of Parameters for all Observations
#'@return The covariance matrix of the Data
sigma_class_exp <- function(data, mu = colMeans(data)) {
  n <- dim(data)[2]
  Bn <- diag(0, ncol = n, nrow = n)
  apply(data, 1, function(x) {
    Bn <<- Bn + tcrossprod(x-mu)
  })
  return(Bn / (dim(data)[1] - 1))
}
mu_est <- function(data,results,G){
  lapply(G, function(class){
    colMeans(data[results == class,])
  })
}

sigma_exp <- function(data, results) {
  G <- unique(results)
  K <- length(G)
  N <- length(results)
  mu <- mu_est(data, results,G)
  n <- dim(data)[2]
  Bn <- diag(0, ncol = n, nrow = n)
  sapply(1:K, function(k) {
    apply(data[results == G[k], ], 1, function(x) {
      Bn <<- Bn + tcrossprod((x - mu[[k]]))
    })
  })
  return(Bn / (N - K))
}

sigma_bet_exp <- function(data, results){
  G <- unique(results)                    ##vector with all unique classnames in it. Nothing mentioned twice
  mu <- mu_est(data, results)             ##Gets the Class centroids of each class (the expected value)
  total_mean <- colMeans(data)            ##The total centroid of all data points (expected value of everything)
  N <- length(G)                          ##Number of unique classes
  B_i <- lapply(1:N, function(i){         ##Calculates the bewtween class covariance Matrix in two steps:
    length(results[results == G[i]]) * (mu[i, ] - total_mean) %*% t(mu[i, ] - total_mean) ##1. Calculating a class specific part of the covariance matrix for every class
  })
  B <- Reduce(`+`, B_i)/(length(results)-N) ##Adding all parts, which are in one big list, together and dividing by a standarizing factor
  return(B)
}

