#Estimators
pi_est <- function(results) {
  classes <- unique(results)
  K <- length(results)
  obs <- table(results)
  #return possibilitys in order of classes
  vec <- sapply(classes, function(class)
    obs[as.character(class)] / K)
  return(vec)
}
#'mue_est
#'
#'given a dataframe with Parameters of Observations and a second dataframe with the corresponding Classes
#'mu_es returns a Matrix with the mean vectors of the classes as rows.
#'
#'@param data Dataframe of Parameters for all Observations
#'@param results Vector of corresponding Classes to the Data
#'@return A Matrix with the mean vectors of the classes as rows
mu_est <- function(data, results) {
  data <- as.data.frame(data)
  classes <- unique(results)
  mu <- sapply(classes, function(class) {
    colMeans(data[results == class, ])
  })
  mu <- t(mu)
  return(mu)
}
#'sigma_class
#'
#'given a dataframe with parameters of Observations of one Class sigma_class returns the
#'covariance matrix of the Data.
#'
#'@param data Dataframe of Parameters for all Observations
#'@return The covariance matrix of the Data
sigma_class <- function(data, mu = colMeans(data)) {
  n <- dim(data)[2]
  Bn <- diag(0, ncol = n, nrow = n)
  apply(data, 1, function(x) {
    Bn <<- Bn + ((x - mu) %*% t(x - mu))
  })
  return(Bn / (dim(data)[1] - 1))
}

sigma_est <- function(data, results) {
  G <- unique(results)
  K <- length(G)
  N <- length(results)
  mu <- mu_est(data, results)
  n <- dim(data)[2]
  Bn <- diag(0, ncol = n, nrow = n)
  sapply(1:K, function(k) {
    apply(data[results == G[k],], 1, function(x) {
      Bn <<- Bn + ((x - mu[k,]) %*% t(x - mu[k,]))
    })
  })
  return(Bn / (N - K))
}

sigma_bet_est <- function(data, results){
  G <- unique(results)
  mu <- mu_est(data, results)
  total_mean <- colMeans(data)
  N <- length(G)
  B_i <- lapply(1:N, function(i){
    length(results[results == G[i]]) * (mu[i, ] - total_mean) %*% t(mu[i, ] - total_mean)
  })
  B <- Reduce(`+`, B_i)/(length(results)-length(G))
  return(B)
}

