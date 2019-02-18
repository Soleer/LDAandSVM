
library(ggplot2)
library(gridExtra)
set.seed(0)
#Regular Discriminant analysis from 4.3.1
source("R/LDA_QDA.R")

alpha_gamma_crossFit <-function( ){
  #TODO
  
  if(missing()){
    res <- list(alpha = 1, gamma = 1)
    return(res) 
  }
  
  optim(par = c(1,1),test, x=, y=y, lower=c(0,1),upper=c(0,1), method="L-BFGS-B")
}

calc_error <- function(data, results, f) {
  G <- unique(results)
  estimated <- apply(data, 1, f)
  of_Data <- lapply(G, function(class) {
    c <- as.character(class)
    t <- table(estimated[results == class])
    number <- sum(t)
    classresults <- as.list(t[as.character(G)] / number)
    right <- t[c] / number
    wrong <- (1 - right)
    col <- unlist(list(classresults, right, wrong))
    return(col)
  })
  
  of_Results <- lapply(G, function(class) {
    c <- as.character(class)
    t <- table(results[estimated == class])
    number <- sum(t)
    classresults <- as.list(t[as.character(G)] / number)
    right <- t[c] / number
    wrong <- (1 - right)
    col <- unlist(list(classresults, right, wrong))
    return(col)
  })
  probs_of_Data <-
    data.frame(class = c(as.character(G), 'right', 'wrong'), of_Data)
  probs_of_Results <-
    data.frame(class = c(as.character(G), 'right', 'wrong'), of_Results)
  colnames(probs_of_Data) <- c('class', as.character(G))
  colnames(probs_of_Results) <- c('class', as.character(G))
  miss <-
    sum(probs_of_Data[probs_of_Data$class == 'wrong', 1:length(G)+1]) / length(G)
  miss <- round(miss,2)
  return(list(probs_of_Data, probs_of_Results, miss))
}


sigma_class <- function(data, mu = colMeans(data), alpha = 1, gamma = 1) {
  
  sigma_class <- sigma_class(data, mu)* alpha + (1-alpha)*sigma_est(data, results, gamma) 
  #TODO super sigma_class, 
  #not recursive
  return(sigmaClass) 
}

sigma_est <-function(data, results, gamma = 1){
  kleinSigma<- 1 #TOO
  s<-sigma_est(data, results)
  n<-ncol(s)
  sigma_est <-  s* gamma + (1-gamma)* kleinSigma*kleinSigma*diag(n)
  return(sigma_est)
}

##kopiert, aber löschen
library(ggplot2)
library(gridExtra)
set.seed(0)
#estimators of chapter 4.3

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