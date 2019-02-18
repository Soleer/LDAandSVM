
library(ggplot2)
library(gridExtra)
set.seed(0)
#Regular Discriminant analysis from 4.3.1
source("R/LDA_QDA.R")

alpha_gamma_crossFit <-function( ){
  #TODO
  
  if(missing()){
    res <- list(alpha = 1, gamma = 1)
    return res
  }
  return ""
}

sigma_class <- function(data, mu = colMeans(data), alpha = 1, gamma = 1) {
  sigmaClass <- sigma_class(data, mu)* alpha + (1-alpha)*sigma_est(data, results, gamma)
  return sigmaClass
}

sigma_est <-function(data, results, gamma = 1){
  return 
}