library(ggplot2)
library(gridExtra)
set.seed(0)
#Regular Discriminant analysis from 4.3.1

####cross validation/ tuning parameters

alpha_gamma_crossFit <- function(data_set) {
  data <- data_set$data
  results <- data_set$results
  #splits data in K equal sized training/validation samples
  #data
  K <- min(10, nrow(data))
  data <- split(data, sample(1:K, nrow(data), replace = T))
  #creates parameters to choose from
  N <- 5
  v <- seq(from = 0,
           to = 1,
           length.out = N) #TODO aequidistant?
  
  alpha_gamma <- array(v, dim = c(N, N, 2), dimnames = list(1:N, 1:N, c("alpha", "gamma")))

  #iterates all
  alpha_gamma_error <- matrix(nrow = N, ncol = N)
  for (i in 1:N) { 
    for (j in 1:N) {
      alpha <- alpha_gamma[i, j, "alpha"]
      gamma <- alpha_gamma[i, j, "gamma"]
      
      alpha_gamma_error[i, j] <-
        validationErrorRate(data, results, alpha, gamma)
      
    }
  }
  
  "alpha_gamma_error <- apply(alpha_gamma, c(1,2), FUN = function(x){ #TODO kontrollieren
    alpha <- x$alpha
    gamma <- x$gamma
    return(validationErrorRate(data, results, alpha, gamma))
  })"
  
  #finds best
  coordinates <-
    which(alpha_gamma_error == min(alpha_gamma_error), arr.ind = TRUE)
  
  alpha <- alpha_gamma[c(coordinates, 1)]
  gamma <- alpha_gamma[c(coordinates, 2)]
  
  return(list(alpha, gamma)) 
}

#'validationErrorRate
#'
#'calculates the mean total error rate of RDA for given alpha, gamma to determin 
#'  best selection in the cross fitting
#'@return total mean error rate
validationErrorRate <- function(data, results, alpha, gamma) {

  
  errors <- sapply(seq_along(data) , FUN = function(i){
    #training
    
    training_data <- do.call(rbind, data[-i]) 
    data_set <- make_set(data = training_data, by = ) #TODO
    #TODO source(Classifier_funs), OOP

    classifier <- RDA(set = data_set, alpha = alpha, gamma = gamma)
    
    #validation on block j
    validation_data_set <- data[[i]]
    current_error <- calc_error(validation_data_set, results, classifier)
    
    current_error
  })
  
  return(mean(errors))
}


#'calc_error
#'
#'calculates the total error rate of of a classifaction function on a dataset
#'
#'@param data Dataframe of Parameters for all Observations
#'@param results correct classes
#'@param f classification function
#'@return total error rate
calc_error <- function(data_set, results, f) {
  data <- data_set$data
  G <- unique(results)
  
  force(f)
  estimated <- apply(data, 1, f)
  
  of_Data <- lapply(G, function(class) {
    c <- as.character(class)
    t <- table(estimated[results == class])
    number <- sum(t)

    order <- t[G]
    order[is.na(order)] <- 0
    classresults <- as.list(order / number)
    
    right <- t[c] / number
    wrong <- (1 - right)
    
    return(col)
  })
  
  probs_of_Data <-
    data.frame(class = c(as.character(G), 'right', 'wrong'), of_Data)

  miss <-
    sum(probs_of_Data[probs_of_Data$class == 'wrong', 1:length(G) + 1]) / length(G)
  
  return(miss)
}



###TEST
source("R/oop.R")
source("R/Classifier_funs.R")
data_set <- make_testset(N=3, G = 3)
data_set$results
RDA(test_set)

