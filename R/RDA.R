#Regular Discriminant analysis from 4.3.1

####cross validation/ tuning parameters

#'alpha_gamma_crossFit
#'
#'cross validates for the best alpha and gamma
#'@param data_set to be trained on for cross validation
#'@return alpha, gamma
#'@export
alpha_gamma_crossFit <- function(data_set) {
  data <- data_set$data
  results <- data_set$results
  #splits data in K equal sized training/validation samples
  #data
  n<- nrow(data) 

  K <- min(10, n)
  partition<-split(1:n, sample(1:K, n, replace = T)) #TODO ?equally sized
  
  results <- lapply(partition, FUN = function(x){
    results[x]
  })
  data <- lapply(partition, FUN = function(x){
    data[x, ]
  })
   
  #creates parameters to choose from in cross fitting
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
#'#'@param data Dataframe of Parameters for all Observations
#'@param results correct classes
#'@param alpha alpha to be evaluated
#'@param gamma gamma to be evaluated
#'@return total mean error rate of all validations
validationErrorRate <- function(data, results, alpha, gamma) {
  results
  errors <- sapply(seq_along(data) , FUN = function(i){
    #training

    training_data <- do.call(rbind, data[-i]) 
    training_results <- unlist(results[-i])

    training_dataframe <- cbind(training_results, training_data) 
    print(colnames(training_dataframe))
    

    data_set <- make_set(data = training_dataframe, by = "training_results") 


    classifier_set <- RDA(set = data_set, alpha = alpha, gamma = gamma)$func
    
    #validation on block j
    validation_data_set <- data[[i]]
    validation_results <- results[[i]]
    
    #TODO call correctly
    calc_totalMiss <- function(set, name)
    current_error <- calc_miss(validation_data_set, validation_results, classifier)
    
    return(current_error)
  })
  
  return(mean(errors))
}
