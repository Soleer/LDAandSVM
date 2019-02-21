#Regular Discriminant analysis from 4.3.1


####cross validation/ tuning parameters

#'alpha_gamma_crossFit
#'
#'cross validates for the best alpha and gamma
#'@param set data_set to be trained on for cross validation
#'@param K number of validation sets. Note that though K = 10 is common, it does take far too long in this code
#'@param N number of parameters to choose. Note that Omega(crossFit) = N^2
#'@return alpha, gamma
alpha_gamma_crossFit <- function(set, K = 3, N = 5) { #TODO adjust K 
  data <- data_set$data
  results <- data_set$results
  #splits data in K equal sized training/validation samples
  #datawarn
  n<- nrow(data) 

  K <- min(K, floor(n/2)) #so in each subpart are at least two elements in order for the the calculation of variances to work properly 

  chunk <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 
  # 
  # if(any(sapply(chunk, anyNA))){ 
  #   browser()
  #   warning("set$sigma contains Na RDA")
  # }
  # 
  partition <- chunk2(sample(1:n), K) #TODO randomorderpartition
  partition
  #splits results and data according to chosen partition with corresponding rows
  results <- lapply(partition, FUN = function(x){
    results[x]
  })
  
  data <- lapply(partition, FUN = function(x){
    data[x, ]
  })
  if(anyNA(data)){ #TODO
    warning("data contains Na (alpha_gamma_crossFit)") 
  }
   
  #creates parameters to choose from in cross fitting
  #how many parameters shall be considered
  v <- seq(from = 0,
           to = 1,
           length.out = N) #TODO aequidistant?
  
  #array of all parameters for alpha and gwaramma
  alpha_gamma <- array(v, dim = c(N, N, 2), dimnames = list(1:N, 1:N, c("alpha", "gamma")))

  #iterates thrue all possible parameters and saves there error rate in a matrix (to choose the minimum later)
  alpha_gamma_error <- matrix(nrow = N, ncol = N)
  for (i in 1:N) { 
    for (j in 1:N) {
      alpha <- alpha_gamma[i, j, "alpha"]
      gamma <- alpha_gamma[i, j, "gamma"]
      
      alpha_gamma_error[i, j] <-
        validationErrorRate(data, results, alpha, gamma)
      
    }
  }
  
  #alternative implementation with apply
  "alpha_gamma_error <- apply(alpha_gamma, c(1,2), FUN = function(x){ 
    alpha <- x$alpha
    gamma <- x$gamma
    return(validationErrorRate(data, results, alpha, gamma))
  })"
  
  #finds best option of alpha and gamma. picks random, if equal do exist
  X <- which(alpha_gamma_error == min(alpha_gamma_error), arr.ind = TRUE)
  coordinates <-X[sample(nrow(X),size=1,replace=TRUE),]

  
  alpha <- alpha_gamma[coordinates[1], coordinates[2],"alpha"]
  gamma <- alpha_gamma[coordinates[1], coordinates[2],"gamma"]
  
  return(list(alpha, gamma)) 
}

#'validationErrorRate
#'
#'calculates the mean total error rate of RDA for given alpha, gamma to determin 
#'  best selection in the cross fitting
#'@param data Dataframe of Parameters for all Observations
#'@param results correct classes
#'@param alpha alpha to be evaluated
#'@param gamma gamma to be evaluated
#'@return total mean error rate of all validations
validationErrorRate <- function(data, results, alpha, gamma) {

  #vector of error rates on a single validation slot
  errors <- sapply(seq_along(data) , FUN = function(i){
    
    #training on all data parts except the validation slot
    training_data <- do.call(rbind, data[-i]) 
    if(anyNA(training_data)){ #TODO
      warning("training_data contains Na (alpha_gamma_crossFit/validationErrorRate)") 
    }
    training_results <- unlist(results[-i])
    training_dataframe <- cbind(training_results, training_data) 
    if(anyNA(training_dataframe)){ #TODO
      warning("training_dataframe contains Na (alpha_gamma_crossFit/validationErrorRate)") 
    }
    data_set <- make_set(data = training_dataframe, by = "training_results")
    if(any(sapply(data_set$data, anyNA))){ #TODO
      warning("training_dataframe contains Na (alpha_gamma_crossFit/validationErrorRate)") 
    }
      #in order to generate a RDA object for it
      classifier_obj <- RDA(set = data_set, alpha = alpha, gamma = gamma)
      
      # if(is.null(classifier_obj)){ #singularities may occur TODO
      #   return(Inf)
      # }
      classifier <- classifier_obj$func
    
    #validation on block i
    validation_data_set <- data[[i]]
    if(anyNA(validation_data_set)){ #TODO
      warning("validation_data_set contains Na (alpha_gamma_crossFit/validationErrorRate)") 
    }
    validation_results <- results[[i]]
    
    current_error <- calc_miss(validation_data_set, validation_results, classifier)
    
    return(current_error)
  })
  
  return(mean(errors))
}

test_cross<- function(){
  numberOfTest <- 2
  
  #attributes of each test
  nobservations <- 10#number of observations per class
  nclass <- 3 #number of classes
  dimParameters <- 2 #number of parameters 
    
  #attributes of alpha Gamma cross fit
  numberOfValidations <- 3
  accuracyOfParameters <- 20
  
  sets <- lapply(1:numberOfTest, FUN= function(i){

    make_testset(N = nobservations, K = nclass, P = dimParameters)
  })

  alpha_gammas <- lapply(sets, FUN = function(set){
    alpha_gamma <- alpha_gamma_crossFit(set, K = numberOfValidations, N = accuracyOfParameters)
    print(alpha_gamma)
    return(alpha_gamma)
  })

  print(alpha_gammas)
}
