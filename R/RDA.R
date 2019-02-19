library(ggplot2)
library(gridExtra)
set.seed(0)
#Regular Discriminant analysis from 4.3.1

#'sigma_class
#'
#'given a dataframe with parameters of Observations of one Class sigma_class returns the
#'covariance matrix of the Data.
#'
#'@param data Dataframe of Parameters for all Observations
#'@return The covariance matrix of the Data
sigma_class2 <- function(data, mu = colMeans(data)) {
  n <- dim(data)[2]
  Bn <- diag(0, ncol = n, nrow = n)
  apply(data, 1, function(x) {
    Bn <<- Bn + ((x - mu) %*% t(x - mu))
  })
  return(Bn / (dim(data)[1] - 1))
}

sigma_class <- function(data, mu = colMeans(data), alpha, gamma) {
  sigma_class <-
    sigma_class2(data, mu) * alpha + (1 - alpha) * sigma_est(data, results, gamma)
  
  
  return(sigma_class)
}

sigma_est <- function(data, results, gamma) {
  kleinSigma <- 1 #TODO
  s <- sigma_est2(data, results)
  n <- ncol(s)
  sigma_est <-  s * gamma + (1 - gamma) * kleinSigma * kleinSigma * diag(n)
  return(sigma_est)
}

sigma_est2 <- function(data, results) {
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

####cross validation/ tuning parameters

alpha_gamma_crossFit <- function(data, results) {
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
  
  #
  alpha_gamma_error <- matrix(nrow = N, ncol = N)
  
  #iterates all
  for (i in 1:N) { #TODO apply
    for (j in 1:N) {
      alpha <- alpha_gamma[i, j, "alpha"]
      gamma <- alpha_gamma[i, j, "gamma"]
      
      alpha_gamma_error[i, j] <-
        validationErrorRate(data, results, alpha, gamma)
      
    }
  }
  
  #finds best
  coordinates <-
    which(alpha_gamma_error == min(alpha_gamma_error), arr.ind = TRUE)
  
  alpha <- alpha_gamma[c(coordinates, 1)]
  gamma <- alpha_gamma[c(coordinates, 2)]
  
  return(list(alpha, gamma)) #TODO Format
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
   
    #classifier <- RDA(training_data, results, alpha, gamma)%>%classify TODO
    delta <- RDA3(training_data, results, alpha, gamma)
    classifier <- classify(delta)
   
     #validation on block j
    validation_data <- data[[i]]
    current_error <- calc_error(validation_data, results, classifier)
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
calc_error <- function(data, results, f) {
  G <- unique(results)
  force(data)
  force(f)
  estimated <- apply(data, 1, f)
  of_Data <- lapply(G, function(class) {
    c <- as.character(class)
    t <- table(estimated[results == class])
    number <- sum(t)
    classresults <- as.list(t[as.character(G)] / number)
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

###### not RDA specific

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
    colMeans(data[results == class,])
  })
  mu <- t(mu)
  return(mu)
}


targets <- function(vector) {
  n <- length(vector)
  En <- diag(1, n, n)
  V <- matrix(vector,
              nrow = n,
              ncol = n,
              byrow = TRUE)
  D <- En - V
  results <- sapply(1:n, function(i) {
    D[i,] %*% D[i,]
  })
  return(results)
}

#return closest target
class_by_targets <- function(uresults, f) {
  classfunction <- function(x) {
    return(uresults[which.min(targets(f(x)))])
  }
  return(classfunction)
}
#return max
classify <- function(uresults, f) {
  classfunction <- function(x) {
    return(uresults[which.max(f(x))])
  }
  return(classfunction)
}

#seperating lines test not working

create_id <- function(a, b, rf) {
  getzero <- function(x) {
    rf(x)[a] - rf(x)[b]
  }
  return(getzero)
}

get_Y_Value <- function(z, ploty) {
  upper = max(ploty)
  lower = min(ploty)
  if (z(lower) * z(upper) <= 0) {
    return(uniroot(z, ploty)$root)
  }
  else{
    return(NA)
  }
}

# classification functions -> G
#helpfunction
getseperatorfun <- function(a,
                            b,
                            rf,
                            y = c(-5, 5),
                            inv = FALSE) {
  getzero <- create_id(a, b, rf)
  sep <- function(x) {
    inverse <- inv
    z <- function(y) {
      if (inverse == FALSE) {
        getzero(c(x, y))
      }
      else{
        getzero(c(y, x))
      }
    }
    y <- get_Y_Value(z, y)
    if (inverse == FALSE) {
      if (is.na(y) || which.max(rf(c(x, y))) == a ||
          which.max(rf(c(x, y))) == b) {
        return(y)
      }
      return(NA)
    }
    else{
      if (is.na(y) || which.max(rf(c(y, x))) == a ||
          which.max(rf(c(y, x))) == b) {
        return(y)
      }
      return(NA)
    }
  }
  return(sep)
}

#RDA
RDA3 <- function(data, results, alpha, gamma) {
  G <- unique(results)
  K <- length(G)
  #set lamda 
  p <- log(pi_est(results))
  mu <- mu_est(data, results)
  sigma_list <- lapply(1:K, function(k) {
    sigma_class(data[results == G[k],], mu[k], alpha, gamma)
  })
  sigma_inv <- lapply(sigma_list, solve)
  sigma_log_det <- lapply(sigma_list, function(x) {
    log(det(x))
  })
  delta <- function(x) {
    result <- sapply(1:K, function(k) {
      -1 / 2 * sigma_log_det[[k]] - 1 / 2 * t(x - mu[k, ]) %*% sigma_inv[[k]] %*% (x - mu[k, ])
    }) + p
    return(result)
  }
  return(delta)
}

#'RDA
#'
#'implements the RDA classification (mix from LDA and QDA) by cross validation for the parameters alpha and gamma
#'
#'@param data Dataframe of Parameters for all Observations
#'@return the delta functions of the algorithms
RDA2 <- function(data, results) {
  
  
  alpha_gamma <- alpha_gamma_crossFit(data, results)
  alpha <- alpha_gamma$alpha
  gamma <- alpha_gamma$gamma
  
  delta <- RDA3(data, results, alpha, gamma)
  
  return(delta)
}


RDA <- function(data_set) {
  data <- data_set$data
  results <- data_set$results
  delta <- RDA2(data, results)
  return(delta)
}

###TEST
source("R/oop.R")
data_set <- make_testset(N=3)
"data <- data_set$data
data
K <- min(10, nrow(data))
split(data, sample(1:K, nrow(data), replace = T))"
RDA(test_set)

