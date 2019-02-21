#helpfunction
targets <- function(vector) {
  n <- length(vector)
  En <- diag(1, n, n)
  V <- matrix(vector,
              nrow = n,
              ncol = n,
              byrow = TRUE)
  D <- En - V
  results <- sapply(1:n, function(i) {
    D[i, ] %*% D[i, ]
  })
  return(results)
}

#return closest target
class_by_targets <- function(classes, delta) {
  classfunction <- function(x) {
    return(classes[which.min(targets(delta(x)))])
  }
  return(classfunction)
}

#return max
classify <- function(classes, delta) {
  classfunction <- function(x) {
    pos_max <- which.max(delta(x))
    return(classes[pos_max])
  }
  return(classfunction)
}


## LDA, QDA, PDA, RDA

#' LDA
#'
#' The LDA classification function as described in Hastie et al. "The Elements Statistical Learning" (2009)
#'
#' @param set A R6 data_set object initialized with make_set. @seealso make_set
#' @return Returns a list with the name of the created LDA function in the given set in the first entry and the actual classification
#' function in the second entry and also saves it in the input set.
#' @examples
#' test <- make_testset()
#' func_name <- LDA(test)[['name']]
#' @export
LDA <- function(set) {
  if (!is.data_set(set)) {
    stop("Input must be of class 'data_set' (?make_set)")
  }
  if (length(set$func) > 0) {
    slot <- character(0)
    sapply(set$func_info, function(l) {
      if (!is.null(l[["type"]])) {
        if (l[["type"]] == "LDA") {
          slot <<- l[["name"]]
        }
      }
    })
    if (length(slot) > 0) {
      return(list(name = slot, func = set$func[[slot]]))
    }
  }
  G <- set$classes
  K <- set$n_classes
  p <- log(unlist(set$pi))
  mu <- set$mean
  sigma <- solve(sigma_est(set))
  delta <- function(x) {
    result <- sapply(1:K, function(k) {
      (x %*% sigma %*% mu[[k]] - 1 / 2 * mu[[k]] %*% sigma %*% mu[[k]])
    }) + p
    return(result)
  }
  classify_func <- classify(set$classes, delta)
  return(set$set_function(
    classify_func,
    type = "LDA",
    list(base = 'id', description =
           "basic LDA function")
  ))
}


#' QDA
#'
#' The QDA classification function as described in Hastie et al. "The Elements Statistical Learning" (2009)
#'
#' @param set A R6 data_set object initialized with make_set. @seealso make_set
#' @return Returns a list with the name of the created QDA function in the given set in the first entry and the actual classification
#' function in the second entry and saves it in the input set.
#' @examples
#' test <- make_testset()
#' func_name <- QDA(test)[['name']]
#' @export
QDA <- function(set) {
  if (!is.data_set(set)) {
    stop("Input must be of class 'data_set' (?make_set)")
  }
  if (length(set$func) > 0) {
    slot <- character(0)
    sapply(set$func_info, function(l) {
      if (!is.null(l[["type"]])) {
        if (l[["type"]] == "QDA") {
          slot <<- l[["name"]]
        }
      }
    })
    if (length(slot) > 0) {
      return(list(name = slot, func = set$func[[slot]]))
    }
  }
  G <- set$classes
  K <- set$n_classes
  p <- log(unlist(set$pi))
  mu <- set$mean
  sigma_inv <- lapply(set$sigma, solve)
  delta <- function(x) {
    result <- sapply(1:K, function(k) {
      -1 / 2 * log(det(set$sigma[[k]])) - 1 / 2 * t(x - mu[[k]]) %*% sigma_inv[[k]] %*% (x - mu[[k]])
    }) + p
    return(result)
  }
  classify_func <- classify(set$classes, delta)
  return(set$set_function(
    classify_func,
    type = "QDA",
    list(base = 'id', description =
           "basic QDA function")
  ))
}


#' PDA
#'
#' The PDA classification function as described in Hastie et al. "The Elements Statistical Learning" (2009)
#'
#' @param set A R6 data_set object initialized with make_set. @seealso make_set
#' @param base One of the following strings \itemize{\item "id"; \item "quad"; \item "cube"; \item "sqrt"; \item "log"; \item "abs"}
#'             The data gets then expanded. @seealso basis_exp
#' @param omega A penalizer matrix used for the classification. Note that the dimensions must fit the dimension of the
#'              (potentially) expanded dataset
#' @return Returns a list with the name of the created PDA function in the given set in the first entry and the actual classification
#' function in the second entry and also saves it in the input set.
#' @examples
#' test <- make_testset()
#' func_name <- PDA(test,base='quad')[['name']]
#' @export
PDA <-
  function(set, base = "id", omega) {
    ##The PDA classification function. A function factory
    if (!is.data_set(set)) {
      stop("Input must be of class 'data_set' (?make_set)")
    }
    data_exp <- set$expansion(base)
    d <- dim(data_exp)[2]
    if (missing(omega)) {
      ##check for omega
      omega <- diag(0, nrow = d, ncol = d) # set 0
    }
    if(!is.matrix(omega) | ncol(omega) != nrow(omega) | ncol(omega) != d){
      stop(paste("Omega must be a quadratic matrix of size", d))
    }
    if (length(set$func) > 0) {
      ##check if already calculated
      slot <- character(0)
      sapply(set$func_info, function(lis) {
        l <- lis[['parameter']]
        if (!is.null(l[["base"]]) && !is.null(l[["omega"]])) {
          if (l[["base"]] == base && l[["omega"]] == omega) {
            slot <<- lis[["name"]]
          }
        }
      })
      if (length(slot) > 0) {
        return(list(name = slot, func = set$func[[slot]]))
      }
    }
    data_exp <- set$expansion(base)     ##expand data if needed
    h <- basis_exp(base)                ##get expansion function
    d <- dim(data_exp)[2]
    if (missing(omega)) {
      ##check for omega
      omega <- diag(0, nrow = d, ncol = d) # set 0
    }
    G <-
      set$classnames                            ##Vector containing all unique classes
    K <-
      set$n_classes                             ##Number of unique classes
    p <-
      log(unlist(set$pi))                       ##Probability of one class occuring
    mu <-
      mu_exp(data_exp, set)                    ##List of class centroid for each class
    
    sigma_list <-
      lapply(G, function(class) {
        ##Calculating expanded Covariances
        sigma_class_exp(data_exp[set$results == set$classes[class], ], mu[[class]])
      })
    
    Matrix <-
      lapply(sigma_list, function(x)
        solve(x + omega)) ##Adding the Omega matrix (penalizer) to every class covariance matrix and getting the inverse
    names(Matrix) <- set$classnames
    delta <-
      function(x) {
        ##The distance function. The same as QDA but with a penalized distance function and with the expanded data.
        result <- sapply(G, function(class) {
          diff <- h(x) - mu[[class]]
          return(-1 / 2 * log(det(Matrix[[class]])) - 1 / 2 * t(diff) %*% Matrix[[class]] %*% (diff))
        }) + p
        return(result)
      }
    
    classify_func <-
      classify(set$classes, delta) #from numbers to classes
    
    return(set$set_function(classify_func, type = "PDA", list(
      base = base,
      dim = d,
      omega = omega
    )))
  }
#'SVM
#'
#' The SVM classification function as described in Hastie et al. "The Elements Statistical Learning" (2009)
#'
#' @param set A R6 data_set object initialized with make_set. @seealso make_set
#' @param C A positive double used to decide how large the margin should be, hence the sensitivity of the
#'          SVM function to misclassification. Large values encourage an overfit wiggly boundary, while a
#'          small value of C causes a smoother boundary
#' @param kernel One of the following strings \itemize{\item "id"; \item "poly"; \item "radial"; \item "neural"}
#'             The feature space gets enlarged using basis expansions such as polynomials('poly') or
#'             Neural networks('neural').The kernel functions are:
#'             dth-Degree polynomial: K(x,x') = (1+ <x,x'>)^d
#'                      Radial basis: K(x,x') = exp(-g ||x - x'||^2)
#'                    Neural network: K(x,x') = tanh(d* <x,x'> + g)
#'            Use "id" for no kernel.
#' @param d A positive double used in dth-Degree polynomial and Neural network kernel. See parameter 'kernel'
#' @param g A positive double used in Radial basis and Neural network kernel. See parameter 'kernel'
#' @return Returns a list with the name of the created SVM function in the given set in the first entry and the actual classification
#' function in the second entry and saves the classification function in the R6 object.
#' @examples
#' test <- make_testset()
#' func_name <- SVM(test,C = 1, kernel = 'radial', g = 1)[['name']]
#' @export
SVM <- function(set,
                C = 1,
                kernel = "id",
                d = 1,
                g = 1) {
  # The SVM classification function. A function factory
  # Test input:
  if (!is.data_set(set)) {
    stop("Input must be of class 'data_set' (?make_set)", call. = FALSE)
  }
  if ((!is.double(C) && C <= 0 && length(C) != 1) || is.na(C)) {
    stop("Input 'C' must be a positive double", call. = FALSE)
  }
  if (!is.character(kernel) && length(kernel) != 1) {
    stop("Input 'kernel' must be a string with length one.")
  }
  if ((!is.double(d) && d <= 0 && length(d) != 1) || is.na(d)) {
    stop("Input 'd' must be a positive double", call. = FALSE)
  }
  if ((!is.double(g) && g <= 0 && length(d) != 1) || is.na(g)) {
    stop("Input 'g' must be a positive double", call. = FALSE)
  }
  # Test if SVM-classification-function was already calculated using the same parameters.
  if (length(set$func) > 0) {
    slot <- character(0)
    sapply(set$func_info, function(lis) {
      l <- lis[['parameter']]
      if (!is.null(l[["C"]]) &&
          !is.null(l[["kernel"]]) &&
          !is.null(l[["d"]]) &&
          !is.null(l[["g"]])) {
        if (l[["C"]] ==  C &&
            isTRUE(all.equal(l[["kernel"]], kernel)) &&
            l[["d"]] == d &&
            l[["g"]] == g) {
          slot <<- lis[["name"]]
        }
      }
    })
    if (length(slot) > 0) {
      # If it was already calculated, return the function saved in the data_set.
      return(list(name = slot, func = set$func[[slot]]))
    }
  }
  # Save the parameters in a list for easier handling.
  values <- list(
    "C" = C,
    "kernel" = kernel,
    "d" = d,
    "g" = g
  )
  # Calculate list of decision-functions.
  t <- svm_classify_list(set, values)
  # Calculate classification-function.
  f <- svm_classify(t, set$classes)
  # return function and save it in the data_set.
  return(set$set_function(
    f,
    type = "SVM",
    parameter = list(
      base = 'id',
      dim = NULL,
      omega = NULL,
      C = C,
      kernel = kernel,
      d = d,
      g = g
    )
  ))
}

#'RDA
#'
#' The regular Discriminant analysis from 4.3.1 as described in Hastie et al. "The Elements Statistical Learning" (2009)
#'
#' @param set A R6 data_set object initialized with make_set. @seealso make_set
#' @param alpha alpha from formula in Hastie between 0 and 1
#' @param gamma gamma from formula between 0 and 1
#' @return Returns a list with the name of the created RDA function in the given set in the first entry and the actual classification
#' function in the second entry and saves the classification function in the R6 object.
#' @examples
#' test <- make_testset()
#' func_name <- RDA(test, alpha = 0, gamma = 1)[['name']]
#' @export
RDA <- function(set, alpha, gamma) {
  if (!is.data_set(set)) {
    stop("Input must be of class 'data_set' (?make_set)")
  }
  #alpha and gamma in between 0 and 1
  if(!(is.numeric(alpha) && is.numeric(gamma))){

    stop("alpha and gamma must be numeric")
  }else if(!((alpha <= 1) && (gamma <= 1) && (alpha >= 0) && (gamma >= 0))){
    stop("alpha and gamma must be between 0 and 1")
  }
  
  if (length(set$func) > 0) {
    slot <- character(0)
    sapply(set$func_info, function(lis) {
      
      l <- lis[['parameter']]
      if (!is.null(l[["alpha"]]) && !is.null(l[["gamma"]])) {
        if (l[["alpha"]] == alpha && l[["gamma"]] == gamma) {
          slot <<- lis[["name"]]
        }
      }
      
    })
    
    if (length(slot) > 0) { 
      return(list(name = slot, func = set$func[[slot]]))
    }
  }

 
  
  G <- set$classes
  K <- set$n_classes
  p <- log(unlist(set$pi))
  N <- set$n_obs
  mu <- set$mean
  
  # if (missing(alpha) || missing(gamma)) { 
  #   return(RDA_crossFit(set))
  # }
  
  kleinesSigma <- small_sigma_est(set)
  sigma_est <- sigma_est(set)
  n <- ncol(sigma_est)
  allSigmas <- set$sigma
  
  if (any(sapply(data_set$data, anyNA))) {
    warning("data_set$dat contains Na (RDA)")
    
  }
  sigmaAlphaGamma <- lapply(
    allSigmas,
    FUN = function(sigma_class) {
      #browser()
      sigma_estGamma <-
        sigma_est * gamma + (1 - gamma) * diag(n) * (kleinesSigma ** 2)
      sigma_classAlphaGamma <-
        sigma_class * alpha + (1 - alpha) * sigma_estGamma

      return(sigma_classAlphaGamma)
    }
  )
  
  detSigma <- lapply(sigmaAlphaGamma, det)

  if(0 %in% detSigma){
    #Singularities may occur
    return(null)
  }
  
  sigma_inv <- lapply(sigmaAlphaGamma, function(X) {
    out <- tryCatch({
      inverse <- solve(X)
      return(inverse)
    },
    error = function(cond) {
      #Singularities may occurwarnings
      return(diag(n))# TODO
    })
    out
  })
  
  delta <- function(x) {
    result <- sapply(1:K, function(k) {
      -1 / 2 * log(detSigma[[k]]) - 1 / 2 * t(x - mu[[k]]) %*% sigma_inv[[k]] %*% (x - mu[[k]])
    }) + p
    return(result)
  }
  
  classify_func <- classify(set$classes, delta)
  
  return(set$set_function(
    classify_func,
    type = "RDA",
    parameter = list(
      base = 'id',
      description =
        "basic RDA function",
      alpha = alpha,
      gamma = gamma
    )
  ))
}

#'RDA_crossFit
#'
#' The regular Discriminant analysis from 4.3.1 as described in Hastie et al. "The Elements Statistical Learning" (2009)
#' Uses cross validation to determin alpha and gamma @seealso alpha_gamma_crossFit
#' Note that number of Observations per class must be a lot higher than numberOfValidations to avoid errors  
#'
#' @param set A R6 data_set object initialized with make_set. @seealso make_set
#' @param numberOfValidations how many validations shall be conducted. Note that though K = 10 is common, it is impractical for RDA
#'@param accuracyOfParameters how many parameters shall be evaluated. Note that Omega(crossFit) = N^2
#' @return Returns a list with the name of the created RDA function in the given set in the first entry and the actual classification
#' function in the second entry and saves the classification function in the R6 object R6.
#' @examples
#' test <- make_testset()
#' func_name <- RDA_crossFit(test, numberOfValidations = 3, accuracyOfParameters = 5)
#' @export
RDA_crossFit <- function(set, numberOfValidations = 3, accuracyOfParameters = 5) {
  
  alpha_gamma <-
    alpha_gamma_crossFit(set, N = accuracyOfParameters, K = numberOfValidations)
  alpha <- alpha_gamma$alpha
  gamma <- alpha_gamma$gamma

  print(paste("classifying with RDA, gamma =", gamma, "and alpha =", alpha))
  #print(alpha_gamma)
  
  result <- RDA(set, alpha = alpha, gamma = gamma)
  return(result)
}
# 
# test_RDA2 <- function() {
#   #attributes of each test
#   nobservations <- 10 #number of observations per class
#   nclass <- 3 #number of classes
#   dimParameters <- 2 #number of parameters
# 
#   test_data <-
#     make_testset(N = nobservations, K = nclass, P = dimParameters)
# 
#   RDA_crossFit(test_data)
# 
#   print(results)
# }

