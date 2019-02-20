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
    D[i,] %*% D[i,]
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
#' function in the second entry and saves it in the input set.
#' @examples
#' LDA(Rockets_set)
#' func_name <- LDA(SAC_G1)[['name']]
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
      return(list(name=slot,func=set$func[[slot]]))
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
  return(set$set_function(classify_func, type = "LDA", list(base='id',description =
                                                              "basic LDA function")))
}


#' QDA
#'
#' The QDA classification function as described in Hastie et al. "The Elements Statistical Learning" (2009)
#' 
#' @param set A R6 data_set object initialized with make_set. @seealso make_set
#' @return Returns a list with the name of the created QDA function in the given set in the first entry and the actual classification
#' function in the second entry and saves it in the input set.
#' @examples
#' QDA(Rockets_set)
#' func_name <- QDA(SAC_G1)[['name']]
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
      return(list(name=slot,func=set$func[[slot]]))
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
  return(set$set_function(classify_func, type = "QDA", list(base='id',description =
                                                              "basic QDA function")))
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
#' function in the second entry and saves it in the input set.
#' @examples
#' PDA(Rockets_set, "quad", diag(2, nrow = 5))
#' func_name <- PDA(SAC_G1)[['name']]
PDA <- function(set, base = "id", omega) {                             ##The PDA classification function. A function factory
    if (!is.data_set(set)) {
      stop("Input must be of class 'data_set' (?make_set)")
    }
    data_exp <- set$expansion(base)
    d <- dim(data_exp)[2]
    if (missing(omega)) {                                       ##check for omega
      omega <- diag(0, nrow = d, ncol = d) # set 0
    }
    if (length(set$func) > 0) {                                 ##check if already calculated
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
        return(list(name=slot,func=set$func[[slot]]))
      }
    }
    data_exp <- set$expansion(base)     ##expand data if needed
    h <- basis_exp(base)                ##get expansion function
    d <- dim(data_exp)[2]
    if (missing(omega)) {               ##check for omega
      omega <- diag(0, nrow = d, ncol = d) # set 0
    }
    G <- set$classnames                            ##Vector containing all unique classes
    K <- set$n_classes                             ##Number of unique classes
    p <- log(unlist(set$pi))                       ##Probability of one class occuring
    mu <- mu_exp(data_exp, set)                    ##List of class centroid for each class
    
    sigma_list <- lapply(G, function(class) {      ##Calculating expanded Covariances
        sigma_class_exp(data_exp[set$results == set$classes[class],], mu[[class]])
      })
    
    Matrix <- lapply(sigma_list, function(x) solve(x + omega)) ##Adding the Omega matrix (penalizer) to every class covariance matrix and getting the inverse
    names(Matrix) <- set$classnames
    delta <- function(x) {                                     ##The distance function. The same as QDA but with a penalized distance function and with the expanded data.
        result <- sapply(G, function(class) {
          diff <- h(x) - mu[[class]] 
          return(- 1 / 2 * log(det(Matrix[[class]])) - 1 / 2 * t(diff) %*% Matrix[[class]] %*% (diff))
        }) + p
        return(result)
    }
    
    classify_func <- classify(set$classes, delta) #from numbers to classes
    
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
#' @param d A positive double used in dth-Degree polynomial and Neural network kernel. See parameter 'kernel'
#' @param g A positive double used in Radial basis and Neural network kernel. See parameter 'kernel'
#' @return Returns a list with the name of the created SVM function in the given set in the first entry and the actual classification
#' function in the second entry and saves the classification function in the R6 object R6.
#' @examples
#' SVM(Rockets_set, 1,"radial, g = 1)
#' func_name <- SVM(SAC_G1)[['name']]

SVM <- function(set,
                C = 1,
                kernel = "id",
                d = 1,
                g = 1) {
  ##The SVM classification function. A function factory
  if (!is.data_set(set)) {
    stop("Input must be of class 'data_set' (?make_set)", call. = FALSE)
  }
  if (!is.double(C) && C <= 0) {
    stop("Input 'C' must be a positive double", call. = FALSE)
  }  
  if (!is.character(kernel) && length(kernel) != 1) { 
    stop("Input 'kernel' must be a string with length one.")
  }  
  if (!is.double(d) && d <= 0) {
    stop("Input 'd' must be a positive double", call. = FALSE)
  }
  if (!is.double(g) && g <= 0) {
    stop("Input 'g' must be a positive double", call. = FALSE)
  }
  if (length(set$func) > 0) {
    slot <- character(0)
    sapply(set$func_info, function(lis) {
      l <- lis[['parameter']]
      if (!is.null(l[["C"]]) &&
          !is.null(l[["kernel"]]) &&
          !is.null(l[["d"]]) &&
          !is.null(l[["g"]])) {
        if (l[["C"]] ==  C &&
            isTRUE(all.equal(l[["kernel"]],kernel)) &&
            l[["d"]] == d &&
            l[["g"]] == g) {
          slot <<- lis[["name"]]
        }
      }
    })
    if (length(slot) > 0) {
      return(list(name = slot, func = set$func[[slot]]))
    }
  }
  values <- list(
    "C" = C,
    "kernel" = kernel,
    "d" = d,
    "g" = g
  )
  t <- svm_classify_list(set, values)
  f <- svm_classify(t, set$classes)
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

#RDA
RDA <- function(set, alpha, gamma){
  if (!is.data_set(set)) {
    stop("Input must be of class 'data_set' (?make_set)")
  }
  
  if (length(set$func) > 0) { 
    slot <- character(0)
    sapply(set$func_info, function(l) {
      if (!is.null(l[["type"]])) {
        if (l[["type"]] == "RDA") {
          slot <<- l[["name"]]
        }
      }
    })
    if (length(slot) > 0) {
      return(list(name=slot,func=set$func[[slot]]))
    }
  }
  G <- set$classes
  K <- set$n_classes
  p <- log(unlist(set$pi))
  N <- set$n_obs
  mu <- set$mean
  
  if(missing(alpha) | missing(gamma)){
    
    alpha_gamma <- alpha_gamma_crossFit(set) #TODO perhaps identical copy of set
    alpha <<- alpha_gamma$alpha
    gamma <<- alpha_gamma$gamma
  }

  kleinesSigma <- small_sigma_est(set)
  sigma_est <- sigma_est(set)
  n <- ncol(sigma_est)

  sigmaAlphaGamma <- lapply(set$sigma, FUN = function(sigma_class){


    sigma_estGamma <-  sigma_est * gamma + (1 - gamma) * diag(n)* (kleinesSigma**2)
    
    sigma_classAlphaGamma <-
      sigma_class*alpha + (1 - alpha) * sigma_estGamma
    
  })
  
  sigma_inv <- lapply(sigmaAlphaGamma, solve) # TODO immer invertierbar?
  
  delta <- function(x) {
    result <- sapply(1:K, function(k) {
      - 1 / 2 * log(det(sigmaAlphaGamma[[k]])) - 1 / 2 * t(x - mu[[k]]) %*% sigma_inv[[k]] %*% (x - mu[[k]])
    }) + p

    print(result)
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



