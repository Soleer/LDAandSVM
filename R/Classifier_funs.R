# classification functions -> G
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
    return(classes[which.max(delta(x))])
  }
  return(classfunction)
}


## LDA, QDA, PDA

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


PDA <- function(set, base, omega) {                             ##The PDA classification function. A function factory
    if (!is.data_set(set)) {
      stop("Input must be of class 'data_set' (?make_set)")
    }
    data_exp <- set$expansion(base)
    d <- dim(data_exp)[2]
    if (missing(omega)) {                                       ##check for omega
      omega <- diag(0, nrow = d, ncol = d) # set 0
    }
    if (length(set$func) > 0) {
      slot <- character(0)
      sapply(set$func_info, function(l) {
        if (!is.null(l[["parameter"]][["base"]]) && !is.null(l[["parameter"]][["omega"]])) {
          if (l[["parameter"]][["base"]] == base && l[["parameter"]][["omega"]] == omega) {
            slot <<- l[["name"]]
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
          diff <- h(x) - mu[[class]] - 1 / 2 * log(det(Matrix[[class]])) - 1 / 2 * t(diff) %*% Matrix[[class]] %*% (diff)
        }) + p
        return(result)
    }
    
    classify_func <- classify(set$classes, delta)
    
    return(set$set_function(classify_func, type = "PDA", list(
      base = base,
      dim = d,
      omega = omega
    )))
}