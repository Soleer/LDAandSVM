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
    D[i, ] %*% D[i, ]
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


## LDA, QDA, PDA

LDA <- function(data, results) {
  G <- unique(results)
  K <- length(G)
  p <- log(pi_est(results))
  mu <- mu_est(data, results)
  sigma <- solve(sigma_est(data, results))
  delta <- function(x) {
    result <- sapply(1:K, function(k) {
      (x %*% sigma %*% mu[k, ] - 1 / 2 * mu[k, ] %*% sigma %*% mu[k, ])
    }) + p
    return(result)
  }
  return(delta)
}

QDA <- function(data, results) {
  G <- unique(results)
  K <- length(G)
  p <- log(pi_est(results))
  mu <- mu_est(data, results)
  sigma_list <- lapply(1:K, function(k) {
    sigma_class(data[results==G[k],],mu[k])
  })
  sigma_inv <- lapply(sigma_list,solve)
  delta <- function(x) {
    result <- sapply(1:K, function(k) {
      -1 / 2 * log(det(sigma_list[[k]])) - 1 / 2 * t(x - mu[k, ]) %*% sigma_inv[[k]] %*% (x - mu[k, ])
    }) + p
    return(result)
  }
  return(delta)
}


PDA <- function(data, results, base) {            ##The PDA classification function. A function factory
  h <- basis_exp(base)                            ##Gets the basis expansion function
  data_exp <- h(data)                             ##Expands the data via the expansion function
  G <- unique(results)                            ##Vector containing all unique classes
  K <- length(G)                                  ##Number of unique classes
  p <- log(pi_est(results))                       ##Probability of one class occuring
  mu <- mu_est(data, results)                     ##Vector of class centroid for each class
  sigma_list <- lapply(1:K, function(k) {         ##Calculating the class specific covariance matrices of the expanded data and writing them to a list
    sigma_class(data_exp[results==G[k],],mu[k])
  })
  Matrix <- lapply(sigma_list, function(x) solve(x + diag(0, nrow=nrow(x), ncol=ncol(x))))  ##Adding the Omega matrix (penalizer) to every class covariance matrix and getting the inverse 
  
  delta <- function(x) {                          ##The distance function. The same as QDA but with a penalized distance function and with the expanded data.
    result <- sapply(1:K, function(k) {
      -1 / 2 * log(det(Matrix[[k]])) - 1 / 2 * t(h(x) - h(mu[k, ])) %*% Matrix[[k]] %*% (h(x) - h(mu[k, ]))
    }) + p
    return(result)
  }
  return(delta)
}
