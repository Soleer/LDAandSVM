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

LDA <- function(set) {
  if(is.data.set(set)){
    stop("Input must be of class 'Dataset' (?make_set)")
  }
  if(is.null(set$func['LDA'])){
  G <- set$classes
  K <- set$n_classe
  p <- log(unlist(set$pi))
  mu <- set$mean
  sigma <- solve(sigma_est(set))
  delta <- function(x) {
    result <- sapply(1:K, function(k) {
      (x %*% sigma %*% mu[[k]] - 1 / 2 * mu[[k]] %*% sigma %*% mu[[k]])
    }) + p
    return(result)
  }
  set$set_function(delta,"LDA",list(name="LDA",  description="basic LDA function"))
  return(delta)
  }
  else{
    set$func[['LDA']]
  }
}

QDA <- function(set) {
  if(is.data.set(set)){
    stop("Input must be of class 'Dataset' (?make_set)")
  }
  if(is.null(set$func['QDA'])){
    G <- set$classes
    K <- set$n_classes
    p <- log(unlist(set$pi))
    mu <- set$mean
    sigma_inv <- lapply(set$sigma,solve)
    delta <- function(x) {
      result <- sapply(1:K, function(k) {
        -1 / 2 * log(det(sigma_list[[k]])) - 1 / 2 * t(x - mu[[k]]) %*% sigma_inv[[k]] %*% (x - mu[[k]])
      }) + p
      return(result)
    }
    set$set_function(delta,"QDA",list(name="QDA", description="basic QDA function"))
    return(delta)
  }
  else{
    set$func[['QDA']]
  }
}


PDA <- function(set, base, omega) { ##The PDA classification function. A function factory
  if(missing(omega)){
    omega <- diag(0, nrow=nrow(x), ncol=ncol(x))
  }
  if(is.null(set$func['PDA'])==FALSE){
    info_list <- set$func_info['PDA']
    find <- sapply(info_list,function(l){
      l["base"]==base && l["omega"]==omega
    })
  if(any(find)){
    name <- names(info_list)[find][[1]]
    return(set$func(name))
  }
  }
  h <- basis_exp(base)                            ##Gets the basis expansion function
  data_exp <- h(set$data)                             ##Expands the data via the expansion function
  G <- set$classes                            ##Vector containing all unique classes
  K <- set$n_classes                                  ##Number of unique classes
  p <- log(unlist(set$pi))                       ##Probability of one class occuring
  mu <- set$mean                    ##Vector of class centroid for each class
  sigma_list <- set$sigma
  Matrix <- lapply(sigma_list, function(x) solve(x + omega))  ##Adding the Omega matrix (penalizer) to every class covariance matrix and getting the inverse 
  delta <- function(x) {                          ##The distance function. The same as QDA but with a penalized distance function and with the expanded data.
    result <- sapply(1:K, function(k) {
      value <- h(x)
      m <- h(mu[[k]])
      -1 / 2 * log(det(Matrix[[k]])) - 1 / 2 * t(value - m) %*% Matrix[[k]] %*% (value - m)
    }) + p
    return(result)
  }
  set$set_function(delta,"PDA",list(name=Sys.time(),base=base,omega=omega))
  return(delta)
}
