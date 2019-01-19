## Basis expansion

basis_exp <- function(type){
  if(type == "id"){
    return(id <- function(x){
      return(x)
    })
  }
  if(type == "quad"){
    return(quad <- function(x){
      if(is.vector(x)){
        len <- length(x)
        for(i in 1:len){
          for(j in i:len){
            y <- x[i] * x[j]
            x <- c(x, y)
          }
        }
      }
      if(is.data.frame(x)){
        cols <- ncol(x)
        name <- names(x)
        for(i in 1:cols){
          for(j in i:cols){
            y <- x[, i] * x[, j]
            name <- c(name, paste(name[i], "*" , name[j]))
            x <- cbind(x, y)
            colnames(x) <- name
          }
        }
      }
      return(x)
    })
  }
  if(type == "cube"){
    return(cube <- function(x) {
      quad <- basis_exp("quad")
      quad_x <- quad(x)
      if (is.vector(x)) {
        len <- length(x)
        for (i in 1:len) {
          for (j in i:len) {
            for (k in j:len) {
              y <- x[i] * x[j] * x[k]
              x <- c(x, y)
            }
          }
        }
        x <- c(quad_x, x[(len+1) : length(x)])
      }
      if (is.data.frame(x)) {
        len <- ncol(x)
        name <- names(x)
        for (i in 1:len) {
          for (j in i:len) {
            for (k in j:len) {
              y <- x[, i] * x[, j] * x[, k]
              name <-
                c(name, paste(name[i], "*" , name[j], "*", name[k]))
              x <- cbind(x, y)
              colnames(x) <- name
            }
          }
        }
        x <- cbind(quad_x, x[, (len+1) : ncol(x)])
      }
      return(x)
    })
  }
  if(type == "log"){
    return(function(x){
      if(min(x) < 0){
        stop("All values of x must larger than zero for sqrt expansion")
      }
      if(is.vector(x)){
        expa <- sapply(x, sqrt)
        return(c(x, expa))
      }
      if(is.data.frame(x)){
        expa <- lapply(x, sqrt)
        return(cbind(x, expa))
      }
    })
  }
  if(type == "sqrt"){
    return(function(x){
      if(min(x) < 0){
        stop("All values of x must larger than zero for sqrt expansion")
      }
      if(is.vector(x)){
        expa <- sapply(x, sqrt)
        return(c(x, expa))
      }
      if(is.data.frame(x)){
        expa <- lapply(x, sqrt)
        return(cbind(x, expa))
      }
    })
  }
  if(type == "abs"){
    return(function(x){
      if(is.vector(x)){
        expa <- sapply(x, abs)
        return(c(x, expa))
      }
      if(is.data.frame(x)){
        expa <- lapply(x, abs)
        return(cbind(x, expa))
      }
    })
  }
}

## Work in progress
decompose_vectors <- function(data, results, base){
  h <- basis_exp(base)
  data_exp <- h(data)
  G <- unique(results)
  N <- length(G)
  mu <- mu_est(data, results)
  
  sigma_list <- lapply(1:N, function(k) {
    sigma_class(data_exp[results==G[k],],mu[k])
  })
  sigma_bet <- sigma_bet_est(data_exp, results)
  
  max_fun <- function(x){
    t(x) %*% sigma_bet %*% x
  }
  
  
  u_list <- lapply(1:N, function(i){
    constraint <- function(x){
      f = NULL
      f = rbind(f, (t(x) %*% sigma_list[[i]] %*% x) - 1)
      return(list(ceq=f, c = NULL))
    }
    start_vec <- as.vector(rep(1, ncol(sigma_bet)))
    
    sol <- solnl(X = start_vec, objfun = max_fun, confun = constraint)
    return(sol[[1]])
  })
  return(u_list)
}