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
  print(p)
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


PDA <- function(data, results, base) {
  h <- basis_exp(base)
  data_exp <- h(data)
  G <- unique(results)
  K <- length(G)
  p <- log(pi_est(results))
  mu <- mu_est(data, results)
  sigma_list <- lapply(1:K, function(k) {
    sigma_class(data_exp[results==G[k],],mu[k])
  })
  Matrix <- lapply(sigma_list, function(x) solve(x + diag(0, nrow=nrow(x), ncol=ncol(x))))
  
  delta <- function(x) {
    result <- sapply(1:K, function(k) {
      -1 / 2 * log(det(Matrix[[k]])) - 1 / 2 * t(as.vector(h(x) - h(mu[k, ]))) %*% Matrix[[k]] %*% as.vector((h(x) - h(mu[k, ])))
    }) + p
    return(result)
  }
  return(delta)
}
