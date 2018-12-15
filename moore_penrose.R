#(i)
lambda_plus <- function(x) {
  sapply(x, function(y) {
    if (all.equal(y, 0) != TRUE)
      return(1 / y)
    return(0)
  })
}

#(ii)
moore_penrose <- function(mat) {
  res <- eigen(mat, symmetric = TRUE)
  diagplus <- diag(lambda_plus(res$values))
  u <- as.matrix(res$vectors)
  return(t(u) %*% diagplus %*% u)
}