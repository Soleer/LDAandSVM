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
A <- matrix(c(1, 0, 0, 2), ncol = 2)
moore_penrose(A)
A %*% moore_penrose(A) %*% A
#(iii)
emp_cov <- function(x, y) {
  Ex <- sum(x) / length(x)
  Ey <- sum(y) / length(y)
  return(((x - Ex) %*% (y - Ey)) / length(x))
}

emp_cov(c(1, 2, 3), c(1, 2, 3))
## [1] 0.6666667
emp_cov(c(1, 2, 3), c(3, 2, 1))
## [1] -0.6666667
emp_cov(c(1, 2, 3), c(1, 3, 2))
## [1] 0.3333333

