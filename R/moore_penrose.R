#(i)
#' lambda_plus
#' 
#' @param vec A Vector
#' @return the Lambda-Plus vector of the given Vector
#' @examples
#' lambda_plus(c(1,4,2))
#' lambda_plus(c(1, 0, 4, 0))
lambda_plus <- function(vec) {
  sapply(x, function(y) {
    if (all.equal(y, 0) != TRUE)
      return(1 / y)
    return(0)
  })
}

#(ii)
#' moore_penrose
#'
#' @param mat A symmetric, positiv semidefinte Matrix
#' @return The Moore-Penrose Inverse of the given Matrix
#' @example
#' A <- matrix(c(1, 0, 0, 2), ncol = 2)
#' moore_penrose(A)
moore_penrose <- function(mat) {
  res <- eigen(mat, symmetric = TRUE)
  diagplus <- diag(lambda_plus(res$values))
  u <- as.matrix(res$vectors)
  return(t(u) %*% diagplus %*% u)
}