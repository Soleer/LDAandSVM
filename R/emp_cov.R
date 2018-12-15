#' emp_cov
#' 
#' @param x A numeric Vector
#' @param y A numeric Vector of same length as x
#' @return The empirical Covariance of x and y
#' @examples 
#' emp_cov(c(1, 2, 3), c(1, 2, 3))
#' emp_cov(c(1, 2, 3), c(3, 2, 1))
emp_cov <- function(x, y) {
  Ex <- sum(x) / length(x)
  Ey <- sum(y) / length(y)
  return(((x - Ex) %*% (y - Ey)) / length(x))
}

