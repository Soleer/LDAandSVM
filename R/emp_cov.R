emp_cov <- function(x, y) {
  Ex <- sum(x) / length(x)
  Ey <- sum(y) / length(y)
  return(((x - Ex) %*% (y - Ey)) / length(x))
}