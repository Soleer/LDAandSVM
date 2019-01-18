## Create Test

#' make_test
#'
#' Generates a Dataframe with three cloumns 'x','y','class'.
#' Every Class has a normal distribution around a random centers between the given x and y Boundaries.
#' The Result can be used to train classification methods to sort a 2D Vectors to Class of 'class'.
#' @param ninputs Number of generated Observations per Class
#' @param nclasses Number of Classes.
#' @param simga Numeric Vector of sigma Values for the gaussian Distribution to generate the Observations. Will be recycled if shorter than nclass.
#' @param x Vector of length 2 with Boundaries for x
#' @param y Vector of length 2 with Boundaries for y
#' @return A Dataframe
#' @examples
#' make_test(10)
#' make_test(80,5,sigma=c(1,2,0.8,1.5),x=c(-10,10),y=c(-10,10)))
make_test <- function(ninputs = 100,
                      nparam = 2,
                      nclasses = 2,
                      sigma = 0.8,
                      cube = c(-5, 5)) {
  nsigma <- length(sigma)
  center <- sapply(1:nparam, function(p) {
    sample(cube[1]:cube[2], nclasses, replace = TRUE)
  })
  data <- lapply(0:(nclasses - 1), function(i) {
    class <- LETTERS[i + 1]
    inputs <- sapply(1:nparam, function(p) {
      return(rnorm(ninputs, center[i + 1, p], sigma[i %% nsigma + 1]))
    })
    inputs <- data.frame(inputs, rep(class, times = ninputs))
    return(inputs)
  })
  result <- data[[1]]
  sapply(2:nclasses, function(r) {
    result <<- rbind(result, data[[r]])
  })
  colnames(result) <- c(letters[1:nparam], 'class')
  return(result)
}
