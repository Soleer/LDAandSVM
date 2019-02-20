## Create Test

#' make_test
#'
#' Generates a Dataframe with ninputs many columns and a 'class' column. Every Class has
#' uncorrelated parameters around a random center within the given cube
#' boundaries. The Output can be used to create a data_set.
#' @param ninputs Number of generated Observations per Class
#' @param nparam Number of generated parameter columns 
#' @param nclasses Number of Classes to create.
#' @param simga Numeric Vector of sigma Values for the gaussian distribution togenerate the Observations. Will be recycled if shorter than nclasses.
#' @param cube Vector of length 2 with lower and upper boundaries for the parameter centers
#' @return A Dataframe with nparam many columns with gaussian data and a columns 'class'
#' @examples
#' test <- make_test()
#' set <- make_set(test, by = class, title="test")
#' 
#' make_test(ninputs = 80, nparam = 4, nclasses = 4, sigma = c(1,2,0.8,1.5), cube = c(-10,10))
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
