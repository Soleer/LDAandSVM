## Basis expansion

basis_exp <- function(type){
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
