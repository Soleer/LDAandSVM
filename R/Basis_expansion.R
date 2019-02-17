## Basis expansion

basis_exp <- function(type = "id"){    ##Function factory that returns function that makes a basis expansion
  if(type == "id"){             ##No extra basis expansion.
    return(id <- function(x){
      return(x)
    })
  }
  if(type == "quad"){           ##Quadratic Polynomial: For colums x and y it calculates x*x, x*y, y*y
    return(quad <- function(x){
      if(is.vector(x)){         ##If the given input is a vector the claculations are the same as with a data-frame but some steps are not taken
        len <- length(x)        ##Takes length of the vector for later use
        for(i in 1:len){        ##First iteration of the double loop. 
          for(j in i:len){      ##Second iteration if the double loop. Starts only at whatever position the first loop as in to not calculate twice
            y <- x[i] * x[j]
            x <- c(x, y)  ##Multiply the i-th column with the j-th one to get the product of the two and appending it to the already existing vector
          }
        }
      }
      if(is.data.frame(x)){     ##If the input is a data-frame the new colums get names depending on the old ones
        cols <- ncol(x)         ##Saving colnumber of data-frame
        name <- names(x)        ##Saving name sof data-frame
        for(i in 1:cols){       ##First iteration of the double loop
          for(j in i:cols){     ##Second iteration if the double loop. Starts only at whatever position the first loop as in to not calculate twice
            y <- x[, i] * x[, j]
            x <- cbind(x, y) ##Multiplying the i-th column with the j-th one to get the product of the two and appending it to the already existing data-frame
            name <- c(name, paste(name[i], "*" , name[j])) ##creating a new name from the names of the coriginal olums
            colnames(x) <- name ##Setting the names of the new data-frame
          }
        }
      }
      return(x)
    })
  }
  if(type == "cube"){           ##Cubic Polynomial: For colums x, y it calculates x*x, x*y, y*y, x*x*x, x*x*y, x*y*y, y*y*y
    return(cube <- function(x) {
      quad <- basis_exp("quad") ##First calcualtes a qudratic expansion of the data
      quad_x <- quad(x)
      if (is.vector(x)) {       ##Again different proceedures for vectors and data-frames
        len <- length(x)
        for (i in 1:len) {      ##This time having 3 iterations for the powers of 3
          for (j in i:len) {
            for (k in j:len) {
              x <- c(x, x[i] * x[j] * x[k])
            }
          }
        }
        x <- c(quad_x, x[(len+1) : length(x)])  ##Appending the third degree polynomials to the already existing second degree polynomials calculated at the beginning
      }
      if (is.data.frame(x)) {   ##For data frames the same calculations are done as with vectors but the names are set too
        len <- ncol(x)
        name <- names(x)
        for (i in 1:len) {
          for (j in i:len) {
            for (k in j:len) {
              x <- cbind(x, x[, i] * x[, j] * x[, k])
              name <-
                c(name, paste(name[i], "*" , name[j], "*", name[k]))  ##Setting the names for the new columns
              colnames(x) <- name
            }
          }
        }
        x <- cbind(quad_x, x[, (len+1) : ncol(x)])  ##Appending the third degree polynomials to the already existing second degree polynomials calculated at the beginning
      }
      return(x)
    })
  }
  if(type == "log"){    ##Logarithm: Given columns x, y it calculates log(x), log(y)
    return(function(x){
      if(min(x) <= 0){   ##Stops if not all values of the are larger than 0
        stop("All values of x must larger than zero for sqrt expansion")
      }
      if(is.vector(x)){
        expa <- sapply(x, log)  ##Applying log to every value of the vector
        return(c(x, expa))      ##Appending the new columns to the old one
      }
      if(is.data.frame(x)){
        expa <- lapply(x, log)  ##Appying log to every column of the data-frame
        return(cbind(x, expa))  ##Appending the new columns to the old one
      }
    })
  }
  if(type == "sqrt"){   ##Square-root: For columns x, y it calculates sqrt(x), sqrt(y)
    return(function(x){
      if(min(x) < 0){   ##Stops if not all values of the input are larger or equal to 0
        stop("All values of x must larger than zero for sqrt expansion")
      }
      if(is.vector(x)){
        expa <- sapply(x, sqrt) ##Applying sqrt to every value of the vector
        return(c(x, expa))      ##Appending the new columns to the old one
      }
      if(is.data.frame(x)){
        expa <- lapply(x, sqrt) ##Applying sqrt to every value of the data-frame
        return(cbind(x, expa))  ##Appending the new columns to the old one
      }
    })
  }
  if(type == "abs"){    ##Absolute: For columns x, y it calculates abs(x) = |x|, abs(y) = |y|
    return(function(x){ 
      if(is.vector(x)){
        expa <- sapply(x, abs)  ##Applying abs to every value of the vector
        return(c(x, expa))      ##Appending the new columns to the old one
      }
      if(is.data.frame(x)){
        expa <- lapply(x, abs)  ##Applying abs to every value of the data-frame
        return(cbind(x, expa))  ##Appending the new columns to the old one
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