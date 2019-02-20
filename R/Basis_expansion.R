## Basis expansion

#' basis_exp
#'
#' A function for expanding data-frames or vectors in aspecific way. Used in @seealso PDA to expand the data into a higher
#' dimensional space
#' @param type One of the following strings: \itemize{\item "id"; \item "quad"; \item "cube"; \item "sqrt"; \item "log"; \item "abs"}
#' @return A function that expands a vector or data-frame by the specified type
#' @examples
#' h <- basis_exp("quad")
#' h(c(2, 3))
#' @export
basis_exp <- function(type = "id") { ##Function factory that returns function that makes a basis expansion
  stopifnot((typeof(type) == "character"))              ##Checking if the input is valid
  pos <- c("id", "quad", "cube", "sqrt", "log", "abs")
  if(!any(pos == type)){
    stop(paste(type, "is not a valid basis expansion"))
  }
  
  if (type == "id") { ##No extra basis expansion.
    return(id <- function(x) {
      return(x)
    })
  }
  
  if (type == "quad") { ##Quadratic Polynomial: For colums x and y it calculates x*x, x*y, y*y
    return(quad <- function(x) {
      if (is.vector(x)) { ##If the given input is a vector the claculations are the same as with a data-frame but some steps are not taken
        len <- length(x)        ##Takes length of the vector for later use
        for (i in 1:len) { ##First iteration of the double loop.
          for (j in i:len) { ##Second iteration if the double loop. Starts only at whatever position the first loop as in to not calculate twice
            x <- c(x, x[i] * x[j])  ##Multiply the i-th column with the j-th one to get the product of the two and appending it to the already existing vector
          }
        }
      }
      if (is.data.frame(x)) { ##If the input is a data-frame the new colums get names depending on the old ones
        cols <- ncol(x)         ##Saving colnumber of data-frame
        name <- names(x)        ##Saving name sof data-frame
        for (i in 1:cols) { ##First iteration of the double loop
          for (j in i:cols) { ##Second iteration if the double loop. Starts only at whatever position the first loop as in to not calculate twice
            x <- cbind(x, x[, i] * x[, j]) ##Multiplying the i-th column with the j-th one to get the product of the two and appending it to the already existing data-frame
            name <- c(name, paste(name[i], "*" , name[j])) ##creating a new name from the names of the coriginal olums
            colnames(x) <- name ##Setting the names of the new data-frame
          }
        }
      }
      return(x)
    })
  }
  
  if (type == "cube") { ##Cubic Polynomial: For colums x, y it calculates x*x, x*y, y*y, x*x*x, x*x*y, x*y*y, y*y*y
    return(cube <- function(x) {
      quad <- basis_exp("quad") ##First calcualtes a qudratic expansion of the data
      quad_x <- quad(x)
      if (is.vector(x)) { ##Again different proceedures for vectors and data-frames
        len <- length(x)
        for (i in 1:len) { ##This time having 3 iterations for the powers of 3
          for (j in i:len) {
            for (k in j:len) {
              x <- c(x, x[i] * x[j] * x[k])
            }
          }
        }
        x <- c(quad_x, x[(len + 1):length(x)])  ##Appending the third degree polynomials to the already existing second degree polynomials calculated at the beginning
      }
      if (is.data.frame(x)) { ##For data frames the same calculations are done as with vectors but the names are set too
        len <- ncol(x)
        name <- names(x)
        for (i in 1:len) {
          for (j in i:len) {
            for (k in j:len) {
              x <- cbind(x, x[, i] * x[, j] * x[, k])
              name <- c(name, paste(name[i], "*" , name[j], "*", name[k]))  ##Setting the names for the new columns
              colnames(x) <- name
            }
          }
        }
        x <- cbind(quad_x, x[, (len + 1):ncol(x)])  ##Appending the third degree polynomials to the already existing second degree polynomials calculated at the beginning
      }
      return(x)
    })
  }
  
  if (type == "log") { ##Logarithm: Given columns x, y it calculates log(x), log(y)
    return(function(x) {
      if (min(x) <= 0) { ##Stops if not all values of the are larger than 0
        stop("All values of x must larger than zero for sqrt expansion")
      }
      if (is.vector(x)) { expa <- sapply(x, log)  ##Applying log to every value of the vector
        return(c(x, expa))      ##Appending the new columns to the old one
      }
      if (is.data.frame(x)) {
        expa <- lapply(x, log)  ##Appying log to every column of the data-frame
        return(cbind(x, expa))  ##Appending the new columns to the old one
      }
    })
  }
  
  if (type == "sqrt") { ##Square-root: For columns x, y it calculates sqrt(x), sqrt(y)
    return(function(x) {
      if (min(x) < 0) { ##Stops if not all values of the input are larger or equal to 0
        stop("All values of x must larger than zero for sqrt expansion")
      }
      if (is.vector(x)) {
        expa <- sapply(x, sqrt) ##Applying sqrt to every value of the vector
        return(c(x, expa))      ##Appending the new columns to the old one
      }
      if (is.data.frame(x)) {
        expa <- lapply(x, sqrt) ##Applying sqrt to every value of the data-frame
        return(cbind(x, expa))  ##Appending the new columns to the old one
      }
    })
  }
  
  if (type == "abs") { ##Absolute: For columns x, y it calculates abs(x) = |x|, abs(y) = |y|
    return(function(x) {
      if (is.vector(x)) {
        expa <- sapply(x, abs)  ##Applying abs to every value of the vector
        return(c(x, expa))      ##Appending the new columns to the old one
        }
      if (is.data.frame(x)) {
        expa <- lapply(x, abs)  ##Applying abs to every value of the data-frame
        return(cbind(x, expa))  ##Appending the new columns to the old one
      }
    })
  }
}
