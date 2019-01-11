library(ggplot2)
set.seed(0)
rep('A', 10)
#Chapter 4.3
# Gauß test data n classes same sigma:

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
                      nclasses = 2,
                      sigma = 0.8,
                      x = c(-5, 5),
                      y = c(-5, 5)
                      ) {
  nsigma <- length(sigma)
  xcoord <- sample(x[1]:x[2], nclasses, replace = TRUE)
  ycoord <- sample(y[1]:y[2], nclasses, replace = TRUE)
  print(1)
  test <-
    data.frame(
      x = rnorm(ninputs, xcoord[1], sigma[1]),
      y = rnorm(ninputs, ycoord[1], sigma[1]),
      class = rep('A', times = ninputs)
    )
  sapply(1:(nclasses-1), function(i) {
    class <- LETTERS[i]
    test <<-
      rbind(test, data.frame(
        x = rnorm(ninputs, xcoord[i], sigma[i%%nsigma+1]),
        y = rnorm(ninputs, ycoord[i], sigma[i%%nsigma+1]),
        class = rep(class, times = ninputs)
      ))
  })
  return(test)
}
#estimators of chapter 4.3

pi_est <- function(results) {
  classes <- unique(results)
  K <- length(results)
  obs <- table(results)
  #return possibilitys in order of classes
  return(sapply(classes, function(class)
    obs[as.character(x)] / K))
}
#'mue_est
#'
#'given a dataframe with Parameters of Observations and a second dataframe with the corresponding Classes 
#'mu_es returns a Matrix with the mean vectors of the classes as rows.
#'
#'@param data Dataframe of Parameters for all Observations
#'@param results Vector of corresponding Classes to the Data
#'@return A Matrix with the mean vectors of the classes as rows
mu_est <- function(data, results) {
  data <- as.data.frame(data)
  classes <- unique(results)
  t <- table(results)
  mu <- sapply(classes, function(class) {
    colMeans(data[results == class,])
  })
  mu <- t(mu)
  return(mu)
}
#'sigma_class
#'
#'given a dataframe with parameters of Observations of one Class sigma_class returns the
#'covariance matrix of the Data.
#'
#'@param data Dataframe of Parameters for all Observations
#'@return The covariance matrix of the Data 
sigma_class <- function(data, mu= colMeans(data)){
  n <- dim(data)[2]
  Bn <- diag(0, ncol = n, nrow = n)
  apply(data, 1, function(x) {
    Bn <<- Bn + ((x - mu) %*% t(x - mu))
  })
  return(Bn/(dim(data)[1]-1))
}

sigma_est <- function(data, results) {
  G <- unique(results)
  K <- length(G)
  N <- length(results)
  mu <- mu_est(data, results)
  n <- dim(data)[2]
  Bn <- diag(0, ncol = n, nrow = n)
  sapply(1:K, function(k) {
    apply(data[results == G[k], ], 1, function(x) {
      Bn <<- Bn + ((x - mu[k, ]) %*% t(x - mu[k, ]))
    })
  })
  return(Bn / (N - K))
}
#LDA formular in one vector
LDA <- function(data, results) {
  G <- unique(results)
  K <- length(G)
  p <- log(pi_est(results))
  mu <- mu_est(data, results)
  sigma <- solve(sigma_est(data, results))
  delta <- function(x) {
    result <- sapply(1:K, function(k) {
      (x %*% sigma %*% mu[k, ] - 1 / 2 * mu[k, ] %*% sigma %*% mu[k, ])
    }) + p
    return(result)
  }
  return(delta)
}

#LDA classification function -> G
targets <- function(vector) {
  n <- length(vector)
  En <- diag(1, n, n)
  V <- matrix(vector,
              nrow = n,
              ncol = n,
              byrow = TRUE)
  D <- En - V
  results <- sapply(1:n, function(i) {
    D[i,] %*% D[i,]
  })
  return(results)
}

classify <- function(uresults, f) {
  classfunction <- function(x) {
    return(uresults[which.min(targets(f(x)))])
  }
  return(classfunction)
}

classifyTargets <- function(uresults, f) {
  classfunction <- function(x) {
    return(uresults[which.max(f(x))])
  }
  return(classfunction)
}
#seperating lines

create_id <- function(a, b, rf) {
  getzero <- function(x) {
    rf(x)[a] - rf(x)[b]
  }
  return(getzero)
}

get_Y_Value <- function(z, ploty) {
  upper = max(ploty)
  lower = min(ploty)
  if (z(lower) * z(upper) <= 0) {
    return(uniroot(z, ploty)$root)
  }
  else{
    return(NA)
  }
}

getseperatorfun <- function(a,
                            b,
                            rf,
                            y = c(-5, 5),
                            inv = FALSE) {
  getzero <- create_id(a, b, rf)
  sep <- function(x) {
    inverse <- inv
    z <- function(y) {
      if (inverse == FALSE) {
        getzero(c(x, y))
      }
      else{
        getzero(c(y, x))
      }
    }
    y <- get_Y_Value(z, y)
    if (inverse == FALSE) {
      if (is.na(y) || which.max(rf(c(x, y))) == a ||
          which.max(rf(c(x, y))) == b) {
        return(y)
      }
      return(NA)
    }
    else{
      if (is.na(y) || which.max(rf(c(y, x))) == a ||
          which.max(rf(c(y, x))) == b) {
        return(y)
      }
      return(NA)
    }
  }
  return(sep)
}
#Not finished yet
make_seperator <- function(classf,
                           nclass,
                           x = c(-5, 5),
                           y = c(-5, 5),
                           ppu = 10) {
  xval <-  seq(x[1], x[2], length.out = (x[2]-x[1])*ppu)
  yval <- seq(y[1],y[2], length.out = (y[2]-y[1])*ppu)
  lines <- data.frame()
  sapply(1:(nclass - 1), function(i) {
    sapply((i + 1):nclass, function(j) {
      
      s <- getseperatorfun(i, j, classf, y = y)
      sinv <- getseperatorfun(i, j, classf, y = x, inv=TRUE)
      ony <- FALSE
      lines[] <<- sapply(sepdata$xval, function(x){
        
      })
      })
    })
  return(sepdata)
}


###

make_plot <- function(data,
                      results,
                      type = LDA,
                      y = c(-5, 5),
                      x = c(-5, 5),
                      ppu = 10) {
  #prama
  f <- type(data, results)
  uresults <- unique(results)
  classfun <- classify(uresults, f)
  n <- length(uresults)
  xtimes <- (x[2] - x[1]) * ppu
  ytimes <- (y[2] - y[1]) * ppu
  mainplotdata <- cbind(data, results)
  mainplot <- ggplot() + xlim(x[1],x[2])+ylim(y[1],y[2])
  mainplot <-
    mainplot + geom_jitter(
      data = mainplotdata,
      aes(x, y, color = results),
      shape = 20,height = 0,width = 0
    )
  #background
  background <-
    data.frame(x = rep(seq(x[1], x[2], length.out = xtimes), times = ytimes),
               y = c(sapply(seq(y[1], y[2], length.out = ytimes), function(x)
                 rep(x, times = xtimes))))
  background$class <- apply(background, 1, classfun)
  mainplot <- mainplot + geom_jitter(
    data = background,
    aes(x, y, color = class),
    shape = 3,
    height = 0,width = 0
  )
  #Lines???
  
  
  
  return(mainplot)
}
#QDA

QDA <- function(data, results) {
  G <- unique(results)
  K <- length(G)
  p <- log(pi_est(results))
  mu <- mu_est(data, results)
  sigma_list <- lapply(1:K, function(k) {
    sigma_class(data[results==G[k],],mu[k])
    })
  sigma_inv <- lapply(sigma_list,solve)
  delta <- function(x) {
    result <- sapply(1:K, function(k) {
      -1 / 2 * log(det(sigma_list[[k]])) - 1 / 2 * t(x - mu[k, ]) %*% sigma_inv[[k]] %*% (x - mu[k, ])
    }) + p
    return(result)
  }
  return(delta)
}

#################
#Test


x <- c(-10, 10)
y <- c(-10, 10)
sig <- c(1, 1.5, 2,0.5)
test <- make_test(100, nclasses = 4, sigma = sig )
testplot <-
  make_plot(test[c('x', 'y')], test$class, type =  QDA, x, y, ppu = 5)
testplot1 <-
  make_plot(test[c('x', 'y')], test$class, type =  LDA, x, y, ppu = 5)

grid.arrange(testplot,testplot1,nrow=1)
