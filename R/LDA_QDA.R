library(ggplot2)
library(gridExtra)
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

#estimators of chapter 4.3

pi_est <- function(results) {
  classes <- unique(results)
  K <- length(results)
  obs <- table(results)
  #return possibilitys in order of classes
  vec <- sapply(classes, function(class)
    obs[as.character(class)] / K)
  return(vec)
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
sigma_class <- function(data, mu = colMeans(data)) {
  n <- dim(data)[2]
  Bn <- diag(0, ncol = n, nrow = n)
  apply(data, 1, function(x) {
    Bn <<- Bn + ((x - mu) %*% t(x - mu))
  })
  return(Bn / (dim(data)[1] - 1))
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

maincomponent_analysis <- function(data) {
  cov_matrix <- sigma_class(data)
  ev <- eigen(cov_matrix)
  values <- ev$values
  n <- length(values)
  main_matrix <- ev$vectors
  D <- diag(values, nrow = n, ncol = n)
  return(list(D, main_matrix, cov_matrix))
}

make_projection <- function(data, dim = 2) {
  l <- maincomponent_analysis(data)
  U <- l[[2]][, 1:dim]
  proj <- function(x) {
    t(U) %*% x
  }
  i_proj <- function(x) {
    U %*% x
  }
  return(list(proj, i_proj))
}

# classification functions -> G
#helpfunction
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

#return closest target
class_by_targets <- function(uresults, f) {
  classfunction <- function(x) {
    return(uresults[which.min(targets(f(x)))])
  }
  return(classfunction)
}
#return max
classify <- function(uresults, f) {
  classfunction <- function(x) {
    return(uresults[which.max(f(x))])
  }
  return(classfunction)
}

#seperating lines test not working

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

#Not finished
make_seperator <- function(classf,
                           nclass,
                           x = c(-5, 5),
                           y = c(-5, 5),
                           ppu = 10) {
  xval <-  seq(x[1], x[2], length.out = (x[2] - x[1]) * ppu)
  yval <- seq(y[1], y[2], length.out = (y[2] - y[1]) * ppu)
  lines <- data.frame()
  sapply(1:(nclass - 1), function(i) {
    sapply((i + 1):nclass, function(j) {
      s <- getseperatorfun(i, j, classf, y = y)
      sinv <- getseperatorfun(i, j, classf, y = x, inv = TRUE)
      ony <- FALSE
      lines[] <<- sapply(sepdata$xval, function(x) {
        
      })
    })
  })
  return(sepdata)
}


### LDA,QDA,PDA,FDA,SVM

#LDA
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

#QDA
QDA <- function(data, results) {
  G <- unique(results)
  K <- length(G)
  p <- log(pi_est(results))
  mu <- mu_est(data, results)
  sigma_list <- lapply(1:K, function(k) {
    sigma_class(data[results == G[k],], mu[k])
  })
  sigma_inv <- lapply(sigma_list, solve)
  sigma_log_det <- lapply(sigma_list, function(x) {
    log(det(x))
  })
  delta <- function(x) {
    result <- sapply(1:K, function(k) {
      -1 / 2 * sigma_log_det[[k]] - 1 / 2 * t(x - mu[k, ]) %*% sigma_inv[[k]] %*% (x - mu[k, ])
    }) + p
    return(result)
  }
  return(delta)
}

#################
#Test

#killr 2D plot
make_2D_plot <- function(data,
                         results,
                         type = LDA,
                         ppu = 10) {
  #prama
  f <- type(data, results)
  uresults <- unique(results)
  classfun <- classify(uresults, f)
  n <- length(uresults)
  xtimes <- (x[2] - x[1]) * ppu
  ytimes <- (y[2] - y[1]) * ppu
  proj <- make_projection(data)
  proj_to <- proj[[1]]
  proj_in <- proj[[2]]
  proj_data <- as.data.frame(t(apply(data, 1, proj_to)))
  x <- c(min(proj_data[, 1]), max(proj_data[, 1]))
  y <- c(min(proj_data[, 2]), max(proj_data[, 2]))
  print(proj_data)
  d <- dim(data)[2]
  #prepare plot data
  #input
  input_data <-
    data.frame(x = proj_data[, 1], y = proj_data[, 2], z = results)
  #background
  background <-
    data.frame(x = rep(seq(x[1], x[2], length.out = xtimes), times = ytimes),
               y = c(sapply(
                 seq(y[1], y[2], length.out = ytimes), rep, times = xtimes
               )))
  proj_background <- as.data.frame(t(apply(background, 1, proj_in)))
  background$class <- apply(proj_background, 1, classfun)
  #make mainplot
  #1. limit
  mainplot <- ggplot() + xlim(x[1], x[2]) + ylim(y[1], y[2])
  #2. input data
  mainplot <-
    mainplot + geom_jitter(
      data = input_data,
      aes(x = x, y = y, color = z),
      shape = 20,
      height = 0,
      width = 0
    )
  #3. colored background
  mainplot <- mainplot + geom_jitter(
    data = background,
    aes(x, y, color = class),
    shape = 3,
    height = 0,
    width = 0
  )
  #4.Lines???
  
  return(mainplot)
}
# performance

calc_error <- function(data, results, f) {
  G <- unique(results)
  estimated <- apply(data, 1, f)
  of_Data <- lapply(G, function(class) {
    c <- as.character(class)
    t <- table(estimated[results == class])
    number <- sum(t)
    classresults <- as.list(t[as.character(G)] / number)
    right <- t[c] / number
    wrong <- (1 - right)
    col <- unlist(list(classresults, right, wrong))
    return(col)
  })
  of_Results <- lapply(G, function(class) {
    c <- as.character(class)
    t <- table(results[estimated == class])
    number <- sum(t)
    classresults <- as.list(t[as.character(G)] / number)
    right <- t[c] / number
    wrong <- (1 - right)
    col <- unlist(list(classresults, right, wrong))
    return(col)
  })
  probs_of_Data <-
    data.frame(class = c(as.character(G), 'right', 'wrong'), of_Data)
  probs_of_Results <-
    data.frame(class = c(as.character(G), 'right', 'wrong'), of_Results)
  colnames(probs_of_Data) <- c('class', as.character(G))
  colnames(probs_of_Results) <- c('class', as.character(G))
  miss <-
    sum(probs_of_Data[probs_of_Data$class == 'wrong', 2:length(G)]) / length(G)
  return(list(probs_of_Data, probs_of_Results, miss))
}

plot_error <- function(data, results, f) {
  G <- as.character(unique(results))
  n <- length(G)
  get_list <- calc_error(data, results, f)
  probs_Data <- get_list[[1]]
  probs_Results <- get_list[[2]]
  miss <- get_list[[3]]
  charts <- lapply(G, function(class) {
    probs_Data[paste0(class,'l')] <- scales::percent(probs_Data[,class])
    colsum <- 0
    probs_Data[paste0(class,'yl')] <- sapply(probs_Data[,class], function(x){
      colsum<<-colsum+x
      colsum-x/2
    })
    
    left <- ggplot(data = probs_Data[1:n,]) +
      geom_bar(aes_string(x = '""', y = class, fill = 'class'), stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +geom_text(aes_string(x='1', y = paste0(class,'yl'), label=paste0(class,'l')))+
      ggtitle(paste('x=',class,'is sorted to:'))
    
    probs_Results[paste0(class,'l')] <- scales::percent(probs_Results[,class])
    colsum <- 0
    probs_Results[paste0(class,'yl')] <- sapply(probs_Results[,class], function(x){
      colsum<<-colsum+x
      colsum-x/2
    })
    right <- ggplot(data = probs_Results[1:n,]) +
      geom_bar(aes_string(x = '""', y = class, fill = 'class'), stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +geom_text(aes_string(x='1', y = paste0(class,'yl'), label=paste0(class,'l')))+
      ggtitle(paste('f(x)=',class,'actually is:'))
    return(list(left,right))
  })
  return(charts)
}
a <- 'b'


sig <- c(1, 1.5, 2, 0.5)
test <- make_test(100,
                  nparam = 2,
                  nclasses = 4,
                  sigma = sig)
f <- classify(unique(test$class), LDA(test[1:2], test$class))
calc_error(test[1:2], test$class, f)
liste <- plot_error(test[1:2], test$class, f)
liste[[1]]
testplot <-
  make_2D_plot(test[1:2], test$class, type = LDA, ppu = 5)
testplot1 <-
  make_2D_plot(test[1:2], test$class, type = QDA, ppu = 5)
grid.arrange(testplot, testplot1, nrow = 1)


library(dplyr)
library(ggplot2)
data <-
  data.frame(
    a = c(
      "a1",
      "a1",
      "a2",
      "a3",
      "a1",
      "a2",
      "a3",
      "a4",
      "a2",
      "a1",
      "a5",
      "a4",
      "a3"
    ),
    b = 1:13
  )
data <- data %>%
  group_by(a) %>%
  count() %>%
  ungroup() %>%
  mutate(per = `n` / sum(`n`)) %>%
  arrange(desc(a))
data$label <- scales::percent(data$per)
data
ggplot(data = data) +
  geom_bar(aes_string(x = '""', y = 'per', fill = 'a'), stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void()
