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
    colMeans(data[results == class, ])
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
    apply(data[results == G[k],], 1, function(x) {
      Bn <<- Bn + ((x - mu[k,]) %*% t(x - mu[k,]))
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
    D[i, ] %*% D[i, ]
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

## LDA, QDA, PDA

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

QDA <- function(data, results) {
  G <- unique(results)
  K <- length(G)
  p <- log(pi_est(results))
  print(p)
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

LDA_exp <- function(data, results){
  data_exp <- h(data)
  G <- unique(results)
  K <- length(G)
  p <- log(pi_est(results))
  mu <- mu_est(data, results)
  sigma <- solve(sigma_est(data_exp, results))
  delta <- function(x) {
    result <- sapply(1:K, function(k) {
      (h(x) %*% sigma %*% h(mu[k, ]) - 1 / 2 * h(mu[k, ]) %*% sigma %*% h(mu[k, ]))
    }) + p
    return(result)
  }
  return(delta)
}

QDA_exp <- function(data, results) {
  data_exp <- h(data)
  G <- unique(results)
  K <- length(G)
  p <- log(pi_est(results))
  print(p)
  mu <- mu_est(data, results)
  sigma_list <- lapply(1:K, function(k) {
    sigma_class(data_exp[results==G[k],],mu[k])
  })
  sigma_inv <- lapply(sigma_list,solve)
  delta <- function(x) {
    result <- sapply(1:K, function(k) {
      -1 / 2 * log(det(sigma_list[[k]])) - 1 / 2 * t(h(x) - h(mu[k, ])) %*% sigma_inv[[k]] %*% (h(x) - h(mu[k, ]))
    }) + p
    return(result)
  }
  return(delta)
}

PDA <- function(data, results, base) {
  h <- basis_exp(base)
  data_exp <- h(data)
  G <- unique(results)
  K <- length(G)
  p <- log(pi_est(results))
  mu <- mu_est(data, results)
  sigma_list <- lapply(1:K, function(k) {
    sigma_class(data_exp[results==G[k],],mu[k])
  })
  Matrix <- lapply(sigma_list, function(x) solve(x + diag(0, nrow=nrow(x), ncol=ncol(x))))
  
  delta <- function(x) {
    result <- sapply(1:K, function(k) {
      -1 / 2 * log(det(Matrix[[k]])) - 1 / 2 * t(as.vector(h(x) - h(mu[k, ]))) %*% Matrix[[k]] %*% as.vector((h(x) - h(mu[k, ])))
      #- (t(as.vector(h(x) - h(mu[k, ]))) %*% Matrix[[k]] %*% (as.vector(h(x) - h(mu[k, ])))) # Minus the function so that max is the searched value
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
                         classfun,
                         ppu = 10,
                         bg = TRUE) {
  #prama
  uresults <- unique(results)
  n <- length(uresults)
  proj <- make_projection(data)
  proj_to <- proj[[1]]
  proj_in <- proj[[2]]
  proj_data <- as.data.frame(t(apply(data, 1, proj_to)))
  x <- c(as.integer(min(proj_data[, 1])), as.integer(max(proj_data[, 1])+1))
  y <- c(as.integer(min(proj_data[, 2])), as.integer(max(proj_data[, 2])+1))
  xtimes <- (x[2] - x[1]) * ppu
  ytimes <- (y[2] - y[1]) * ppu
  d <- dim(data)[2]
  #prepare plot data
  #input
  input_data <-
    data.frame(x = proj_data[, 1], y = proj_data[, 2], Legend = results)
  #make mainplot
  #1. limit
  mainplot <- ggplot() + xlim(x[1], x[2]) + ylim(y[1], y[2])
  #2. input data
  mainplot <-
    mainplot + geom_jitter(
      data = input_data,
      aes(x = x, y = y, color = Legend),
      shape = 20,
      height = 0,
      width = 0
    )
  #3. colored background
  if (bg == TRUE) {
    background <-
      data.frame(x = rep(seq(x[1], x[2], length.out = xtimes), times = ytimes),
                 y = c(sapply(seq(y[1], y[2], length.out = ytimes), rep, times = xtimes)))
    proj_background <-
      as.data.frame(t(apply(background, 1, proj_in)))
    background$class <- apply(proj_background, 1, classfun)
    mainplot <- mainplot + geom_jitter(
      data = background,
      aes(x, y, color = class),
      shape = 3,
      height = 0,
      width = 0
    )
  }
  
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
    
    probs_Data[paste0(class, 'l')] <-
      scales::percent(probs_Data[, class])
    colsum <- 0
    probs_Data[paste0(class, 'yl')] <-
      sapply(probs_Data[, class], function(x) {
        colsum <<- colsum + x
        colsum - x / 2
      })
    left <- ggplot(data = probs_Data[1:n, ]) +
      geom_bar(
        aes_string(
          x = paste0(class, 'l'),
          y = class,
          fill = 'class'
        ),
        stat = "identity",
        width = 1
      ) + theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()
      ) +
      labs(title = paste0('f(x=', class, ')'))
    
    probs_Results[paste0(class, 'l')] <-
      scales::percent(probs_Results[, class])
    colsum <- 0
    probs_Results[paste0(class, 'yl')] <-
      sapply(probs_Results[, class], function(x) {
        colsum <<- colsum + x
        colsum - x / 2
      })
    right <- ggplot(data = probs_Results[1:n, ]) +
      geom_bar(
        aes_string(
          y = class,
          x = paste0(class, 'l'),
          fill = 'class'
        ),
        stat = "identity",
        width = 1
      ) + theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()
      ) +
      labs(title = paste0('f^-1(', class, ')'))
    return(grid.arrange(left, right, nrow = 1))
  })
  
  mistake_lable <- c(paste0(miss*100,'%',' wrong'),paste0((1-miss)*100,'%',' right'))
  mistake <- data.frame(i=mistake_lable,per=c(miss,1-miss))
  mi <- ggplot(data = mistake) +
    geom_bar(
      aes(
        y = per,
        x = i,
        fill = i
      ),
      stat = "identity",
      width = 1
    ) + theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "none",
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    )+ggtitle('Data')
  charts[[n+1]] <- mi
  return(charts)
}

##Analyse
sig <- c(1,1.5,2,2.5,1.3,1.1,2.1,1.8)
test <- make_test(100,
                  nparam = 4,
                  nclasses = 8,
                  sigma = sig)

### LDA
f <- classify(unique(test$class), PDA(test[1:4], test$class, base = "quad"))
liste <- plot_error(test[1:4], test$class, f)
p1 <- do.call(grid.arrange, liste)
testplot <-
  make_2D_plot(test[1:4],
               test$class,
               f,
               ppu = 5)
plotlist <- list(p1, testplot)
nice <- do.call("grid.arrange", c(plotlist, ncol = 2, top = "PDA"))

ggsave('PDA.png',
       plot = nice,
       device = 'png',
       dpi = 400)


### LDA
f2 <- classify(unique(test$class), LDA(test[1:4], test$class))
liste2 <- plot_error(test[1:4], test$class, f2)
p2 <- do.call(grid.arrange, liste2)
testplot2 <-
  make_2D_plot(test[1:4],
               test$class,
               f2,
               ppu = 5)
plotlist2 <- list(p2, testplot2)

nice2 <-
  do.call("grid.arrange", c(plotlist2, ncol = 2, top = "LDA"))
ggsave('LDA.png',
       plot = nice2,
       device = 'png',
       dpi = 400)


### QDA
f3 <- classify(unique(test$class), QDA(test[1:4], test$class))
liste3 <- plot_error(test[1:4], test$class, f3)
p3 <- do.call(grid.arrange, liste3)
testplot3 <-
  make_2D_plot(test[1:4],
               test$class,
               f3,
               ppu = 5)
plotlist3 <- list(p3, testplot3)

nice3 <-
  do.call("grid.arrange", c(plotlist3, ncol = 2, top = "QDA"))
ggsave('QDA.png',
       plot = nice3,
       device = 'png',
       dpi = 400)
