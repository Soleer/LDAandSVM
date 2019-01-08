library(ggplot2)
set.seed(0)

#Chapter 4.3

# Gauß test data n classes same sigma:
make_test <- function(ninputs = 100,
                      nclasses = 2,
                      sigma = 0.6) {
  xcoord <- sample(-4:4, nclasses, replace = TRUE)
  ycoord <- sample(-4:4, nclasses, replace = TRUE)
  test <-
    data.frame(
      x = rnorm(ninputs, xcoord[1], sigma),
      y = rnorm(ninputs, ycoord[1], sigma),
      class = rep('A', ninputs)
    )
  sapply(2:nclasses, function(i) {
    class <- LETTERS[i]
    test <<-
      rbind(test, data.frame(
        x = rnorm(ninputs, xcoord[i], sigma),
        y = rnorm(ninputs, ycoord[i], sigma),
        class = rep(class, ninputs)
      ))
  })
  return(test)
}

#estimators of chapter 4.3

pi_est <- function(results) {
  n <- length(results)
  vec <- table(results)
  return(sapply(vec, function(x)
    x / n))
}

mu_est <- function(data, results) {
  data <- as.data.frame(data)
  classes <- unique(results)
  t <- table(results)
  mu <- sapply(classes, function(class) {
    colMeans(data[results == class, ])
  })
  mu <- t(mu)
  rownames(mu) <- as.character(classes)
  return(mu)
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
#LDA formular in one vector
LDA <- function(data, results) {
  G <- unique(results)
  K <- length(G)
  p <- log(pi_est(results))
  mu <- mu_est(data, results)
  sigma <- solve(sigma_est(data, results))
  delta <- function(x) {
    result <- sapply(1:K, function(k) {
      (x %*% sigma %*% mu[k,] - 1 / 2 * mu[k,] %*% sigma %*% mu[k,])
    }) + p
    return(result)
  }
  return(delta)
}

#LDA classification function -> G
LDAclass <- function(data, results) {
  classes <- unique(results)
  f <- LDA(data, results)
  classfunction <- function(x) {
    return(classes[which.max(f(x))])
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
                            ploty = c(-5, 5),
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
    y <- get_Y_Value(z, ploty)
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

make_seperator <- function(classf,
                          nclass,
                          min = -5,
                          max = 5,
                          acc = 100) {
  sepdata <- data.frame(xval = seq(min, max, length.out = acc))
  iT <- integer(0)
  k <- 1
  sapply(1:(nclass - 1), function(i) {
    sapply((i + 1):nclass, function(j) {
      k <<- k + 1
      s <- getseperatorfun(i, j, classf, ploty = c(min, max))
      sepdata[k] <<- sapply(sepdata$xval, s)
      iT[k] <<- FALSE
      if (length(sepdata[!is.na(sepdata[k]), k]) <= 1) {
        s <- getseperatorfun(i,
                             j,
                             classf,
                             ploty = c(min, max),
                             inv = TRUE)
        sepdata[k] <<- sapply(sepdata$xval, s)
        iT[k] <<- TRUE
      }
    })
  })
  return(list(sepdata, iT))
}

make_plot <- function(data, results) {
  #prama
  f <- LDA(data, results)
  classfun <- LDAclass(data, results)
  n <- length(unique(results))
  #
  mainplotdata <- cbind(data, results)
  mainplot <- ggplot()
  mainplot <-
    mainplot + geom_jitter(
      data = mainplotdata,
      aes(x, y, color = results),
      shape = 20,
      height = 0 ,
      width = 0
    )
  #background
  background <-
    data.frame(x = rep(seq(-5, 5, length.out = 50), time = 50), y = c(sapply(seq(-5, 5, length.out = 50), function(x)
      rep(x, times = 50))))
  background$class <- apply(background, 1, classfun)
  mainplot <- mainplot + geom_jitter(
      data = background,
      aes(x, y, color = class),
      shape = 3,
      height = 0 ,
      width = 0
    )
  #lines
  seps <- make_seperator(f, n)
  sepdata <- seps[[1]]
  iT <- seps[[2]]
  #addlines
  
  sapply(1:choose(n, 2) + 1, function(i) {
    if (any(!is.na(sepdata[paste0('V', i)]))) {
      if (!iT[i]) {
        mainplot <<-
          mainplot + geom_path(data = sepdata,
                               aes_string(x = 'xval', y = paste0('V', i)),
                               na.rm = TRUE)
      }
      else{
        mainplot <<-
          mainplot + geom_path(data = sepdata,
                               aes_string(x = paste0('V', i), y = 'xval'),
                               na.rm = TRUE)
      }
    }
  })
  return(mainplot)
}
test <- make_test(nclasses = 7,sigma = 0.6)
testplot <- make_plot(test[c('x', 'y')], test$class)
testplot

