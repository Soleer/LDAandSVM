library(ggplot2)
set.seed(0)

#Umsetzung von 4.6 (Ergebnisse sind als summe =1 )
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

#Chapter 4.3


# Gauß test data 4 classes same sigma:
test <-
  data.frame(
    x = rnorm(100, 2, 0.8),
    y = rnorm(100, 2, 0.8),
    class = rep('A', 100)
  )
test1 <- data.frame(
  x = rnorm(100, 3, 0.8),
  y = rnorm(100,-1, 0.8),
  class = rep('B', 100)
)
test2 <-
  data.frame(
    x = rnorm(100, 0, 0.8),
    y = rnorm(100, 0, 0.8),
    class = rep('C', 100)
  )
test3 <-
  data.frame(
    x = rnorm(100, 4, 0.8),
    y = rnorm(100, 6, 0.8),
    class = rep('D', 100)
  )
test <- rbind(test, test1, test2,test3)
#Plot it
testplot <- ggplot()
testplot <-
  testplot + geom_jitter(data = test, aes(x, y, color = class), shape=24,height=0 , width=0)
testplot

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
    apply(data[results == class,], 2, sum) / t[as.character(class)]
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
  sigma <- sum(sapply(1:K, function(k) {
    sum(apply(data[results == G[k], ], 1, function(x) {
      sum((x - mu[k, ]) ^ 2) # norm squared
    }))
  })) / (N - K)
  return(sigma)
}
#LDA formular in one vector

LDA <- function(data, results) {
  G <- unique(results)
  K <- length(G)
  p <- log(pi_est(results))
  mu <- mu_est(data, results)
  sigma <- sigma_est(data, results)
  delta <- function(x) {
    result <- sapply(1:K, function(k) {
      1 / sigma * (x %*% mu[k, ] - 1 / 2 * mu[k, ] %*% mu[k, ])
    }) + p
    return(result)
  }
  return(delta)
}
#test
f <- LDA(test[c('x', 'y')], test$class)
f(c(2, 2))

#LDA classification function -> G
LDAclass <- function(data, results) {
  classes <- unique(results)
  f <- LDA(data, results)
  classfunction <- function(x) {
    reg <- f(x)
    values <- targets(reg)
    return(classes[which.min(values)])
  }
  return(classfunction)
}
f <- LDAclass(test[c('x', 'y')], test$class)

#plot background
plot <- data.frame(x=rep(seq(-4,10,length.out = 30),time=30),y=c(sapply(seq(-4,10,length.out = 30),function(x) rep(x,times=30))))
plot$class <- apply(plot,1,f)
testplot <- testplot+geom_jitter(data = plot, aes(x, y, color = class),shape=3, height=0 , width=0) 

#seperating lines

#getting the numbers
targetLDA <-function(data, results) {
  f <- LDA(data, results)
  tf <- function(x) {
    return(targets(f(x)))
  }
  return(tf)
}

f <- targetLDA(test[c('x','y')],test$class)
f(c(1,2))
