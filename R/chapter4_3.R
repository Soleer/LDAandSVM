library(ggplot2)
set.seed(0)

#Chapter 4.3


# Gauß test data 4 classes same sigma:
test <-
  data.frame(
    x = rnorm(100, 0, 0.6),
    y = rnorm(100, 0, 0.6),
    class = rep('A', 100)
  )
test1 <- data.frame(
  x = rnorm(100, 0, 0.6),
  y = rnorm(100,-2, 0.6),
  class = rep('B', 100)
)
test2 <-
  data.frame(
    x = rnorm(100, 0, 0.6),
    y = rnorm(100, 2, 0.6),
    class = rep('C', 100)
  )
test3 <-
  data.frame(
    x = rnorm(100, 2, 0.6),
    y = rnorm(100, 2, 0.6),
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
    colMeans(data[results == class,])
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
  Bn <- diag(0,ncol=n,nrow=n)
  sapply(1:K, function(k) {
    apply(data[results == G[k], ], 1, function(x) {
      Bn <<- Bn + ((x - mu[k, ]) %*% t(x - mu[k, ]))
    })
  })
  return(Bn/(N-K))
}
sigma_est(test[c(1,2)],test$class)
#LDA formular in one vector
LDA <- function(data, results) {
  G <- unique(results)
  K <- length(G)
  p <- log(pi_est(results))
  mu <- mu_est(data, results)
  sigma <- solve(sigma_est(data, results))
  print('SIGMA')
  print(sigma)
  delta <- function(x) {
    result <- sapply(1:K, function(k) {
      (x%*% sigma %*% mu[k, ] - 1 / 2 * mu[k, ]%*% sigma %*% mu[k, ])
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
    return(classes[which.max(f(x))])
  }
  return(classfunction)
}
f <- LDAclass(test[c('x', 'y')], test$class)

#plot background
plot <- data.frame(x=rep(seq(-5,5,length.out = 30),time=30),y=c(sapply(seq(-5,5,length.out = 30),function(x) rep(x,times=30))))
plot$class <- apply(plot,1,f)
testplot <- testplot+geom_jitter(data = plot, aes(x, y, color = class),shape=3, height=0 , width=0) 
testplot



#seperating lines
f <- LDA(test[c('x', 'y')], test$class)
#getting the numbers
create_id <- function(a,b,rf){
  getzero <- function(x) {
    rf(x)[a] - rf(x)[b]
  }
  return(getzero)
}

get_Y_Value <- function(z,ploty){
  upper=max(ploty)
  lower=min(ploty)
  if(z(lower) * z(upper) <= 0){
    return(uniroot(z,ploty)$root)
  }
  else{
    return(NA)
  }
  
}
getseperatorfun <- function(a, b, rf, ploty = c(-5, 5)) {
  getzero <- create_id(a, b, rf)
  sep <- function(x) {
    z <- function(y) {
      getzero(c(x, y))
    }
    result <-get_Y_Value(z,ploty)
    return(result)
  }
  return(sep)
}
h <- getseperatorfun(3,4,f)
h_test <- h(1)
test <- getseperatorfun(1,2,f)
sepdata <- data.frame(x=c(seq(-5,0,length.out = 30),seq(0,5,length.out = 30)))
sepdata$y <- apply(sepdata,1,test)
testplot+ geom_path(data = sepdata, aes(x = x, y = y)) 
