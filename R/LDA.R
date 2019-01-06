library(ggplot2)

#testdatensatz Gauß 2D
set.seed(0)
test <-
  data.frame(
    x = rnorm(100, 2, 0.8),
    y = rnorm(100, 2, 0.8),
    class = rep('red', 100)
  )
test2 <-
  data.frame(
    x = rnorm(100, 0, 0.8),
    y = rnorm(100, 0, 0.8),
    class = rep('blue', 100)
  )
test <- rbind(test, test2)
testplot <- ggplot()
testplot <-
  testplot + geom_jitter(data = test, aes(x, y, color = class))
testplot
#vapkin hyperplane (optimale trennung durch eine Hyperfläche)
#Chapter 4.2
#Ergebnisse in Response Matrix umwandeln
getY <- function(results) {
  level <- unique(test$class)
  Y <- sapply(results, function(class) {
    sapply(level, function(l) {
      if (l == class) {
        return(1)
      }
      return(0)
    })
  })
  return(t(as.matrix(Y)))
}
#B fitten
getB <- function(data, results) {
  X <- as.matrix(data)
  X <- cbind(1, X)
  Y <- getY(results)
  B <- solve(t(X) %*% X) %*% t(X) %*% Y
  return(B)
}
#linear-regression-model
linearmodel <- function(data, results) {
  B <- getB(data, results)
  f <- function(x) {
    return(c(1, x) %*% B)
  }
  return(f)
}

#plot stuff
b <- getB(test[c('x', 'y')], test$class)
seperator2D <- function(x) {
  diff <- b[, 1] - b[, 2]
  res <- (diff[1] + diff[2] * x) / (-diff[3])
  return(res)
}
sep <- data.frame(x = -1:4, y = Vectorize(seperator2D)(-1:4))
sepplot <- geom_path(data = sep, aes(x = x, y = y))
testplot <- testplot + sepplot
testplot

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
#classifizierungsfunktion erstellen
classfunc <- function(data, results) {
  classes <- unique(results)
  f <- linearmodel(data, results)
  classfunction <- function(x) {
    regression <- f(x)
    values <- targets(regression)
    return(classes[which.min(values)])
  }
  return(classfunction)
}
#test
f <- classfunc(test[c('x', 'y')], test$class)

