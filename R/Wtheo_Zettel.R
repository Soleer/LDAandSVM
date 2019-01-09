#aufgabe38
x <- c(112.0,
       105.2,
       98.1,
       108.7,
       97.2,
       102.3,
       110.1,
       100.5,
       103.3,
       99.0)
mu0 <-100 
sn <-function(x){
  1/length(x)*sum(x-mean(x))
} 
rtest <- function(x){
  n <- length(x)
  if( sqrt(n)*(mean(x)-mu0) >= 1.833*sn(x)){
    return(1) 
  }
  return(0)
}

rtest(x)
