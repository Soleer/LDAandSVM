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

Sn <-function(x){
  1/(length(x)-1)*sum((x-mean(x))^2)
} 

rtest <- function(x){
  n <- length(x)
  if( sqrt(n)*(mean(x)-mu0) >= 1.833*sqrt(sn(x))){
    return(1) 
  }
  return(0)
}

rtest(x)

B <- function(x){
  mu <- mean(x)
  n <- length(x)
  mi <- mu - sqrt(sn(x)/n)*2.262
  ma <- mu + sqrt(sn(x)/n)*2.262
  return(c(mi,ma))
}
B(x)

Barock <- c(39.7,
            47.5,
            37.4,
            46.6,
            40.2,
            48.4,
            39.0,
            37.2)

Renaissance <- c(47.0,
                 39.2,
                 45.5,
                 38.7,
                 43.0,
                 43.1,
                 40.4,
                 45.0,
                 43.4,
                 48.6,
                 45.7,
                 43.8)
mu_beidseitig <- function(x,y){
  if(mean(x)-mean(y)<=-1.734*sqrt(length(x)+length(y))/sqrt(length(x)*length(y))*sn(c(x,y))){
    return(1)
  }
  return(0)
}
mu_beidseitig(Barock,Renaissance)
