library(MASS)
library(NlcOptim)
library(e1071)
library(ggplot2)
library(gridExtra)
#library("Test.R")
#library("plot_functions.R")

#LD############################################
LD_function <- function(data,results){
  f <- function(a){
    g <- 0
    for (i in 1:length(results)) {
      for (j in 1:length(results)) {
        g <- g + a[i]*a[j]*results[i]*results[j]*(sum(data[i,]*data[j,]))
      }
    }
    return(-(sum(a) - 0.5*g))
  }
  return(f)
}
#condition###########################################
con_fun <- function(results){
  con <- function(x){
    f=NULL
    f=rbind(f,sum(x*results))
    return(list(ceq=f,c=NULL))
  }
  return(con)
}
#parameters##########################################
alpha_est <- function(data,results,C){
  LD <- LD_function(data,results)
  con <- con_fun(results)
  #mu <- mu_est(data,results)
  #abc <- calc_C(mu,results)
  #x <- rep(abc,times=nrow(data))
  x <- rep(1,times=nrow(data))
  llb <- rep(0,times=nrow(data))
  uub <- rep(C,times=nrow(data))
  a <- solnl(X=x, objfun =LD,confun=con,lb = llb,ub = uub)
  issero <- sapply(1:nrow(data), function(i){isTRUE(all.equal(a$par[i],0))})
  a$par[issero] <- 0
  return(a$par)
}
beta_est <- function(alpha, data, results) {
  h <- alpha * data * results
  sapply(1:ncol(data), function(i){sum(h[,i])})
}
beta_0 <- function(alpha,data,results,beta){
  data <- data[alpha != 0,]
  results <- results[alpha != 0]
  alpha <- alpha[alpha != 0]
  be <- function(i,data,results,beta){
    return(results[i]-as.double(data[i,])%*%beta)
  }
  s <- sapply(1:length(results), be,data=data,results=results,beta=beta)
  return(mean(s))
}
#estimator####################################################
targets <- function(data, results,C) {
  classes <- unique(results)
  res <- sapply(results, function(class) {
    if (class == classes[1]) {
      return(1)
    }
    return(-1)
  })
  results <- res
  alpha <- alpha_est(data,results,C)
  beta <- beta_est(alpha, data, results)
  beta_Null <- beta_0(alpha,data, results, beta)
  f <- function(x) {
    return(x %*% beta + beta_Null)
  }
  return(f)
}

svm_decision <- function(t,uresults){
  f <- function(x){
    if(t(x)>=0){
      return(uresults[1])
    }
    return(uresults[2])
  }
  return(f)
}


#Test################################################
#Vergleiche mit SVM aus Paket e1071
test <- make_test(nclasses = 2,ninputs = 5000)
data <- test[,1:2]
results <- test[,3]
data_self <- rbind(test[1:25,1:2],test[5001:5025,1:2])
results_self <- c(as.character(test[1:25,3]),as.character(test[5001:5025,3]))
data_lib <- rbind(test[1:200,1:2],test[5001:5200,1:2])
results_lib <- c(as.double(test[1:200,3]),as.character(test[5001:5200,3]))
results_lib[results_lib=="B"] <- -1
results_lib <- as.double(results_lib)
t <- targets(data_self,results_self,1)
f <- svm_decision(t,c('A','B'))
f_lib <- svm(x=data_lib,y=results_lib,cost=1)

#da meine alpha schätzfunktion sehr rechenaufwendig ist,
#kann mein svm-schätzer bis jetzt auf höchstens 50 
#Observationen aufbauen. 
test_svms <- function(f,f_lib,data){
  pred_lib <- sign(predict(f_lib,data))
  pred <- sapply(1:10000,function(i){return(f(as.double(data[i,])))}) 
  pred[pred == "A"] <- 1
  pred[pred == "B"] <- -1
  pred <- as.double(pred)
  right <- c(rep(1,times=5000),rep(-1,times=5000))
  test_it <- function(i,tr,right){
    if(tr[i] == right[i]){
      return(1)
    } else return(0)
  }
  right_pred_f <- sum(sapply(1:10000, test_it, tr=pred,right=right))
  right_pred_f_lib <- sum(sapply(1:10000, test_it, tr=pred_lib,right=right))
  return(c(right_pred_f/10000,right_pred_f_lib/10000))
}
test_svms(f,f_lib,data)


#Test_Ende##########################################
####################################################
#plot##############################################

liste <- plot_error(data, results, f)
p1 <- do.call(grid.arrange, liste)
testplot <-
  make_2D_plot(data,
               results,
               f,
               ppu = 5,
               bg=FALSE)
f2 <- classify(unique(results), QDA(data, results))
liste1 <- plot_error(data, results, f2)
p2 <- do.call(grid.arrange, liste1)
testplot1 <-
  make_2D_plot(data,
               results,
               f2,
               ppu = 5)
plotlist <- list(p1, testplot)
plotlist2 <- list(p2, testplot1)
nice <- do.call("grid.arrange", c(plotlist, ncol = 2, top = "LDA"))
ggsave('LDA.png',
       plot = nice,
       device = 'png',
       dpi = 400)
nice2 <-
  do.call("grid.arrange", c(plotlist2, ncol = 2, top = "QDA"))
ggsave('QDA.png',
       plot = nice2,
       device = 'png',
       dpi = 400)
