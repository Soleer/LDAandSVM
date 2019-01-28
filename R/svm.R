library(MASS)
library(NlcOptim)
library(e1071)
library(ggplot2)
library(gridExtra)
source("R/Basis_expansion.R")
source('R/Test.R')
source("R/Estimators.R")
source("R/Classifier_funs.R")
source("R/plot_functions.R")
#LD############################################
LD_function <- function(data,results){
  cache <- matrix(0,ncol = length(results),nrow=length(results))
  for (i in 1:length(results)) {
    for (j in 1:i) {
      cache[i,j] <- results[i]*results[j]*(sum(data[i,]*data[j,]))
    }
  }
  f <- function(a){
    g <- 0
    for (i in 1:length(results)) {
      for (j in 1:i) {
        g <- g + a[i]*a[j]*cache[i,j]
      }
    }
    return(-(sum(a) - g))
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
alpha_svm_est <- function(data,results,C){
  LD <- LD_function(data,results)
  con <- con_fun(results)
  x <- rep(1,times=nrow(data))
  llb <- rep(0,times=nrow(data))
  uub <- rep(C,times=nrow(data))
  a <- solnl(X=x, objfun =LD,confun=con,lb = llb,ub = uub)
  issero <- sapply(1:nrow(data), function(i){isTRUE(all.equal(a$par[i],0))})
  a$par[issero] <- 0
  return(a$par)
}
beta_svm_est <- function(alpha, data, results) {
  h <- alpha * data * results
  sapply(1:ncol(data), function(i){sum(h[,i])})
}
beta_svm_0 <- function(alpha,data,results,beta){
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
targets_j <- function(data, results,C=1,kernel=0, d=1,j=1) {
  classes <- unique(results)
  res <- sapply(results, function(class) {
    if (class == classes[j]) {
      return(1)
    }
    return(-1)
  })
  results <- res
  alpha <- alpha_svm_est(data,results,C)
  beta <- beta_svm_est(alpha, data, results)
  beta_Null <- beta_svm_0(alpha,data, results, beta)
  if(kernel==0){
    f <- function(x) {
      return(x %*% beta + beta_Null)
    }
  }else if(kernel == 1){
    f <- function(x){
      h <- 0
      for (i in 1:nrow(data)) {
        h <- h+ alpha[i]*results[i](1+x%*%data[i,])^d
      }
      h <- h + beta_0
    }
  }else{
    warning("Wrong Parameter kernel! No Kernel used.",immediate. = TRUE)
    f <- function(x) {
      return(x %*% beta + beta_Null)
    }
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
#more_classes###################################
targets_multiple_classes <- function(data,results,C=1,kernel=0,d=1){
  classes <- unique(results)
  if(length(classes)==2){return(targets_j(data,results,C,kernel,d=1))}
  fun_list <- function(r,classes,data,results,C,kernel,d){
    temp <- list()
    for (s in (r+1):length(classes)) {
      dat <- rbind(data[r*100-(99:0),],data[s*100-(99:0),])
      res <- c(results[r*100-(99:0)],results[s*100-(99:0)])
      temp[[s]] <- targets_j(data=dat,results=res,C=C,kernel=kernel,d=d)
    }
    return(temp)
  }
  b <- lapply(1:(length(classes)-1),fun_list,classes=classes,data=data,results=results,C=C,kernel=kernel,d=d)
  return(b)
}

svm_decision_more_classes <- function(t,uresults){
  f <- function(x){
    cla <- 1
    tr <- FALSE
    for (r in 1:(length(uresults)-1)) {
      for (s in (r+1):(length(uresults))) {
        a <- t[[r]][[s]](x)
        if(s==length(uresults)&&a>0){
          cla <- r
          tr <- TRUE
          break
        }
        else if(s==length(uresults)&&a<0){cla <- r+1}
        if(a<0)break
      }
      if(tr==TRUE)break
    }
    return(uresults[cla])
  }
  return(f)
}


#Test################################################
#Vergleiche mit SVM aus Paket e1071
test <- make_test(nclasses = 6,ninputs = 5000)
data <- test[,1:2]
results <- test[,3]
data_self <- rbind(test[1:100,1:2],test[5001:5100,1:2],test[10001:10100,1:2],test[15001:15100,1:2],test[20001:20100,1:2],test[25001:25100,1:2])
results_self <- c(as.character(test[1:100,3]),as.character(test[5001:5100,3]),as.character(test[10001:10100,3]),as.character(test[15001:15100,3]),as.character(test[20001:20100,3]),as.character(test[25001:25100,3]))
results_self
nrow(data_self)
data_lib <- rbind(test[1:200,1:2],test[5001:5200,1:2])
results_lib <- c(as.double(test[1:200,3]),as.character(test[5001:5200,3]))
results_lib[results_lib=="B"] <- -1
results_lib <- as.double(results_lib)
t <- targets_multiple_classes(data = data_self,results = results_self,kernel = 4)
f <- svm_decision_more_classes(t,c("A","B","C","D","E","F"))
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
test_svm <- function(f,data){
  pred <- sapply(1:30000,function(i){return(f(as.double(data[i,])))}) 
  count <- 0
  for (i in 1:30000) {
    if(pred[i]==results[i]){
      count <- count + 1
    }
  }
  return(count)
  right_pred_f <- sum(sapply(1:10000, test_it, tr=pred,right=right))
  right_pred_f_lib <- sum(sapply(1:10000, test_it, tr=pred_lib,right=right))
  return(c(right_pred_f/10000,right_pred_f_lib/10000))
}
test_svm(f,data)/30000


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
