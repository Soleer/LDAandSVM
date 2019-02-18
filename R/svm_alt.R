library(MASS)
library(NlcOptim)
#library(e1071)
#library(ggplot2)
#library(gridExtra)
#source("R/Basis_expansion.R")
#source('R/Test.R')
#source("R/Estimators.R")
#source("R/Classifier_funs.R")
#source("R/plot_functions.R")
#safety#########################################
#This function catches the error-messages of other functions and prints them as warnings.
safety <- function(expr) {
  tryCatch(
    error = function(cnd) {
      list(result = NULL,
           error = cnd,
           warning = cnd)
    },
    warning = function(cnd) {
      list(result = NULL,
           error = cnd,
           warning = cnd)
    },
    list(
      result = expr,
      error = NULL,
      warning = NULL
    )
  )
}
#LD############################################
#This is the function that has to be maximised.
LD_function <- function(data, results, kernel, d, g) {
  cache <- matrix(0, ncol = length(results), nrow = length(results))
  if (kernel == 1) {
    for (i in 1:length(results)) {
      for (j in 1:i) {
        cache[i, j] <- results[i] * results[j] * (1 + sum(data[i, ] * data[j, ])) ^
          d
      }
    }
  } else if (kernel == 2) {
    for (i in 1:length(results)) {
      for (j in 1:i) {
        cache[i, j] <-
          results[i] * results[j] * exp((-g) * sum((data[i, ] - data[j, ])) ^ 2)
      }
    }
  } else{
    for (i in 1:length(results)) {
      for (j in 1:i) {
        cache[i, j] <- results[i] * results[j] * (sum(data[i, ] * data[j, ]))
      }
    }
  }
  f <- function(a) {
    g <- 0
    for (i in 1:length(results)) {
      for (j in 1:i) {
        g <- g + a[i] * a[j] * cache[i, j]
      }
    }
    return(-(sum(a) - g))
  }
  return(f)
}
#condition###########################################
#the conditions under which the LD function has to be maximised
con_fun <- function(results) {
  con <- function(x) {
    f = NULL
    f = rbind(f, sum(x * results))
    return(list(ceq = f, c = NULL))
  }
  return(con)
}
#####################################################
#safe_solnl <- function(x,LD,con,llb,uub,C,data)

#parameters##########################################

alpha_svm_est <- function(data, results, C, kernel, d, g) {
  LD <- LD_function(data, results, kernel, d, g)
  con <- con_fun(results)
  x <- rep(1 / sqrt(nrow(data)), times = nrow(data))
  llb <- rep(0, times = nrow(data))
  uub <- rep(C, times = nrow(data))
  a <- safety(solnl(
    X = x,
    objfun = LD,
    confun = con,
    lb = llb,
    ub = uub
  ))
  if (is.null(a$result) || !is.null(a$warning)) {
    warning(c(a$error, "\n Ändere Startwert zu 0:"))
    x <- rep(0, times = nrow(data))
    a <- safety(solnl(
      X = x,
      objfun = LD,
      confun = con,
      lb = llb,
      ub = uub
    ))
  }
  if (is.null(a$result) || !is.null(a$warning)) {
    warning(c(a$error, "\n Ändere Startwert zu 1/C:"))
    x <- rep(1 / C, times = nrow(data))
    a <- safety(solnl(
      X = x,
      objfun = LD,
      confun = con,
      lb = llb,
      ub = uub
    ))
  }
  if (is.null(a$result) || !is.null(a$warning)) {
    warning(c(a$error, "\n Ändere Startwert zu 0:"))
    x <- rep(0, times = nrow(data))
    a <- safety(solnl(
      X = x,
      objfun = LD,
      confun = con,
      lb = llb,
      ub = uub
    ))
  }
  if ((is.null(a$result) || !is.null(a$warning)) && kernel != 0) {
    warning(
      c(
        a$error,
        "\n Ändere Startwert zurück zu 1/sqrt(nrow(data)) und verwende keinen Kernel:"
      )
    )
    LD <- LD_function(data, results, 0, 0, 0)
    x <- rep(1 / sqrt(nrow(data)), times = nrow(data))
    a <- safety(solnl(
      X = x,
      objfun = LD,
      confun = con,
      lb = llb,
      ub = uub
    ))
  }
  if (is.null(a$result) || !is.null(a$error) || !is.null(a$warning)) {
    stop("Funktion kann vom verwendeten Paket nicht optimiert werden.")
  }
  
  a <- as.double(a$result$par)
  issero <-
    sapply(1:nrow(data), function(i) {
      isTRUE(all.equal(a, 0))
    })
  a[issero] <- 0
  return(a)
}
#########################################################
#constrains
beta_svm_est <- function(alpha, data, results) {
  h <- alpha * data * results
  sapply(1:ncol(data), function(i) {
    sum(h[, i])
  })
}
beta_svm_0 <- function(alpha, data, results, beta) {
  data <- data[alpha != 0, ]
  results <- results[alpha != 0]
  alpha <- alpha[alpha != 0]
  be <- function(i, data, results, beta) {
    return(results[i] - as.double(data[i, ]) %*% beta)
  }
  s <-
    sapply(
      1:length(results),
      be,
      data = data,
      results = results,
      beta = beta
    )
  return(mean(as.double(s)))
}
#estimator####################################################
svm_two_classes <- function(data,
                            results,
                            C = 1,
                            kernel = 0,
                            d = 1,
                            j = 1,
                            classes ,
                            g = 1) {
    res <- sapply(results, function(class) {
      if (class == classes[j]) {
        return(1)
      }
      return(-1)
    })
    results <- res[]
    alpha <- alpha_svm_est(data, results, C, kernel, d, g)
    beta <- beta_svm_est(alpha, data, results)
    beta_Null <- beta_svm_0(alpha, data, results, beta)
    if (kernel == 0) {
      f <- function(x) {
        return(x %*% beta + beta_Null)
      }
    } else if (kernel == 1) {
      f <- function(x) {
        h <- 0
        for (i in 1:nrow(data)) {
          h[i] <- (alpha[i] * results[i]) * (1 + x %*% as.double(data[i, ])) ^ d
        }
        return(sum(h) + beta_Null)
      }
    } else if (kernel == 2) {
      f <- function(x) {
        h <- 0
        for (i in 1:nrow(data)) {
          h[i] <-
            (alpha[i] * results[i]) * exp(-g * sum((x - as.double(data[i, ])) ^ 2))
        }
        return(sum(h) + beta_Null)
      }
    } else{
      warning("Wrong Parameter kernel! No Kernel used.", immediate. = TRUE)
      f <- function(x) {
        return(x %*% beta + beta_Null)
      }
    }
    return(f)
  }

#more_classes###################################
#This function returns the decision-function comparing two classes
fun_list <-
  function(r,
           data,
           results,
           C,
           kernel,
           d,
           ob_mat,
           unique_results,
           length_unique_results,
           g) {
    temp <- list()
    for (s in (r + 1):length_unique_results) {
      dat <-
        rbind(data[ob_mat[1, r]:ob_mat[2, r], ], data[ob_mat[1, s]:ob_mat[2, s], ])
      res <-
        c(as.character(results[ob_mat[1, r]:ob_mat[2, r]]), as.character(results[ob_mat[1, s]:ob_mat[2, s]]))
      temp[[s]] <-
        svm_two_classes(
          data = dat,
          results = res,
          C = C,
          kernel = kernel,
          d = d,
          classes = unique_results,
          j = r,
          g = g
        )
      
    }
    return(temp)
  }
# This function returns a vector of length 2 containing the rownumber where class i starts and ends.
fun_ob <- function(i, ob) {
  if (i == 1) {
    return(c(1, sum(ob[1:i])))
  }
  return(c(sum(ob[1:(i - 1)]) + 1, sum(ob[1:i])))
}

svm_fun_list <- function(data,
                results,
                C = 1,
                kernel = 0,
                d = 1,
                g = 1) {
  unique_results <- unique(results)
  length_unique_results <- length(unique_results)
  ob <- table(results)
  if (length_unique_results == 2) {
    return(
      svm_two_classes(
        data = data,
        results = results,
        C = C,
        kernel = kernel,
        d = d,
        classes = unique_results,
        g = g
      )
    )
  }
  ob_mat <-
    sapply(1:length_unique_results, fun_ob, ob = ob) #this is a matrix with two rows, each colum i contains the row where class i starts and ends
  b <-
    lapply(
      1:(length_unique_results - 1),
      fun_list,
      data = data,
      results = results,
      C = C,
      kernel = kernel,
      d = d,
      ob_mat = ob_mat,
      unique_results = unique_results,
      length_unique_results = length_unique_results,
      g = g
    )
  #b is a list of functions comparing two classes each class i is compared with every class j>i,
  #so b[[i]][[j]] compares class i and j. Every other entry (e.g b[[j]][[i]]) is NULL.
  return(b)
}
#The svm_classify function returns a function that actually classifies an observation.
svm_classify <- function(t, uresults) {

  
  if (length(t) == 1) {
    f <- function(x) {
      if (t(x) >= 0) {
        return(uresults[1])
      }
      return(uresults[2])
    }
    return(f)
  }
  f <- function(x) {
    cla <- 1
    tr <- FALSE
    for (r in 1:(length(uresults) - 1)) {
      for (s in (r + 1):(length(uresults))) {
        a <- t[[r]][[s]](x)
        if (s == length(uresults) && a > 0) {
          cla <- r
          tr <- TRUE
          break
        }
        else if (s == length(uresults) && a < 0) {
          cla <- r + 1
        }
        if (a < 0)
          break
      }
      if (tr == TRUE)
        break
    }
    return(uresults[cla])
  }
  return(f)
}











#########test#############################
test <- make_test()
results <- test[,3]
data <- test[,1:2]
f <- svm(data,results,kernel=2,d=0.9,g=2)
dd <- svm_classify(f,c("A","B"))
gg <- 0
for (i in 1:200) {
  gg[i] <- (dd(as.double(data[i,])))
}
gg
liste4 <- plot_error(test[1:2], test$class, dd)
p4 <- do.call(grid.arrange, liste4)
testplot4 <-
  make_2D_plot(test[1:2],
               test$class,
               dd,
               ppu = 5)
plotlist4 <- list(p4, testplot4)

nice4 <-
  do.call("grid.arrange", c(plotlist4, ncol = 2, top = "svm"))
ggsave('svm.png',
       plot = nice4,
       device = 'png',
       dpi = 400)







#Vergleiche mit SVM aus Paket e1071