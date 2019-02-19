library(MASS)
library(NlcOptim)
library(R6)
source("R/oop.R")
#library(e1071)
#library(ggplot2)
#library(gridExtra)
#source("R/Basis_expansion.R")
#source('R/Test.R')
#source("R/Estimators.R")
#source("R/LDA_QDA.R")
source("R/plot_functions.R")
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
LD_function <- function(data, results, values) {
  n <- length(results)
  cache <- matrix(0, ncol = n, nrow = n)
  if (!is.na(values$kernel) && values$kernel == "poly") {
    for (i in 1:n) {
      for (j in 1:i) {
<<<<<<< HEAD
        cache[i, j] <- results[i] * results[j] * (1 + sum(data[i, ] * data[j, ])) ^ values$d
        if(i==j){
          cache[i, i] <- 1/2*cache[i, i]
        }
=======
        cache[i, j] <- results[i] * results[j] * (1 + (sum(data[i, ] * data[j, ]))) ^
          values$d
>>>>>>> d0d8c175785a22aae52e76c8172baff51a071c59
      }
    }
  } else if (!is.na(values$kernel) && values$kernel == "radial") {
    for (i in 1:length(results)) {
      for (j in 1:i) {
        cache[i, j] <-
          results[i] * results[j] * exp(-(values$g) * sum((data[i, ] - data[j, ])^2))
        if(i==j){
          cache[i, i] <- 1/2*cache[i, i]
        }
      }
    }
#  } else if (values$kernel == 2) {
 #   for (i in 1:length(results)) {
  #    for (j in 1:i) {
   #     cache[i, j] <-
    #      results[i] * results[j] * exp(abs(-values$g) * sum(abs(data[i, ] - data[j, ])))
     # }
    #}
  } else{
    for (i in 1:n) {
      for (j in 1:i) {
        cache[i, j] <- results[i] * results[j] * (sum(data[i, ] * data[j, ]))
        if(i==j){
          cache[i, i] <- 1/2*cache[i, i]
        }
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

alpha_svm_est <- function(data, results, values) {
  C<- values$C
  N <- length(results)
  LD <- LD_function(data, results, values)
  con <- con_fun(results)
  n <- length(results[results==-1])
  pos <- min(which(results==1))
  neg <- min(which(results==-1))
  x <- rep(0, times = nrow(data))
  x[pos] <- C^2/(2*C) 
  x[neg] <- C^2/(2*C)
  llb <- rep(0, times = nrow(data))
  uub <- rep(values$C, times = nrow(data))
  a <- safety(solnl(
    X = x,
    objfun = LD,
    confun = con,
    lb = llb,
    ub = uub
  ))
  if (is.null(a$result) || !is.null(a$warning)) {
    warning(c(a$error, "\n Ändere Startwert zu 0:"),immediate. = TRUE)
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
    warning(c(a$error, "\n Ändere Startwert zu 1/C:"),immediate. = TRUE)
    x <- rep(1 / values$C, times = nrow(data))
    a <- safety(solnl(
      X = x,
      objfun = LD,
      confun = con,
      lb = llb,
      ub = uub
    ))
  }
  if (is.null(a$result) || !is.null(a$warning)) {
    warning(c(a$error, "\n Ändere Startwert zu (C^2)/(2*C):"),immediate. = TRUE)
    x <- rep(((values$C)^2)/(2*values$C), times = nrow(data))
    a <- safety(solnl(
      X = x,
      objfun = LD,
      confun = con,
      lb = llb,
      ub = uub
    ))
  }
  if ((is.null(a$result) || !is.null(a$warning)) && values$kernel != 0) {
    warning(
      c(
        a$error,
        "\n Ändere Startwert zurück zu 1/sqrt(nrow(data)) und verwende keinen Kernel:"
      ),immediate. = TRUE
    )
    return(NA)
  }
  
  if (is.null(a$result) || !is.null(a$error) || !is.null(a$warning)) {
    stop("Funktion kann vom verwendeten Paket nicht optimiert werden.")
  }
  a <- as.double(a$result$par)
  is_zero <-
    sapply(1:nrow(data), function(i) {
      isTRUE(all.equal(a, 0))
    })
  a[is_zero] <- 0
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
                            values,
                            j = 1,
                            classes) {
  res <- sapply(results, function(class) {
    if (class == classes[j]) {
      return(1)
    }
    return(-1)
  })
  results <- res[]
  alpha <- alpha_svm_est(data, results, values)
  if(is.na(alpha[1])){return(NA)}
  beta <- beta_svm_est(alpha, data, results)
  beta_Null <- beta_svm_0(alpha, data, results, beta)
  if (is.na(values$kernel)) {
   f <- function(x) {
      return(x %*% beta + beta_Null)
    }
  } else if (values$kernel == "poly") {
    f <- function(x) {
      h <- sapply(1:nrow(data), function(i){
        alpha[i] * results[i] * (1 + x %*% as.double(data[i, ])) ^ values$d
      })
      return(sum(h) + beta_Null)
    }
  } else if (values$kernel == "radial") {
    f <- function(x) {
      h <- sapply(1:nrow(data), function(i){
          alpha[i] * results[i] * exp(-values$g * sum((x - as.double(data[i, ])) ^ 2))
      })
      return(sum(h) + beta_Null)
    }
#  } else if (values$kernel == 2) {
 #   f <- function(x) {
  #    h <- 0
   #   for (i in 1:nrow(data)) {
    #    h[i] <-
     #     (alpha[i] * results[i]) * exp(-values$g * sum(abs(x - as.double(set$data[i, ]))))
      #}
      #return(sum(h) + beta_Null)
    #}
  } else{
    warning("Wrong Parameter kernel! No Kernel used.", immediate. = TRUE)
    f <- function(x) {
      return(x %*% beta + beta_Null)
    }
  }
  return(f)
}
###########################################################################
svm_two_classes_oop <- function(set,
                                values,
                                j = 1) {
  res <- sapply(set$results, function(class) {
    if (class == set$classes[j]) {
      return(1)
    }
    return(-1)
  })
  results <- res[]
  alpha <- alpha_svm_est(data, results, values)
  if(is.na(alpha[1])){return(NA)}
  beta <- beta_svm_est(alpha, set$data, results)
  beta_Null <- beta_svm_0(alpha, set$data, results, beta)
  if (is.na(values$kernel)) {
    f <- function(x) {
      return(x %*% beta + beta_Null)
    }
  } else if (!is.na(values$kernel) && values$kernel == "poly") {
    f <- function(x) {
      h <- 0
      for (i in 1:set$n_obs) {
        h[i] <- (alpha[i] * results[i]) * (1 + x %*% as.double(data[i, ])) ^ values$d
      }
      return(sum(h) + beta_Null)
    }
  } else if (!is.na(values$kernel) && values$kernel == "radial") {
    f <- function(x) {
      h <- 0
      for (i in 1:set$n_obs) {
        h[i] <-
          alpha[i] * results[i] * exp(-(values$g) * sum((x - as.double(set$data[i, ])) ^ 2))
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
           set,
           values ,
           ob_mat) {
    temp <- list()
    for (s in (r + 1):set$n_classes) {
      dat <-
        rbind(set$data[ob_mat[1, r]:ob_mat[2, r], ], set$data[ob_mat[1, s]:ob_mat[2, s], ])
      res <-
        c(as.character(set$results[ob_mat[1, r]:ob_mat[2, r]]), as.character(set$results[ob_mat[1, s]:ob_mat[2, s]]))
      temp[[s]] <-
        svm_two_classes(
          dat,
          res,
          values,
          j = r,
          set$classes
        )
      if(class(temp[[s]]) != "function" && is.na(temp[[s]])){return(NA)}
    }
    return(temp)
  }
# This function returns a vector of length 2 containing the rownumber where class i starts and ends.
fun_ob <- function(i, ob) {
  if (i == 1) {
    return(c(1, ob[1]))
  }
  return(c(sum(ob[1:(i - 1)]) + 1, sum(ob[1:i])))
}

svm_classify_list <- function(set, values) {
  if (set$n_classes == 2) {
    return(svm_two_classes_oop(set, values))
  }
  
  ob_mat <-
    sapply(1:set$n_classes, fun_ob, ob = set$count) #this is a matrix with two rows, each colum i contains the row where class i starts and ends
  b <- list()
  bool <- FALSE
  for (r in 1:(set$n_classes - 1)) {
    b[[r]] <- fun_list(r,
             set = set,
             values = values,
             ob_mat = ob_mat)
    if(is.na(b[[r]][1])){
      values$kernel <- NA_character_
      bool <- TRUE
      break
    }
  }
  if(isTRUE(bool)){
    return(svm_classify_list(set,values))
  }
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
          cla <- s
          tr <- TRUE
        }
        if (a < 0) {
          r <- s
          break
        }
      }
      if (tr == TRUE)
        break
    }
    return(uresults[cla])
  }
  return(f)
}


SVM <- function(set,
                C = 1,
                kernel = NA_character_,
                d = 1,
                g = 1) {
  ##The SVM classification function. A function factory
  if (!is.data_set(set)) {
    stop("Input must be of class 'data_set' (?make_set)")
  }
  if (length(set$func) > 0) {
    slot <- character(0)
    sapply(set$func_info, function(lis) {
      l <- lis[['parameter']]
      if (!is.null(l[["C"]]) &&
          !is.null(l[["kernel"]]) &&
          !is.null(l[["d"]]) &&
          !is.null(l[["g"]])) {
        if (l[["C"]] ==  C &&
            isTRUE(all.equal(l[["kernel"]],kernel)) &&
            l[["d"]] == d &&
            l[["g"]] == g) {
          slot <<- lis[["name"]]
        }
      }
    })
    if (length(slot) > 0) {
      return(list(name = slot, func = set$func[[slot]]))
    }
  }
  values <- list(
    "C" = C,
    "kernel" = kernel,
    "d" = d,
    "g" = g
  )
  t <- svm_classify_list(set, values)
  f <- svm_classify(t, set$classes)
  return(set$set_function(
    f,
    type = "SVM",
    parameter = list(
      base = 'id',
      dim = NULL,
      omega = NULL,
      C = C,
      kernel = kernel,
      d = d,
      g = g
    )
  ))
}








#test##########################
print("VORISCHT,TEST")
test <- make_test(nclasses = 3,ninputs = 50)
test <- make_set(test,"class","TITEL",description = "Description")
test$func_names
results <- test$results
data <- test$data
<<<<<<< HEAD
dd <- SVM(test,C = 1,kernel = "poly",d=2,g=-3)[['name']]
f <- test$func[[dd]]
calc_error(test,dd)
=======
dd <- SVM(test,C = 1,kernel = "poly",d=2,g=1)
dd$func(as.double(data[1,]))

gg <- 0
for (i in 1:150) {
  gg[i] <- (dd$func(as.double(data[i,])))
}
gg




#Vergleiche mit SVM aus Paket e1071
>>>>>>> d0d8c175785a22aae52e76c8172baff51a071c59
