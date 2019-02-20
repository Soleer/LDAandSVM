# This function catches the error-messages of another function expr and prints them as warnings.
# It returns a list containing the output of the function expr, a slot for error-messages
# and a slot for warning-messages.
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

#######################################################################################
# This is the function that has to be maximised to solve the SVM-Classification problem.
# See Hastie, Tibshirani, Friedman (2009) - The Elements of Statistical Learning - 2nd Edition,page 420
LD_function <- function(data, results, values) {
  n <- length(results)
  cache <- matrix(0, ncol = n, nrow = n)    #saving some results for easier calculation
  # The LD_function depends on the kernel:
  if (values$kernel == "poly") {
    for (i in 1:n) {
      for (j in 1:i) {
        cache[i, j] <- results[i] * results[j] * (1 + sum(data[i, ] * data[j, ])) ^ values$d
        if(i==j){
          cache[i, i] <- 1/2*cache[i, i]
        }
      }
    }
  } else if (values$kernel == "radial") {
    for (i in 1:length(results)) {
      for (j in 1:i) {
        cache[i, j] <-
          results[i] * results[j] * exp(-(values$g) * sum((data[i, ] - data[j, ])^2))
        if(i==j){
          cache[i, i] <- 1/2*cache[i, i]
        }
      }
    }
  } else if (values$kernel == "neural") {
    for (i in 1:n) {
      for (j in 1:i) {
        cache[i, j] <-
          results[i] * results[j] * tanh(values$d*sum(data[i, ] * data[j, ]) + values$g)
        if(i == j){
          cache[i, i] <- 1/2*cache[i, i]
        }
      }
    }
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
  # Returnging the function:
  f <- function(a) {
    g <- 0
    for (i in 1:n) {
      for (j in 1:i) {
        g <- g + a[i] * a[j] * cache[i, j]
      }
    }
    return(-(sum(a) - g))
  }
  return(f)
}

###############################################################
#The conditions under which the LD function has to be maximised
con_fun <- function(results) {
  con <- function(x) {
    f = NULL
    f = rbind(f, sum(x * results))
    return(list(ceq = f, c = NULL))
  }
  return(con)
}

############################################################################################
# This parameter is calculated by maximising the LD_function (in this case minimising -(LD))
# under the constraints of con_fun and 0 <= a[i] <= C for all i in 1:length(results)
alpha_svm_est <- function(data, results, values) {
  # Assign some variables
  C <- values$C
  N <- length(results)
  LD <- LD_function(data, results, values)
  con <- con_fun(results)
  pos_first <- min(which(results == 1))
  neg_first <- min(which(results == -1))
  # Select the starting point for the minimisation. This point has to fullfill all constraints
  x <- rep(0, times = N)
  x[pos_first] <- C^2/(2*C)  # 0 <= C^2/(2*C) <= C
  x[neg_first] <- C^2/(2*C)  # 0 <= C^2/(2*C) <= C
  # Define lower and upper bound for alpha
  llb <- rep(0, times = N)
  uub <- rep(C, times = N)
  # Calculate alpha using solnl from package NlcOptim.
  # This function might return error-messages, so use safety()
  alpha <- safety(solnl(
    X = x,
    objfun = LD,
    confun = con,
    lb = llb,
    ub = uub
  ))
  # Occurring error-messages might depend on the starting point x, so change the starting point a few times.
  if (is.null(alpha$result) || !is.null(alpha$warning)) {
    warning(c(alpha$error, "\n Change starting point for minimisation!"),immediate. = TRUE)
    pos_last <- max(which(results == 1))
    neg_last <- max(which(results == -1))
    x[pos_last] <- C^2/(2*C) 
    x[neg_last] <- C^2/(2*C)
    alpha <- safety(solnl(
      X = x,
      objfun = LD,
      confun = con,
      lb = llb,
      ub = uub
    ))
  }
  if (is.null(alpha$result) || !is.null(alpha$warning)) {
    warning(c(alpha$error, "\n Change starting point again!"),immediate. = TRUE)
    x[pos_first] <- 0
    x[neg_first] <- 0
    alpha <- safety(solnl(
      X = x,
      objfun = LD,
      confun = con,
      lb = llb,
      ub = uub
    ))
  }
  if (is.null(alpha$result) || !is.null(alpha$warning)) {
    warning(c(alpha$error, "\n Change starting point to 0"),immediate. = TRUE)
    x[pos_last] <- 0 
    x[neg_last] <- 0
    alpha <- safety(solnl(
      X = x,
      objfun = LD,
      confun = con,
      lb = llb,
      ub = uub
    ))
  }
  # The starting point has been changed a few times. If the error still occurs and a kernel has
  # been used, the problem might be solved by calculating alpha without using a kernel.
  if ((is.null(alpha$result) || !is.null(alpha$warning)) && values$kernel != "id") {
    warning(
      c(
        alpha$error,
        "\n Change starting point back to the first and don't use a kernel:"
      ),immediate. = TRUE
    )
    # If no kernel is used for one two_class_function, no kernel has to be used for all
    # two_class_function, so return NA and start again from the beginning without using a
    # kernel.
    return(NA)
  }
  # If the error still occurs, stop the function and return an error message.
  if (is.null(alpha$result) || !is.null(alpha$error) || !is.null(alpha$warning)) {
    stop("Function can't be optimised by solnl() from package NlcOptim.")
  }
  # Alpha is a list containing the return value of solnl, the error messages and the
  # warning messages solnl returns. The return value of solnl is a list itself, the actual
  # optimised solution for alpha can be accessed using $par
  # Alpha should be a vector containig only the solution of solnl.
  alpha <- as.double(alpha$result$par)
  # This vector contains the positions where alpha is zero.
  is_zero <-
    sapply(1:N, function(i) {
      isTRUE(all.equal(alpha, 0))
    })
  # replace all positions equal to zero with zero.
  alpha[is_zero] <- 0
  return(alpha)
}

##########################################################################################
# beta is another parameter-vector depending on the data, the results(1 and -1) and alpha.
# See Hastie, Tibshirani, Friedman (2009) - The Elements of Statistical Learning - 2nd Edition,page 421
beta_svm_est <- function(alpha, data, results) {
  h <- alpha * data * results
  sapply(1:ncol(data), function(i) { sum(h[, i]) })
}

###################
# beta_0 is a scalar
beta_svm_0 <- function(alpha, data, results, beta) {
  #beta_0 is calculated using the places where alpha is not zero.
  data <- data[alpha != 0, ]
  results <- results[alpha != 0]
  alpha <- alpha[alpha != 0]
  # The Karush-Kuhn-Tucker conditions are used to calculate beta_0 
  be <- function(i, data, results, beta) {
    return(results[i] - as.double(data[i, ]) %*% beta)
  }
  # The conditions are applied on every row of the data set.
  s <-
    sapply(1:length(results), be, data = data, results = results, beta = beta )
  # Beta_0 is the mean of all constraints
  return(mean(as.double(s)))
}

##################################################################
# This function returns the SVM decision function for two classes.
svm_two_classes <- function(data,
                            results,
                            values,
                            j = 1,
                            classes) {
  # At first the names of the two classes have to be changed to 1 and -1
  res <- sapply(results, function(class) {
    if (class == classes[j]) {
      return(1)
    }
    return(-1)
  })
  # Now change results to an atomic vector containing 1 for the first and -1 for the second class.
  results <- res[]
  #assign alpha
  alpha <- alpha_svm_est(data, results, values)
  # If is.na(alpha[1]) is TRUE, then alpha could not be calculated and a kernel was used.
  # Stop all calculations and return(NA) in order to start again from the beginning using no kernel.
  if (is.na(alpha[1])) { return(NA)}
  # Assign beta and beta_NULL
  beta <- beta_svm_est(alpha, data, results)
  beta_Null <- beta_svm_0(alpha, data, results, beta)
  # Now between the different kernels has to be distinguished.
  if (values$kernel == "id") {
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
      return(sum(h))
    }
  } else if (values$kernel == "neural") {
    f <- function(x) {
      h <- sapply(1:nrow(data), function(i){
        alpha[i] * results[i] * tanh(values$d*sum(x * data[j, ]) + values$g)
      })
      return(sum(h))
    }
  } else{
    warning("Wrong Parameter kernel! No Kernel used.", immediate. = TRUE)
    f <- function(x) {
      return(x %*% beta + beta_Null)
    }
  }
  # Return the decision function.
  return(f)
}

#############################################s#####################################################
# This function returns a list containing the decision-functions comparing class r (the r-th class)
# to classes (r+1):(amount of classes). Each entry i contains the decision-function
# comparing class r to class i.
fun_list <-
  function(r,
           set,
           values) {
    # Define the list that will be returned
    temp <- list()
    for (s in (r + 1):set$n_classes) {
      # Define a data-set containing the observations of class r and s
      dat <-
        rbind(set$data[set$results == set$classes[r], ], set$data[set$results == set$classes[s], ])
      # Define an atomic vector containing the associated classes
      res <-
        c(as.character(set$results[set$results == set$classes[r]]), as.character(set$results[set$results == set$classes[s]]))
      # Assign the decision function comparing the r-th class and the s-th class to postion s in
      # the list temp.
      temp[[s]] <-
        svm_two_classes(
          dat,
          res,
          values,
          j = r,
          set$classes
        )
      # If is.na(temp[[s]] is TRUE, then alpha could not be calculated and a kernel was used.
      # Stop all calculations and return(NA) in order to start again from the beginning using no kernel.
      if (class(temp[[s]]) != "function" && is.na(temp[[s]])) {return(NA)}
    }
    return(temp)
}

######################################################################################
# The SVM decision function described in Hastie - The Elements of Statistical Learning
# only works for two classes. So in order to use SVM for more classes, this function returns a
# list (lets call it list1 for a better overview) with every entry containing a list  of
# decision functions, if the input contains more then two classes.
# The first entry of list1 is a list comparing the first class to every other class
# (eg. list1[[1]][[1]]==NULL,list1[[1]][[2]] is the decision function comparing class 1 to class 2,
#   list1[[1]][[3]] is the decision function comparing class 1 to class 3, etc.).
# The second entry of list1 is a list comparing the second class to every class 
# 3:(amount of classes) (eg. list1[[2]][[1]] = list1[[2]][[2]] = NULL, list1[[2]][[3]] is a 
#                         decision function comparing the second class to the third class).
# The length of the list is (amount of classes) - 1, because the last class is already compared
# to every class and doesn't has to be compared to any other class anymore.
svm_classify_list <- function(set, values) {  
  # If the input contains only two classes, then the output of the function is the SVM-decision-function
  if (set$n_classes == 2) {
    b <- svm_two_classes(set$data, set$results, values = values, classes = set$classes)
    # If b is NA, then try to calculate the decsion-function again using no kernel
    if (class(b) != "function" && is.na(b)) {
      # In order to use no kernel, values$kernel has to be set to id
      values$kernel <- "id"
      # calculate the decsion-function again using no kernel
      b <- svm_two_classes(set$data, set$results, values = values, classes = set$classes)
    }
    return(b)
  }
  # b is the list of lists of functions that will be returned 
  b <- list()
  # bool is a boolean that is used to restart svm_classify_list without a kernel 
  # in case one decision-function can't be evaluated and a kernel was used.
  bool <- FALSE
  for (r in 1:(set$n_classes - 1)) {
    # assign b[[r]] with a list of dedision-functions comparing the r-th class to 
    # classes (r+1):(amount of classes)
    b[[r]] <- fun_list(r,
                       set = set,
                       values = values)
    # If b is NA, then try to calculate the decsion-functions again using no kernel
    if(is.na(b[[r]][1])){
      values$kernel <- "id"
      bool <- TRUE
      break
    }
  }
  if(isTRUE(bool)){
    return(svm_classify_list(set,values))
  }
  # b is a list of functions comparing two classes each class i is compared with every class j>i,
  # so b[[i]][[j]] compares class i and j. Every other entry (e.g b[[j]][[i]]) is NULL.
  return(b)
}


######################################################################################
# The svm_classify function returns a function that actually classifies an observation.
svm_classify <- function(t, uresults) {
  # t is a list of functions, uresults are the names of the classes.
  # If the input contains only two classes, then length(t) == 1.
  if (length(t) == 1) {
    # In this case the decision depend only on the sign of function t.
    # If the sign is positive or equal zero, the observation belongs to the first class,
    # if the sign is negative, the observation belongs to the second class.
    f <- function(x) {
      if (t(x) >= 0) {
        return(uresults[1])
      }
      return(uresults[2])
    }
    return(f)
  }
  # Returning a function that actually classifies an observation is a little bit more complicated
  # if the training data contained more than two classes.
  f <- function(x) {
    # cla is the return value
    cla <- 1
    # tr is a boolean to stop the first for-loop.
    tr <- FALSE
    # Iterate over all entries of list t
    for (r in 1:(length(uresults) - 1)) {
      # Every entry is a list itself. Iterate from r + 1 to length(uresults) = number of classes
      for (s in (r + 1):(length(uresults))) {
        # save t[[r]][[s]](x), the value evaluated by the decision-function comparing class r to s.
        a <- t[[r]][[s]](x)
        
        # As soon as a < 0 for the first time, the observation lays rather in class s than 
        # in class r, in this case for every r<=i<=s, the observation layed rather in class r than 
        # in class i. So class s only has to be compared to classes (s+1):(amount of classes).
        # So the third if-clause if(a<0) assigns r <- s and stops the second for-loop.
        # If s was the last class, than the observation lays in the last class, so the second
        # if-clause [else if (s == length(uresults) && a < 0)] assigns cla <- s and tr <- TRUE.
        # The last case is that a >= 0 for all s in r:(amount of classes). In this case the
        # observation lays in class r. The first if-clause handles this case, assigns cla <- r
        # tr <- TRUE and breaks the second for-loop.
        
        # The following if-clauses handle the cases explained above
        if (s == length(uresults) && a > 0) {
          cla <- r
          tr <- TRUE
          break
        }
        else if (s == length(uresults) && a < 0) {
          cla <- s
          tr <- TRUE
          break
        }
        if (a < 0) {
          r <- s
          break
        }
      }
      # If tr == TRUE break the first for-loop
      if (tr == TRUE)
        break
    }
    # The observation lays in the cla-th class, so return uresults[cla]
    return(uresults[cla])
  }
  # return the function that classifies an observation.
  return(f)
}
