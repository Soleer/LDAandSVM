calc_miss <- function(data, results, f){
  estimations <- apply(data,1,f)
  n <- length(results)
  sum(results!=estimations)/n
}


#'calc_error
#'
#'Calculates the following propabilitys for a classificationfunction of a data_set:
#'[[1]]                                                        #accessed over: 
#'prop. of: class 'A' to be classified to a class 'B'          # ['B','A']
#'prop. of: class 'A' to be classified to 'A'                  # ['right','A']
#'prop. of: class 'A' to be not classified to 'A'              # ['wrong','A']
#'[[2]]
#'prop. of: classified to class 'A' and actually was class 'B' #['B','A']
#'prop. of: classified to class 'A' and also was 'A'           #['right','A']
#'prop. of: classified to class 'A' but was not 'A'            #['wrong','A']
#'[[3]]
#'prop. of: missclassification                                 #['miss']
#'
#'@param set A object of class 'data_set'
#'@param name the functionname of the function in set
#'@return A list with 3 entrys. Entry [[1]] and [[2]] are matrixes and contain the values as described in the description.
#' [[3]] is the propability of missclassification.
calc_error <- function(set, name) {
  if (!is.data_set(set)) {
    stop("Input must be of class 'data_set' (?make_set)", call. = FALSE)
  }
  if(!any(set$func_names==name)){
    stop(sprintf("%s is not in given  data_set",name), call. = FALSE)
  }
  info <- set$func_info[[name]][['parameter']]
  f <- set$func[[name]]
  G <- set$classnames
  estimated <- apply(set$data, 1, f)              # classify data with function to calc missclassifications
  
  of_Data <- lapply(G, function(class) {                    # calc missclassifications of dataset
    t <- table(estimated[set$results == set$classes[class]])
    if(sum(t)!=0){
      number <- sum(t)
    }
    else{
      number <- 1
    }
    order <- t[G]
    names(order) <- G
    order[is.na(order)] <- 0
    classresults <- as.list(order / number)
    right <- order[class] / number
    wrong <- (1 - right)
    col <- unlist(list(classresults, right, wrong))
    return(col)
  })
  
  of_Results <- lapply(G, function(class) {                 # mistake of f^-1(class)
    t <- table(set$results[estimated == set$classes[class]])
    if(sum(t)!=0){
      number <- sum(t)
    }
    else{
      number <- 1
    }
    order <- t[G]
    names(order) <- G
    order[is.na(order)] <- 0
    classresults <- as.list(order / number)
    right <- order[class] / number
    wrong <- (1 - right)
    col <- unlist(list(classresults, right, wrong))
    return(col)
  })
  probs_of_Data <-
    data.frame(class = c(G, 'right', 'wrong'), of_Data)
  probs_of_Results <-
    data.frame(class = c(G, 'right', 'wrong'), of_Results)
  colnames(probs_of_Data) <- c('class', G)
  colnames(probs_of_Results) <- c('class', G)
  miss <-
    sum(probs_of_Data[probs_of_Data$class == 'wrong', 1:set$n_classes +
                        1]) / set$n_classes
  miss <- round(miss, 2)
  return(list(probs_of_Data,probs_of_Results,miss=miss))
}
