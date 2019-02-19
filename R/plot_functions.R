#Plot functions
library(ggplot2)
library(gridExtra)

#'maincomponent_analysis
#'
#'@param data Dataframe of Parameters
#'@param results Vector with corresponding Classes to \code{data}
#'@return A list with  the mean vectors of the classes as rows
maincomponent_analysis <- function(set) {
  cov_matrix <- set$sigma_bet
  ev <- eigen(cov_matrix)
  values <- ev$values
  n <- length(values)
  main_matrix <- ev$vectors
  D <- diag(values, nrow = n, ncol = n)
  return(list(D, main_matrix, cov_matrix))
}

#'make_projection
#'
#'@param data Dataframe of Parameters
#'@param results Vector with corresponding Classes to \code{data}
#'@param dim dimension of target space
#'@return A list with a function that reduces to dim via maincomponent analysis and one
make_projection <- function(set, dim = 2) {
  l <- maincomponent_analysis(set)
  U <- l[[2]][, 1:dim]
  proj <- function(x) {
    t(U) %*% x
  }
  i_proj <- function(x) {
    U %*% x
  }
  return(list(proj, i_proj))
}

#'make_2D_plot
#'
#'@param set A data_set
#'@param func_name Functionname of the data_set function to plot
#'@param ppu points per unit 
make_2D_plot <- function(set,
                         func_name,
                         ppu = 10,
                         project = TRUE,
                         bg = TRUE) {
  if (!is.data_set(set)) {
    stop("Input must be of class 'data_set' (?make_set)", call. = FALSE)
  }
  if(!any(set$func_names==func_name)){
    stop(sprintf("%s is not in given  data_set",func_name), call. = FALSE)
  }
  #prama
  info <- set$func_info[[func_name]][['parameter']]
  classfunc <- set$func[[func_name]]
  uresults <- set$classes
  n <- set$n_classes
  #Project on first two parameter or maincomponents
  if (project == TRUE) {
    proj <- make_projection(set)
    proj_to <- proj[[1]]
    proj_in <- proj[[2]]
    proj_data <-
      as.data.frame(t(apply(set$data, 1, proj_to)))
  }
  else{
    proj_in <- function(x)
      c(x, rep(0, times = (info[['dim']] - 2)))
    proj_data <- set$data
  }
  
  x <- c(floor(min(proj_data[, 1])), ceiling(max(proj_data[, 1])))
  y <- c(floor(min(proj_data[, 2])), ceiling(max(proj_data[, 2])))
  xtimes <- (x[2] - x[1]) * ppu
  ytimes <- (y[2] - y[1]) * ppu
  d <- set$dim
  #prepare plot data
  #input
  input_data <-
    data.frame(x = proj_data[, 1],
               y = proj_data[, 2],
               Legend = set$results)
  #make mainplot
  #1. limit
  mainplot <- ggplot() + xlim(x[1], x[2]) + ylim(y[1], y[2])
  #2. input data
  mainplot <-
    mainplot + geom_jitter(
      data = input_data,
      aes(x = x, y = y, color = Legend),
      shape = 19,
      height = 0,
      width = 0
    )
  
  #3. colored background
  if (bg == TRUE) {
    background <-
      data.frame(x = rep(seq(x[1], x[2], length.out = xtimes), times = ytimes),
                 y = rep(seq(y[1], y[2], length.out = ytimes),  each = xtimes))
    proj_background <-
      as.data.frame(t(apply(background, 1, proj_in)))
    background$class <- apply(proj_background, 1, classfunc)
    mainplot <- mainplot + geom_jitter(
      data = background,
      aes(x, y, color = class),
      shape = 3,
      height = 0,
      width = 0
    )
  }
  return(mainplot)
}

calc_error <- function(set, name) {
  if (!is.data_set(set)) {
    stop("Input must be of class 'data_set' (?make_set)", call. = FALSE)
  }
  if(!any(set$func_names==name)){
<<<<<<< HEAD
    stop(sprintf("%s is not in given  data_set",func_name), call. = FALSE)
=======
    stop(sprintf("%s is not in given  data_set",name), call. = FALSE)
>>>>>>> 35d72d5f568c109c6cd3552f738ab6c8cf066595
  }
  info <- set$func_info[[name]][['parameter']]
  f <- set$func[[name]]
  G <- set$classnames
  estimated <- apply(set$data, 1, f)
  of_Data <- lapply(G, function(class) {
    t <- table(estimated[set$results == set$classes[class]])
    number <- sum(t)
    classresults <- as.list(t[G] / number)
    right <- t[class] / number
    wrong <- (1 - right)
    col <- unlist(list(classresults, right, wrong))
    return(col)
  })
  
  of_Results <- lapply(G, function(class) {
    t <- table(set$results[estimated == set$classes[class]])
    number <- sum(t)
    classresults <- as.list(t[G] / number)
    right <- t[class] / number
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
  return(list(probs_of_Data, probs_of_Results, miss))
}

plot_error <- function(set, name) {
  if (!is.data_set(set)) {
    stop("Input must be of class 'data_set' (?make_set)", call. = FALSE)
  }
  if(!any(set$func_names==name)){
<<<<<<< HEAD
    stop(sprintf("%s is not in given  data_set",func_name), call. = FALSE)
=======
    stop(sprintf("%s is not in given  data_set",name), call. = FALSE)
>>>>>>> 35d72d5f568c109c6cd3552f738ab6c8cf066595
  }
  G <- set$classnames
  n <- set$n_classes
  get_list <- calc_error(set, name)
  probs_Data <- get_list[[1]]
  probs_Results <- get_list[[2]]
  miss <- get_list[[3]]
  
  charts <-
    lapply(G, function(class) {
      # Create mistake plots for each Class
      
      probs_Data[paste0(class, 'label')] <-    #Labels in percent
<<<<<<< HEAD
        paste0(probs_Data[, class]*100,'%')
=======
        paste0(round(probs_Data[, class],2)*100,'%')
>>>>>>> 35d72d5f568c109c6cd3552f738ab6c8cf066595
      
      left <-
        ggplot(data = probs_Data[1:n, ]) +            #make plot mit aesthetics
        geom_bar(
          aes_string(
            x = paste0(class, 'label'),
            y = class,
            fill = 'class'
          ),
          stat = "identity",
          width = 1
        ) + theme(
          axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank()
        ) +
        labs(title = paste0('f(x=', class, ')'))             #
      
<<<<<<< HEAD
      probs_Results[paste0(class, 'label')] <- paste0(probs_Results[, class]*100,'%')#Labels in percent
=======
      probs_Results[paste0(class, 'label')] <- paste0(round(probs_Results[, class],2)*100,'%')#Labels in percent
>>>>>>> 35d72d5f568c109c6cd3552f738ab6c8cf066595
      
      right <-
        ggplot(data = probs_Results[1:n, ]) +         #make plot mit aesthetics
        geom_bar(
          aes_string(
            y = class,
            x = paste0(class, 'label'),
            fill = 'class'
          ),
          stat = "identity",
          width = 1
        ) + theme(
          axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = "none",
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank()
        ) +
        labs(title = paste0('f^-1(', class, ')'))            #
      
      return(grid.arrange(left, right, nrow = 1))   # arrange in one row and return
    })
  
  
  mistake_lable <-
    c(paste0(miss * 100, '%', ' wrong'), paste0((1 - miss) * 100, '%', ' right')) #Labels in percent
  
  mistake <-
    data.frame(i = mistake_lable, per = c(miss, 1 - miss))  #combine labels and data
  
  mi <-
    ggplot(data = mistake) +                             #make plot
    geom_bar(aes(y = per,
                 x = i,
                 fill = i),
             stat = "identity",
             width = 1) + theme(
               axis.text.x = element_text(angle = 90, hjust = 1),
               legend.position = "none",
               axis.text.y = element_blank(),
               axis.title.y = element_blank(),
               axis.title.x = element_blank()
             ) + ggtitle('Data')
  
  charts[[n + 1]] <-
    mi                                  #add last Plot
  return(charts)
}

<<<<<<< HEAD
vec <- c("a","b","c")
paste0(vec,"%")
=======
>>>>>>> 35d72d5f568c109c6cd3552f738ab6c8cf066595
