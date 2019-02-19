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
#'@param set a data_set
#'@param func_name Functionname of the data_set function to plot as string. If equals FALSE
#'just the first two columns of the data will be plotted as points 
#'@param ppu points per unit of the backgound grid as integer
#'@param project logical or a character vector with names of two data columns, defines if maincomponent analysis should be used if logical.
#'@param bg logical, defines if a background grid should be generated
#'@return A 2 dimensional plot of the data_set with the function as 
make_2D_plot <- function(set,
                         func_name=FALSE,
                         ppu = 10,
                         project = TRUE,
                         bg = TRUE) {
  if (!is.data_set(set)) {
    stop("Input must be of class 'data_set' (?make_set)", call. = FALSE)
  }
  if(is.logical(func_name)&&!func_names){
    project <- FALSE
    bg <- FALSE
  }
  else if(!any(set$func_names==func_name)){
    stop(sprintf("%s is not in given  data_set",func_name), call. = FALSE)
  }
  #prama
  classfunc <- set$func[[func_name]]
  uresults <- set$classes
  n <- set$n_classes
  #Project on first two parameter or maincomponents
  if (is.logical(project)&&project) {
    proj <- make_projection(set)
    proj_to <- proj[[1]]
    proj_in <- proj[[2]]
    proj_data <-
      as.data.frame(t(apply(set$data, 1, proj_to)))
  }
  else if(is.character(project)){
    proj_data <- set$data[project]
    proj_in <- function(x){
      vec <- rep(0, times = (set$dim))
      vec[set$param==project[1]] <- x[1]
      vec[set$param==project[2]] <- x[2]
    }
  }
  else{
    proj_in <- function(x)
      c(x, rep(0, times = (set$dim - 2)))
    proj_data <- set$data
  }
  x <- c(floor(min(proj_data[, 1])), ceiling(max(proj_data[, 1])))
  y <- c(floor(min(proj_data[, 2])), ceiling(max(proj_data[, 2])))
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
  if(project){
    xtimes <- (x[2] - x[1]) * ppu
    ytimes <- (y[2] - y[1]) * ppu
    mainplot + theme(axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank(),
                     axis.title.y=element_blank(),
                     axis.text.y=element_blank(),
                     axis.ticks.y=element_blank())
  }
  #3. colored background
  if (bg) {
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
#'calc_error
#'
#'Calculates the following percentages for a function of a data_set:
#'an observation of a class 'A' classified to a class 'B'
#'class 'A' classified to 'A'
#'class 'A' not classified to 'A' 
#'classified to class 'A' actually was class 'B'
#'classified to class 'A' and also was 'A'
#'classified to class 'A' but was not 'A'
#'total missclassification of the data 
#'
#'@param set A object of class 'data_set'
#'@param name the functionname of the function in set
#'@return A list with 3 entrys: 
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
  return(list(probs_of_Data,probs_of_Results,total_miss=miss))
}

plot_error <- function(set, name) {
  if (!is.data_set(set)) {
    stop("Input must be of class 'data_set' (?make_set)", call. = FALSE)
  }
  if(!any(set$func_names==name)){
    stop(sprintf("%s is not in given  data_set",name), call. = FALSE)
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
      
      probs_Data[paste0(class, 'label')] <-         #Labels in percent
        paste0(round(probs_Data[, class],2)*100,'%')
      
      left <-                                         #class ... is classified to class ... by percent
        ggplot(data = probs_Data[1:n, ]) +            #make plot with aesthetics
        geom_bar(
          aes_string(
            x = paste0(class, 'label'),
            y = class,
            fill = 'class'
          ),
          stat = "identity",
          width = 1
        ) + theme(
          legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank()
        ) +
        labs(title = paste0('f(x=', class, ')'))             #
      
      probs_Results[paste0(class, 'label')] <- paste0(round(probs_Results[, class],2)*100,'%')#Labels in percent
      
      right <-                                        #if function classifys to class ... then it is actually class ... by percent
        ggplot(data = probs_Results[1:n, ]) +         #make plot with aesthetics 
        geom_bar(                    
          aes_string(
            x = paste0(class, 'label'),
            y = class,
            fill = 'class'
          ),
          stat = "identity",
          width = 1
        ) + theme(
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
    data.frame(i = mistake_lable, per = c(miss, 1 - miss),color=c('red1','green3'))  #combine labels and data
  
  mi <-
    ggplot(data = mistake) +                             #make plot
    geom_bar(aes(y = per,
                 x = i,
                 fill = color),
             stat = "identity",
             width = 1) + theme(
               legend.position = "none",
               axis.text.y = element_blank(),
               axis.title.y = element_blank(),
               axis.title.x = element_blank()
             )+ 
    scale_fill_identity() +
    ggtitle('Classified:')
  
  charts[[n + 1]] <-
    mi                                  #add last Plot
  return(charts)
}
#'plot_summary
#'
#'summarizes a function of a data_set in one plot
#'
#'@param set a data_set
#'@param name the name of the function to summarize
#'@param background logical, decides if the data plot should have a background grid
#'@param project logical, decides if maincomponent analyse should be used to make the data plot
#'@example 
#'plot_summary(set,"LDA_1")
#'
plot_summary <- function(set,name,background=TRUE,project=TRUE){
  liste0 <- plot_error(set, name)
  error_list <- do.call(grid.arrange, liste0)
  plot <-
    make_2D_plot(set,
               name,
               ppu = 5,
               background,
               project
               )
  plotlist <- list(error_list, plot)
  niceplot <- do.call("grid.arrange", c(plotlist, ncol = 2, top = sprintf("%s %s",set$title,name)))
  return(niceplot)
}
