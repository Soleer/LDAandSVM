#Plot functions
library(ggplot2)
library(gridExtra)
source("R/Calc_error.R")

#'maincomponent_analysis
#'
#'@param set a data_set
#'@return matrix whose columns contain the eigenvectors of set$sigma_bet.
#' The vectors are normalized to unit length and sorted by decreasing eigenvalues. 
maincomponent_analysis <- function(set) {
  cov_matrix <- set$sigma_bet
  ev <- eigen(cov_matrix)
  main_matrix <- ev$vectors
  return(main_matrix)
}

#'make_projection
#'
#'@param set a data_set
#'@param dim dimension of target space
#'@return A list with a function that reduces to dim via maincomponent analysis and one that projects back by setting all other dimensions to zero.
make_projection <- function(set, dim = 2) {
  U <- maincomponent_analysis(set)[, 1:dim]
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
#'Takes a data_set and creates a 2 dimensional plot
#'@param set a data_set
#'@param func_name Functionname of the data_set function to plot as string. If equals FALSE
#'just the first two columns or the the columns defined with project will be plotted as points in a plane. 
#'@param ppu points per unit of the backgoundgrid as integer 
#'@param project logical or vector with a selection of two data columns.If logical: defines if maincomponent analysis should be used.
#'@param bg logical, defines if a background grid should be generated
#'@return A 2 dimensional plot of the data_set with the function as 
#'@examples
#'test <- make_test()
#'set <- make_set(test, by='class')
#'make_2D_plot(set, project = c(1,1), project=FALSE)
#'
#'make_2D_plot(set,'LDA_1', ppu = 3)
#'make_2D_plot(set,'LDA_1', project = c('a','b'))
make_2D_plot <- function(set,
                         func_name=FALSE,
                         ppu = 5,
                         project = TRUE,
                         bg = TRUE) {
  
  if (!is.data_set(set)) {                                               #check if input correct
    stop("Input must be of class 'data_set' (?make_set)", call. = FALSE)
  }
  if(is.logical(func_name)&&!func_names){
    project <- FALSE
    bg <- FALSE
  }
  else if(!any(set$func_names==func_name)){
    stop(sprintf("%s is not in given  data_set",func_name), call. = FALSE)
  }
  if(!set$dim>=2){
    stop("set dimension must be at least two!", call. = FALSE)
  }
  stopifnot(is.numeric(ppu)&&is.logical(bg))
  #prama
  classfunc <- set$func[[func_name]]
  uresults <- set$classes
  n <- set$n_classes
  
  #Project on selected parameter or first two eigenvectors
  if (is.logical(project)&&project) {        
    proj <- make_projection(set)            #use maincomponent analysis
    proj_to <- proj[[1]]                    
    proj_in <- proj[[2]]                    #projectfunction for grid
    proj_data <-
      as.data.frame(t(apply(set$data, 1, proj_to))) # project data
  }
  else if(is.character(project)&&length(project)==2){
    proj_data <- set$data[project] # select two columns 
    proj_in <- function(x){        # projectfunction for grid
      vec <- rep(0, times = set$dim)
      vec[set$parnames==project[1]] <- x[1]
      vec[set$parnames==project[2]] <- x[2]
      return(vec)
    }
  }
  else{
    proj_in <- function(x) c(x, rep(0, times = (set$dim - 2)))   #project function for grid
    proj_data <- set$data[,c(1,2)]                               #use first two columns   
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
  if(is.logical(project)&&project){
    mainplot + theme(axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank(),
                     axis.title.y=element_blank(),
                     axis.text.y=element_blank(),
                     axis.ticks.y=element_blank())
  }
  #3. colored background
  if (bg) {
    xtimes <- (x[2] - x[1]) * ppu
    ytimes <- (y[2] - y[1]) * ppu
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
#'@examples 
#'plot_summary(set,"LDA_1")
plot_summary <- function(set, name, background=TRUE, project=TRUE){
  liste0 <- plot_error(set, name)
  plot_list <- do.call(grid.arrange, liste0)
  
  if(set$dim>=2){
    plot <- make_2D_plot(set,
               name,
               ppu = 5,
               project,
               background
               )
  plot_list[[length(plot_list)+1]] <-  plot
  }
  
  niceplot <- do.call("grid.arrange", c(plot_list, ncol = 2, top = sprintf("%s %s",set$title,name)))
  return(niceplot)
}
