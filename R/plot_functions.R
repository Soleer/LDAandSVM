library(ggplot2)
library(gridExtra)
#Plot functions

maincomponent_analysis <- function(data) {
  cov_matrix <- sigma_class(data)
  ev <- eigen(cov_matrix)
  values <- ev$values
  n <- length(values)
  main_matrix <- ev$vectors
  D <- diag(values, nrow = n, ncol = n)
  return(list(D, main_matrix, cov_matrix))
}

make_projection <- function(data, dim = 2) {
  l <- maincomponent_analysis(data)
  U <- l[[2]][, 1:dim]
  proj <- function(x) {
    t(U) %*% x
  }
  i_proj <- function(x) {
    U %*% x
  }
  return(list(proj, i_proj))
}

make_2D_plot <- function(data,
                         results,
                         classfun,
                         ppu = 10,
                         bg = TRUE) {
  #prama
  uresults <- unique(results)
  n <- length(uresults)
  proj <- make_projection(data)
  proj_to <- proj[[1]]
  proj_in <- proj[[2]]
  proj_data <- as.data.frame(t(apply(data, 1, proj_to)))
  x <- c(floor(min(proj_data[, 1])), roof(max(proj_data[, 1])))
  y <- c(floor(min(proj_data[, 2])), roof(max(proj_data[, 2])))
  xtimes <- (x[2] - x[1]) * ppu
  ytimes <- (y[2] - y[1]) * ppu
  d <- dim(data)[2]
  #prepare plot data
  #input
  input_data <-
    data.frame(x = proj_data[, 1], y = proj_data[, 2], Legend = results)
  #make mainplot
  #1. limit
  mainplot <- ggplot() + xlim(x[1], x[2]) + ylim(y[1], y[2])
  #2. input data
  mainplot <-
    mainplot + geom_jitter(
      data = input_data,
      aes(x = x, y = y, color = Legend),
      shape = 20,
      height = 0,
      width = 0
    )
  #3. colored background
  if (bg == TRUE) {
    background <-
      data.frame(x = rep(seq(x[1], x[2], length.out = xtimes), times = ytimes),
                 y = c(sapply(seq(y[1], y[2], length.out = ytimes), rep, times = xtimes)))
    proj_background <-
      as.data.frame(t(apply(background, 1, proj_in)))
    background$class <- apply(proj_background, 1, classfun)
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

calc_error <- function(data, results, f) {
  G <- unique(results)
  estimated <- apply(data, 1, f)
  of_Data <- lapply(G, function(class) {
    c <- as.character(class)
    t <- table(estimated[results == class])
    number <- sum(t)
    classresults <- as.list(t[as.character(G)] / number)
    right <- t[c] / number
    wrong <- (1 - right)
    col <- unlist(list(classresults, right, wrong))
    return(col)
  })
  of_Results <- lapply(G, function(class) {
    c <- as.character(class)
    t <- table(results[estimated == class])
    number <- sum(t)
    classresults <- as.list(t[as.character(G)] / number)
    right <- t[c] / number
    wrong <- (1 - right)
    col <- unlist(list(classresults, right, wrong))
    return(col)
  })
  probs_of_Data <-
    data.frame(class = c(as.character(G), 'right', 'wrong'), of_Data)
  probs_of_Results <-
    data.frame(class = c(as.character(G), 'right', 'wrong'), of_Results)
  colnames(probs_of_Data) <- c('class', as.character(G))
  colnames(probs_of_Results) <- c('class', as.character(G))
  miss <-
    sum(probs_of_Data[probs_of_Data$class == 'wrong', 2:length(G)]) / length(G)
  return(list(probs_of_Data, probs_of_Results, miss))
}


plot_error <- function(data, results, f) {
  G <- as.character(unique(results))
  n <- length(G)
  get_list <- calc_error(data, results, f)
  probs_Data <- get_list[[1]]
  probs_Results <- get_list[[2]]
  miss <- get_list[[3]]
  charts <- lapply(G, function(class) {
    
    probs_Data[paste0(class, 'l')] <-
      scales::percent(probs_Data[, class])
    colsum <- 0
    probs_Data[paste0(class, 'yl')] <-
      sapply(probs_Data[, class], function(x) {
        colsum <<- colsum + x
        colsum - x / 2
      })
    left <- ggplot(data = probs_Data[1:n, ]) +
      geom_bar(
        aes_string(
          x = paste0(class, 'l'),
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
      labs(title = paste0('f(x=', class, ')'))
    
    probs_Results[paste0(class, 'l')] <-
      scales::percent(probs_Results[, class])
    colsum <- 0
    probs_Results[paste0(class, 'yl')] <-
      sapply(probs_Results[, class], function(x) {
        colsum <<- colsum + x
        colsum - x / 2
      })
    right <- ggplot(data = probs_Results[1:n, ]) +
      geom_bar(
        aes_string(
          y = class,
          x = paste0(class, 'l'),
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
      labs(title = paste0('f^-1(', class, ')'))
    return(grid.arrange(left, right, nrow = 1))
  })
  mistake_lable <- c(paste0(miss*100,'%',' wrong'),paste0((1-miss)*100,'%',' right'))
  mistake <- data.frame(i=mistake_lable,per=c(miss,1-miss))
  mi <- ggplot(data = mistake) +
    geom_bar(
      aes(
        y = per,
        x = i,
        fill = i
      ),
      stat = "identity",
      width = 1
    ) + theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "none",
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    )+ggtitle('Data')
  charts[[n+1]] <- mi
  return(charts)
}