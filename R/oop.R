library(R6)
#Our Basic Object "Dataset":

make_set <- R6Class(
  "Dataset",
  private = list(
    #list of private slots
    .data = NULL,
    .results = NULL,
    .col_names = NULL,
    .classes = NULL,
    .classnames = NULL,
    .parnames = NULL,
    .n_classes = 0,
    .count = 0,
    .dim = 0,
    .n_obs = 0,
    .title = "",
    .description = "",
    .pi = 0,
    .mean = 0,
    .meantotal = 0,
    .sigma = 0,
    .sigma_bet = 0,
    .classification_functions = list(),
    .function_informations = list()
  ),
  public = list(
    #init function
    initialize = function(data,
                          by,
                          title = "",
                          description = "") {
      #Check Input
      stopifnot(is.data.frame(data) == TRUE)
      private$.col_names <- colnames(data)
      #Check if the 'by' Value is a Column of 'data'
      stopifnot(any(private$.col_names == by))
      #title and decription
      
      self$description <- description
      self$title <- title
      
      
      #Data
      
      #Check if parameter are numerical
      if (any(sapply(data[, private$.col_names != by], Negate(is.numeric)))) {
        stop("Parametercolumns must be numerical!", call. = FALSE)
      }
      #save Parameters seperated from their classes
      private$.data <- data[, private$.col_names != by]
      #Print progress
      print(private$.data[1:5,])
      #save classvalues of parameters under '.results'
      private$.results <- data[, by]
      #save parameternames
      private$.parnames <- colnames(private$.data)
      #get vector of unique classes
      u_classes <- as.vector(unique(results))
      #print progress
      cat("\nClasses:\n")
      print(u_classes)
      
      #Numbers
      
      #save
      private$.classes <- u_classes
      #generate a vector with names for each class (for selections)
      private$.classnames <-
        make.names(as.character(u_classes), unique = TRUE)
      #set names for easyer selection
      names(private$.classes) <- private$.classnames
      #save number of parameters under dim
      private$.dim <- ncol(data) - 1
      #print progress
      cat(sprintf("\nNumber of Parameters: %s \n", private$.dim))
      #save number of unique classes
      private$.n_classes <- length(private$.classes)
      #print progress
      cat(sprintf("\nNumber of Classes: %s \n", private$.n_classes))
      
      private$.n_obs <- nrow(data)
      cat(sprintf("\nNumber of Observations: %s \n", private$.n_obs))
      private$.count <- table(private$.results)
      cat("\nObservations per Class:\n")
      print(private$.count)
      
      #Estimators
      # All as lists
      
      #pi
      #possibilitys in order of classes
      private$.pi <- sapply(private$.classnames, function(class) {
        private$.count[class] / private$.n_obs
      })
      cat("\nOdds:\n")
      private$.pi <- as.list(private$.pi)
      names(private$.pi) <- private$.classnames
      print(as.data.frame(private$.pi))
      
      #mean
      
      private$.mean <- lapply(private$.classes, function(class) {
        colMeans(private$.data[private$.results == class, ])
      })
      names(private$.mean) <- private$.classnames
      cat("\nMeans of Parameters:\n")
      print(as.data.frame(private$.mean))
      
      #totalmean
      
      private$.meantotal <- colMeans(private$.data)
      
      #sigma calculate later if needed
      
      sigma_list <- as.list(rep(NA, times = private$.n_classes))
      names(sigma_list) <- private$.classnames
      private$.sigma <- sigma_list
    },
    
    #custom print function
    print = function(...) {
      #print given title and description
      if (private$.title != "") {
        cat(sprintf("Title: %s \n", private$.title))
      }
      if (private$.description != "") {
        cat(sprintf("Description: %s \n", private$.description))
      }
      #print short overview
      cat("\nNumer of Observations per Class:")
      print(private$.count)
      print(private$.data[1:5,])
      invisible(self)
    }
    
  ),
  
  #Control
  activ = list(
    #Control every Value
    data = function(Value) {
      if (missing(Value)) {
        return(private$.data)
      }
      else{
        stop("data is read only", call. = FALSE)
      }
    },
    results = function(Value) {
      if (missing(Value)) {
        return(private$.results)
      }
      else{
        stop("results is read only", call. = FALSE)
      }
    },
    col_names = function(Value) {
      if (missing(Value)) {
        return(private$.col_names)
      }
      else{
        stop("col_names is read only", call. = FALSE)
      }
    },
    classes = function(Value) {
      if (missing(Value)) {
        return(private$.classes)
      }
      else{
        stop("classes is read only", call. = FALSE)
      }
    },
    classnames = function(Value) {
      if (missing(Value)) {
        return(private$.classnames)
      }
      else{
        stop("classnames is read only", call. = FALSE)
      }
    },
    n_classes = function(Value) {
      if (missing(Value)) {
        return(private$.n_classes)
      }
      else{
        stop("n_classes is read only", call. = FALSE)
      }
    },
    count = function(Value) {
      if (missing(Value)) {
        return(private$.count)
      }
      else{
        stop("count is read only", call. = FALSE)
      }
    },
    dim = function(Value) {
      if (missing(Value)) {
        return(private$.dim)
      }
      else{
        stop("dim is read only", call. = FALSE)
      }
    },
    n_obs = function(Value) {
      if (missing(Value)) {
        return(private$.n_obs)
      }
      else{
        stop("n_obs is read only", call. = FALSE)
      }
    },
    title = function(Value) {
      if (missing(Value)) {
        return(private$.title)
      }
      else{
        if (is.character(Value)) {
          if (length(Value) == 1) {
            private$.title <- Value
          }
          else{
            stop("must be length 1", call. = FALSE)
          }
        }
        else{
          stop("must be character", call. = FALSE)
        }
      }
    },
    description = function(Value) {
      if (missing(Value)) {
        return(private$.description)
      }
      else{
        if (is.character(Value)) {
          private$.description <- Value
        }
        else{
          stop("must be character", call. = FALSE)
        }
      }
    },
    pi = function(Value) {
      if (missing(Value)) {
        return(private$.pi)
      }
      else{
        stop("pi is read only", call. = FALSE)
      }
    },
    mean = function(Value) {
      if (missing(Value)) {
        return(private$.mean)
      }
      else{
        stop("mean is read only", call. = FALSE)
      }
    },
    meantotal = function(Value) {
      if (missing(Value)) {
        return(private$.meantotal)
      }
      else{
        stop("meantotal is read only", call. = FALSE)
      }
    },
    #calculate the covariance matrix of each class
    sigma = function(Value) {
      if (missing(Value)) {
        if (is.na(private$.sigma[1]) == FALSE) {
          return(private$.sigma)
        }
        else{
          cat("\nCalculating variances ...\n")
          n <- self$dim
          #calculate matrix for each class (iterate over names for subsetting)
          sapply(self$classnames, function(class) {
            #seperate parameter of specific class
            data <-self$data[self$results == self$classes[class], ]
            #get parameter means for this class
            mu <- self$mean[[class]]
            print(mu)
            # sum up variance of each observation
            array <- apply(data, 1, function(x) {
              x - mu
            })
            vec <- rowSums(array)
            print(vec)
            #save Matrix in already created private list
            private$.sigma[[class]] <- vec %o% vec / (self$count[class] - 1)
            #repeat
          })
          return(private$.sigma)
        }
      }
      else{
        stop("sigma is read only", call. = FALSE)
      }
    },
    #Between Classes Variance
    sigma_bet = function(Value) {
      if (missing(Value)) {
        if (is.na(private$.sigma[1]) == FALSE) {
          return(private$.sigma_bet)
        }
        else{
          G <- self$classnames
          mu <- self$mean
          total <- self$meantotal
          N <- self$n_classes
          B_i <- lapply(1:N, function(i) {
            self$count[G[i]] * (mu[[i]] - total) %o% (mu[[i]] - total)
          })
          private$.sigma_bet <- Reduce(`+`, B_i) / (self$n_obs-N)
        }
      }
      else{
        stop("sigma_bet is read only", call. = FALSE)
      }
    }
  )
)

data <- make_test()
problem <- make_set$new(data,
                        by = "class",
                        title = "Versuch",
                        description = "Warum nicht?")
problem$sigma[['A']]
#OOP Estimators
pi_est <- function(set) {
  stopifnot(class(set) == "Dataset")
  return(set$pi)
}
#'mue_est
#'
#'given a dataframe with Parameters of Observations and a second dataframe with the corresponding Classes
#'mu_es returns a Matrix with the mean vectors of the classes as rows.
#'
#'@param set Object of Class 'Dataset' see: \code{\link[graphics]{Problem}
#'@return A List with the mean vectors of the parameters for each class
mu_est <- function(set) {
  stopifnot(class(set) == "Dataset")
  return(set$mean)
}

#'sigma_class
#'
#'@param set Object of Class 'Dataset'
#'@param class Name of a specific Class of 'set'
#'@return The covariance matrix of the Class 'class'
#'
sigma_class <- function(set, class) {
  stopifnot(class(set) == "Dataset")
  stopifnot(any(set$classnames == class))
  return(set$sigma[[class]])
}

#'sigma_est
#'
#'@param set Object of Class 'Dataset'
#'@return a list of Covariance Matrices for each Class of 'set'
#'
sigma_est <- function(set) {
  stopifnot(class(set) == "Dataset")
  return(set$sigma)
}

sigma_bet_est <- function(problem) {
  stopifnot(class(set) == "Dataset")
  return(set$sigma_bet)
}
