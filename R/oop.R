#Basic R6-Object "data_set":


data_set <- R6Class(
  "data_set",
  private = list(
    #list of private slots
    .data = NULL,            #dataframe with parameters
    .data_expansion = list(),#list of dataframes for each parameter expansion
    .results = NULL,         #vector of classvalues for .data
    .col_names = NULL,       #colnames of data an results
    .classes = NULL,         #vector with all possible classvalues
    .classnames = NULL,      #vector of generated names for each class (used for subsetting)
    .parnames = NULL,        #vector of parameternames
    .n_classes = 0,          #number of different classes
    .count = 0,              #number of observations per class in a vector
    .dim = 0,                #number of parameters
    .n_obs = 0,              #number of observations ( nrow(.data))
    .title = "",             #title of data_set
    .description = "",       #description
    .pi = 0,                 #vector of probability of each class
    .mean = 0,               #list of parameter means of each class
    .meantotal = 0,          #parameter means over all classes
    .sigma = 0,              #list of covaraince matrices of each class
    .sigma_bet = NA,         #between class covariance matrix
    .n_func = 0,             #number of saved classification functions
    .function_list = list(), #list of already calculated classification functions 
    .function_info = list()  #list of corresponding parameters for each function
  ),
  public = list(
    
    #init function
#######################################################################################################    
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
      if(ncol(data)==1){
        stop("Data must have at least one parametercolumn", call. = FALSE)
      }
      
      self$description <- description
      self$title <- title
      
      
      #Data
      
      #Check if parameter are usable
      if (any(sapply(data[, private$.col_names != by], Negate(is.numeric)))) {
        stop("Parametercolumns must be numerical!", call. = FALSE)
      }
      if (any(sapply(data[, private$.col_names != by], is.na))) {
        stop("Parametercolumns contain NA Values!", call. = FALSE)
      }
      if (any(sapply(data[, private$.col_names != by], is.infinite))) {
        stop("Parametercolumns contain NA Values!", call. = FALSE)
      }
      
      #save Parameters seperated from their classes
      private$.data <- data[, private$.col_names != by]
      private$.data_expansion[['id']] <- private$.data
      #Print progress
      print(private$.data[1:5,])
      cat("...\n")
      #save classvalues of parameters under '.results'
      private$.results <- data[, by]
      
      private$.count <- table(private$.results)
      if(any(private$.count == 1)){
        stop("Every class must have at least two observations! Enter more data.", call. = FALSE)
      }
      cat("\nObservations per Class:\n")
      print(private$.count)
      
      #save parameternames
      private$.parnames <- colnames(private$.data)
      #get vector of unique classes
      u_classes <- as.vector(unique(private$.results))
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
#######################################################################################################
    #custom print function
    
print = function(...) {
      #print given title and description
      if (private$.title != "") {
        cat(sprintf("Title: %s \n", private$.title))
      }
      if (private$.description != "") {
        cat(sprintf("Description: %s \n", private$.description))
      }
      cat(sprintf("Number of classification Functions: %s \n", private$.n_func))
      #print short overview
      cat("\nNumer of Observations per Class:")
      print(private$.count)
      if (length(private$.data_expansion) > 0) {
        cat("\nExpansions:\n")
        print(names(private$.data_expansion))
      }
      cat("\nData:\n")
      print(private$.data[1:5,])
      invisible(self)
},
#######################################################################################################
    #add a classification function to the set
set_function = function(func, type, parameter) {
      if (typeof(func) != "closure") {
        stop("No valid Input", call. = FALSE)
      }
      else{
        stopifnot(length(type) == 1 && is.character(type))
        private$.n_func <- private$.n_func + 1
        name <- sprintf("%s_%s", type, private$.n_func)
        private$.function_list[[name]] <- func
        private$.function_info[[name]] <-
          list(name=name,type = type, parameter = parameter)
        return(invisible(list(name = name, func = func)))
      }
      
},
#######################################################################################################  
    #change name of a function in the set
change_func_name = function(from, to) {
      stopifnot(is.character(from) && is.character(to))
      if (!any(names(private$.function_list) == from)) {
        stop(sprintf("%s is no function name", from))
      }
      if (any(names(private$.function_list) == to)) {
        stop(sprintf("%s is already taken", to))
      }
      n <- names(private$.func_list)
      names(private$.func_list)[n==from] <- to
      names(private$.func_info)[n==from] <- to
      return(invisible(to))
},
#######################################################################################################
    #expand the data with a basisexpansion
expansion = function(base) {
      stopifnot(is.character(base))
      if (is.null(private$.data_expansion[[base]])) {    ##checks if expension already exists 
        h <-
          basis_exp(base)                                 ##Gets the basis expansion function
        private$.data_expansion[[base]] <- h(self$data)   ##calculates and adds expanded data
        return(private$.data_expansion[[base]])           
      }
      else{
        private$.data_expansion[[base]]                   #no calculating 
      }
},
####################################################################################################### 
    #get observations of one class
get_data_by_class = function(class){
      return(data[private$.results == self$classes[class],]) #TODO funzt nicht, ruft anderen datensatz ab
}
#######################################################################################################
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
    parnames = function(Value) {
      if (missing(Value)) {
        return(private$.parnames)
      }
      else{
        stop("parnames is read only", call. = FALSE)
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
    data_by_classes = function(Value) {
      #TODO access at k 
      if (missing(Value)) {
        return(private$.data_by_classes)
      }
      else{
        stop("results is read only", call. = FALSE)
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
#######################################################################################################
    #calculate the covariance matrix of each class
sigma = function(Value) {
      if (missing(Value)) {
        if (is.na(private$.sigma[1]) == FALSE) {
          return(private$.sigma)
        }
        else{
          n <- self$dim
          #calculate matrix for each class (iterating over classnames for subsetting)
          sapply(self$classnames, function(class) {
            rows <-
              self$data[self$results == self$classes[class], ]  #get rows of specific class
            mu <-
              self$mean[[class]]                               #get parameter means for this class
            Bn <- diag(0, ncol = n, nrow = n)
            apply(rows, 1, function(x) {
              Bn <<- Bn + tcrossprod(x - mu)
            })                                         #get row-means
            private$.sigma[[class]] <-
              Bn / (self$count[class] - 1)             #save Matrix in already created private list
            #repeat
          })
          return(private$.sigma)
        }
      }
      else{
        stop("sigma is read only", call. = FALSE)
      }
},
#######################################################################################################
    #Between Classes Variance
sigma_bet = function(Value) {
      if (missing(Value)) {
        if (is.na(private$.sigma_bet[1]) == FALSE) {
          return(private$.sigma_bet)
        }
        else{
          G <- self$classnames
          mu <- self$mean
          total <- self$meantotal
          N <- self$n_classes
          B_i <- lapply(1:N, function(i) {
            self$count[G[i]] * tcrossprod(mu[[i]] - total)
          })
          private$.sigma_bet <- Reduce(`+`, B_i) / (self$n_obs - N)
        }
      }
      else{
        stop("sigma_bet is read only", call. = FALSE)
      }
},
#######################################################################################################
    #access to all classificationfunctions in the set
func = function(Value) {
      if (missing(Value)) {
        return(private$.function_list)
      }
      else{
        stop("read only", call. = FALSE)
      }
    },
#######################################################################################################
    func_info = function(Value) {
      if (missing(Value)) {
        return(private$.function_info)
      }
      else{
        stop("read only", call. = FALSE)
      }
    },
#######################################################################################################
    func_names = function(Value){
      if (missing(Value)) {
        return(names(private$.function_list))
      }
      else{
        stop("read only", call. = FALSE)
      }
    }
  )
)

#######################################################################################################
#usefull functions

#'make_set
#'
#'\code{make_set} transforms a dataframe into an R6 object of class 'data_set'.
#'
#'@param data a Dataframe
#'@param by the name of a column of data to classify by
#'@param title optional title of the data
#'@param description optional description of the data
#'@return A data_set
#'@export
make_set <- function(data,
                     by,
                     title="",
                     description="") {
  
  data_set$new(data, by, title, description)
}
#'is.data_set
#'
#'Checks if the input is an Object of class data_set.
#'@param set An R Object
#'@return TRUE if set is a data_set, else FALSE
#'@examples
#'is.data_set(NULL)
#'@export
is.data_set <- function(set) {
  any(class(set) == "data_set")
}

#'make_testset
#'
#'Creates a simple data_set with random data. 
#'
#'@param N number of observations per class
#'@param K number of classes
#'@param P number of parameters of each observation
#'@return a data_set
#'@examples
#'set <- make_testset(N = 50, K= 2)
#'@export
make_testset <- function(N = 10, K = 3, P = 2) {
  test <- make_test(ninputs = N, nclasses = K, nparam = P)
  set <-
    make_set(test,
             by = "class",
             title = "TEST",
             description = "Testset")
  return(set)
}
