library(rlang)


projects <- c(
  "The set cover problem",
  #1
  "Scheduling jobs with deadlines",
  #2
  "Scheduling jobs on parallel machines",
  #3
  "The traveling salesman problem",
  #4
  "Uncapacitated facility location problem",
  #5
  "Hidden Markov Models",
  "Image de-noising",
  "Clustering",
  #8
  "Kernel smoother",
  "Kernel density estimation",
  #10
  "Kernel regression",
  "LASSO, Ridge and friends",
  "Linear regression",
  "Logistic regression",
  "Network",
  #15
  "Neural networks and Stochastic Gradient Descent",
  "Kalman and Particle Filters",
  "Time series",
  #18
  "Gaussian Process Regression",
  #19
  "OSE",
  #20
  "LDA and SVM",
  "Sampling Methods",
  #22
  "Image Segmentation"
)
wixxer <- list("benni", "daniel", "henning", "niklas")

#Beispiel:
niklas <- c(13, 14, 16, 18, 21)
henning <- c(16, 15, 8, 13, 22) #Test
benni <- c(3, 8, 15, 19, 21)

# Eintragen ---------------------------------------------------------------

print(projects)

daniel <- c()


# - -----------------------------------------------------------------------

getselection <- function(name) {
  var <- eval(parse_expr(name))
  if (is.null(var)) {
    return(sample(1:length(projects), 5))
  }
  return(var)
}

val <- function(selec, vec) {
  sapply(vec, function(x) {
    if (x %in% selec) {
      return(TRUE)
    }
    return(FALSE)
  })
}

evaluation <- function() {
  selection <- lapply(wixxer, getselection)
  liste <- unique(unlist(selection))
  results <-
    as.data.frame(sapply(selection, function(selec)
      val(selec, liste)))
  colnames(results) <- wixxer
  rownames(results) <- projects[liste]
  results
}

ergebnis <- function() {
  data <- evaluation()
  print("Überblick")
  print(data)
  bests <- as.data.frame(sort(rowSums(data), decreasing = TRUE))
  colnames(bests) <- c("Stimmen")
  print("Zusammenfassung")
  print(bests[bests[, 1] > 1, , drop = FALSE])
}

ergebnis()
