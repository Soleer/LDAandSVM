library(rlang)


projects <- c(
  "The set cover problem",
  "Scheduling jobs with deadlines",
  "Scheduling jobs on parallel machines",
  "The traveling salesman problem",
  "Uncapacitated facility location problem",
  "Hidden Markov Models",
  "Image de-noising",
  "Clustering",
  "Kernel smoother",
  "Kernel density estimation",
  "Kernel regression",
  "LASSO, Ridge and friends",
  "Linear regression",
  "Logistic regression",
  "Network",
  "Neural networks and Stochastic Gradient Descent",
  "Kalman and Particle Filters",
  "Time series",
  "Gaussian Process Regression",
  "OSE",
  "LDA and SVM",
  "Sampling Methods",
  "Image Segmentation"
)
wixxer <- list("benni", "daniel", "henning", "niklas")

#Beispiel:
niklas <- c(13, 14, 16, 18, 21)
henning <- c(16, 15, 8, 13, 22) #Test

# Eintragen ---------------------------------------------------------------

print(projects)

benni <- c()
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
