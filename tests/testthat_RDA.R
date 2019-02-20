#library(testthat)
#source("../R/Calc_error.R")

#RDA test 
#TODO delete 

library(ggplot2)
library(gridExtra)
library(quadprog)
library(R6)
library(MASS)
library(NlcOptim)
library(e1071)
library(shiny)
library(NlcOptim)
library(shiny)
library(rlang)
source("R/Basis_expansion.R")
source("R/Calc_error.R")
source("R/Estimators.R")
source("R/oop.R")
source("R/plot_functions.R")
source("R/RDA_crossvalidation.R")
source("R/shinyplot.R")
source("R/svm.R")
source("R/Test.R")
source("R/testthat.R")

testRDA()

testRDA <- function() {
  N <- 2
  G<- 3
  P<- 2
  test_set <- make_testset(N, G, P )
  validation_set <- make_testset(N, G, P)
  
  #TODO complete
  #test_that("RDA works at all", {
    RDA_object <- RDA(test_set$clone())
    RDA_function <- RDA_object$func
    print(RDA_function)
    
  #})
  # 
  # test_that("RDA has valuable results, works with maximum failure rate", {
  #   maximum_failure <- 0.5
  #   RDA_function <- RDA(test_set$clone())$func # uses cross validation 
  #   
  #   calc_errorRDA3Train <- calc_miss(test_set$data , test_set$results,RDA3_function)
  #   calc_errorRDA3Val <- calc_miss(validation_set$data , validation_set$results,RDA3_function)
  #   
  # 
  #   #RDA better in training
  #   expect_lte(calc_errorRDA3Train, calc_errorQDATrain)
  #   
  #   #RDA better in validation
  #   expect_lte(calc_errorRDA3Val, maximum_failure)
  #   
  #   #Training performance better than validation
  #   expect_lte(calc_errorRDA3Train, calc_errorRDA3Val)
  #   
  # })
  # 
  # 
  # LDA_function <- LDA(test_set)$func
  # test_that("LDA equals RDA", {
  #   
  #   RDA1_function <- RDA(test_set$clone(), alpha = 1, gamma =1)$func
  #  
  #   print(LDA_function)
  #   typeof(LDA_function)
  #   
  #   print(validation_set$data)
  #   typeof(validation_set$data)
  #   
  #   resultLDAVal <- apply(validation_set$data, 1, LDA_function)
  #   
  #   print(resultLDAVal)
  #   typeof(resultLDAVal)
  #   
  #   resultRDA1Val <- apply(validation_set$data, 1, RDA1_function)
  #   
  #   print(resultRDA1Val)
  #   typeof(resultRDA1Val)
  #   
  #   resultLDATrain <- apply(test_set$data, 1, LDA_function)
  #   resultRDA1Train <- apply(test_set$data, 1, RDA1_function)
  #   
  #   expect_equivalent(resultRDA1Train, resultLDATrain)
  #   expect_equivalent(resultRDA1Val, resultRDA1Val)
  # })
  # 
  # #QDA
  # QDA_function <- QDA(test_set)$func
  # test_that("QDA equals RDA", { 
  #   RDA2_function <- RDA(test_set$clone(), alpha = 0, gamma = 1)$func
  # 
  #   resultQDAVal <- apply(validation_set$data, 1, QDA_function)
  #   resultRDA2Val <- apply(validation_set$data, 1, RDA2_function)
  #   
  #   resultQDATrain <- apply(test_set$data, 1, QDA_function)
  #   resultRDA2Train <- apply(test_set$data, 1, RDA2_function)
  #   
  #   expect_equivalent(resultRDA2Train, resultQDATrain)
  #   expect_equivalent(resultQDAVal, resultRDA2Val)
  # })
  # 
  # #Test performance win
  # test_that("RDA better than LDA and QDA", {
  #   RDA3_function <- RDA(test_set$clone())$func # uses cross validation 
  # 
  #   calc_errorRDA3Train <- calc_miss(test_set$data , test_set$results,RDA3_function)
  #   calc_errorRDA3Val <- calc_miss(validation_set$data , validation_set$results,RDA3_function)
  #   
  #   calc_errorQDATrain <- calc_miss(test_set$data , test_set$results,QDA_function)
  #   calc_errorQDAVal <- calc_miss(validation_set$data , validation_set$results,QDA_function)
  #   
  #   calc_errorLDATrain <- calc_miss(test_set$data , test_set$results, LDA_function)
  #   calc_errorLDAVal <- calc_miss(validation_set$data , validation_set$results,LDA_function)
  #   
  #   #Training performance better than validation
  #   expect_lte(calc_errorRDA3Train, calc_errorRDA3Val)
  #   
  #   #RDA better in training
  #   expect_lte(calc_errorRDA3Train, calc_errorQDATrain)
  #   expect_lte(calc_errorRDA3Train, calc_errorLDATrain)
  #   
  #   #RDA better in validation
  #   expect_lte(calc_errorRDA3Val, calc_errorQDAVal)
  #   expect_lte(calc_errorRDA3Val, calc_errorLDAVal)
  # })
}

testSmallSigma <- function(){
  N <- 5
  G<- 3
  test_set <- make_testset(N, G)
  print(test_set)
  small_sigma_est(test_set)
  
  test_that("smallSigma makes sense", {
    small_sigma_est(test_set)
    #TODO 
  })
}

