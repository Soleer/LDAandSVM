#RDA test 
testRDA <- function() {
  N <- 5
  G<- 2
  test_set <- make_testset(N, G )
  validation_set <- make_testset(N, G)
  
  #TODO complete
  test_that("LDA equals RDA", {
    
    RDA1_function <- RDA(test_set, alpha = 1, gamma =1)$func
    print("")
    print("RDA1:")
    print(RDA1_function)
    
    LDA_function <- LDA(test_set)$func
    print(LDA_function)
    typeof(LDA_function)
    
    resultLDA <- apply(validation_set$data, 1, LDA_function)
    resultRDA1 <- apply(validation_set$data, 1, RDA1_function)
    
    expect_equivalent(RDA2_function(test_set$data), LDA_function(test_set$data))
    expect_equivalent(resultRDA1, resultLDA)
  })
  
  test_that("QDA equals RDA", { 
    RDA2_function <- RDA(test_set, alpha = 0, gamma = 1)$func
    QDA_function <- QDA(test_set)$func
    
    resultRDA1 <- RDA2_function(validation_set$data)
    resultQDA <- QDA_function(validation_set$data)
    
    expect_equivalent(RDA2_function(test_set$data), QDA_function(test_set$data))
    expect_equivalent(resultRDA1, resultQDA)
  })
  
  test_that("RDA better that LDA and QDA", {
    RDA3_function <- RDA(test_set)$func # uses cross validation 
    resultRDA3 <- RDA3_function(validation_set$data)
    
    expect_gte(resultRDA3, resultQDA)
    expect_gte(resultRDA3, resultLDA)
  })
}