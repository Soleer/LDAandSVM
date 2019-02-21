context("test_Classifier_funs")
set.seed(0)
set <- make_testset()
lda <- LDA(set)
qda <- QDA(set)
pda <- PDA(set)
svm <- SVM(set)
rda <- RDA(set, alpha = 0, gamma = 1) #ohne cross validation, feste parameter. sollte fuer alpha und gamma zwischen 0 und 1 funktionieren
a <- runif(4)
d <- data.frame('a' = a[1:2],
                'b' = a[3:4],
                'class'  = c('A', 'B'))
char_mat <- matrix("a", nrow = 2, ncol = 2)
mat <- matrix(0,nrow=20,ncol=3)
for (i in 1:set$n_obs) {
  print(asdf(as.double(set$data[i,])))
}


test_that("LDA", {
  expect_error(LDA(d))
  expect_error(LDA(NA))
  expect_error(LDA(NULL))
  expect_equal("LDA_1", lda$name)
  expect_equal(c(A="A"), lda$func(c(0,0)))
  expect_equal("function",class(lda$func))
  
})

test_that("QDA", {
  expect_error(QDA(d))
  expect_error(QDA(NA))
  expect_error(QDA(NULL))
  expect_equal("QDA_2", qda$name)
  expect_equal(c(C="C"), qda$func(c(0,0)))
  expect_equal("function",class(qda$func))
  
})
test_that("PDA", {
  expect_error(PDA(d))
  expect_error(PDA(NA))
  expect_error(PDA(NULL))
  expect_error(PDA(NULL))
  expect_error(PDA(set,"idquad"))
  expect_error(PDA(set,"cube",char_mat))
  expect_error(PDA(set,"cube",NULL))
  expect_error(PDA(set,"cube",-2))
  expect_error(PDA(set,"id",char_mat))
  expect_error(PDA(set,"log",mat))
  expect_error(PDA(set,"abs",d))
  expect_equal("PDA_3", pda$name)
  expect_equal(c(C="C"), pda$func(c(0,0)))
  expect_equal("function",class(pda$func))
  
})

test_that("SVM", {
  expect_error(SVM(d))
  expect_error(SVM(NA))
  expect_error(SVM(NULL))
  expect_error(SVM(NULL))
  expect_error(SVM(set,"C"))
  expect_error(SVM(set,char_mat,"poly"))
  expect_error(SVM(set,NULL))
  expect_error(SVM(set,NA))
  expect_error(SVM(set,-2))
  expect_error(SVM(set,1,"poly",NULL,NULL))
  expect_error(SVM(set,1,"poly",NA,NA))
  
  expect_error(SVM(set,"log",mat))
  expect_error(SVM(set,"abs",d))
  expect_equal("SVM_4", svm$name)
  expect_equal(c(A="A"), svm$func(c(0,0)))
  expect_equal("function",class(svm$func))
})
#################### work in progress!!!
