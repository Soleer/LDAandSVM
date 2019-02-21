context("test_Calc_error")
set.seed(0)
set <- make_testset()
QDA(set)
LDA(set)
a <- runif(4)
d <- data.frame('a' = a[1:2],
                'b' = a[3:4],
                'class'  = c('A', 'B'))
char_mat <- matrix("a", nrow = 3, ncol = 3)

test_that("calc_error", {
  expect_equal("list", class(calc_error(set, "QDA_1")))
  expect_equal("data.frame",class(calc_error(set,"LDA_2")[[1]]))
  expect_equal(0,calc_error(set,"QDA_1")$miss)
  expect_equal(0,calc_error(set,"LDA_2")$miss)
  expect_error(calc_error(set,"SVM"))
  expect_error(calc_error(set,"SVM"))
  expect_error(calc_error(set,char_mat))
  expect_error(calc_error(set))
  expect_error(calc_error(d,"LDA_2"))
  expect_error(calc_error(set$clone,"QDA_1"))
  expect_equal("list", class(calc_error(set$clone(), "QDA_1")))
  expect_error(calc_error(set,set$func_names))
  expect_error(calc_error(set,"QDA_1",char_mat))
})

