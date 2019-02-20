context("test_oop")
set.seed(0)
test_that("test_make_test", {
  
  expect_equal(2.9237227, make_test(ninputs = 2)$b[4])
  expect_error(make_test(ninputs = 3,nclasses = "a"))  
  d <- matrix(1:9,nrow=3)
  expect_error(make_test(ninputs = d))  
  expect_error(make_test(ninputs = 0))
  expect_error(make_test(nclasses = -2))
  set.seed(0)
  expect_equal(make_test(sigma = 0)$a[1], make_test(sigma = 0)$a[1])
})


