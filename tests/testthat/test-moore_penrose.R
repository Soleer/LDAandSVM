context("test-moore_penrose")
A <- matrix(c(1,2,2,4))
Aplus <- moore_penrose(A)
test_that("multiplication works", {
  expect_equal(A%*%Aplus%*%A, A)
  expect_equal(Aplus%*%A%*%Aplus,Aplus)
  expect_true(isSymmetric(A%*%Aplus))
  expect_true(isSymmetric(Aplus%*%A))
})
