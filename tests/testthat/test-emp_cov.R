context("test-emp_cov")
test <-
  c(
    c(1, 2, 3, 4),
    c(1, 2, 3, 4),
    c(1, 2, 3, 4),
    1:100,
    c(1, 2, 3, 4),
    c(4, 3, 2, 1),
    c(4, 2, 1, 1),
    rep(0, 100)
  )
test_that("multiplication works", {
  lapply(test, function(x) expect_equal(emp_cov(x), emp_cov(x)))
})
