context("test_plot_functions")

set <- make_testset()
test_that("maincomponent_analysis", {
  expect_equal("list",class(maincomponent_analysis(set)))
  expect_equal("matrix",class(maincomponent_analysis(set)[[1]]))
  expect_true(is.double(maincomponent_analysis(set)[[1]]))
})
m <- matrix(1:9,nrow = 3)
test_that("make_projection", {
  expect_equal("list",class(make_projection(set)))
  expect_equal("function",class(make_projection(set)[[1]]))
  expect_error(make_projection(set,-2))
  expect_error(make_projection(set,3))
  expect_warning(make_projection(set,m))
  expect_equivalent(0,make_projection(set,0)[[1]](c(0,0)))
})

test_that("make_2D_plot", {
  expect_error(make_2D_plot(set))
  QDA(set)
  expect_error(make_2D_plot(set,"QDA"))
})

test_that("plot_summary", {
    
})