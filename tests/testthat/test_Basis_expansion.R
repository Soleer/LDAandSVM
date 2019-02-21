context("test_Basis_expansion")

test_that("basis_exp", {
  expect_error(basis_exp(2))
  expect_error(basis_exp(NA_character_))
  expect_equal("function", class(basis_exp("id")))
  expect_error(basis_exp("Wort"))
})
