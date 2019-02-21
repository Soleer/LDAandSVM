context("test_oop")
set.seed(0)
a <- runif(4)
d <- data.frame('a' = a[1:2],
                'b' = a[3:4],
                'class'  = c('A', 'B'))
b <-
  data.frame('a' = a[1:2],
             'b' = a[3:4],
             'NA_character_'  = c('A', 'B'))
char_mat <- matrix("a", nrow = 3, ncol = 3)
set <-
  make_set(
    data = d,
    by = "class",
    title = "title",
    description = "Description"
  )
m <- matrix(1:9, nrow = 3)

test_that("test_make_set", {
  expect_equal(c("data_set", "R6"), class(make_set(data = d, by = "class")))
  expect_error(make_set())
  expect_error(make_set(data = m, by = "abc"))
  expect_error(make_set(data = d, by = 'a'))
  expect_error(make_set(data = d))
  expect_error(make_set(data = d, by = "abc"))
  expect_error(make_set(data = d, by = "class", title = 0))
  expect_error(make_set(data = d, by = "class", title = char_mat))
  expect_error(make_set(data = d, by = "class", title = NA_character_))
  expect_warning(make_set(
    data = d,
    by = "class",
    title = "title",
    description = char_mat
  ))
  expect_null(set$func_names)
  expect_equal(list(), set$func_info)
  expect_equal("title", set$title)
  
})

test_that("is.data_set", {
  expect_true(is.data_set(set))
  expect_false(is.data_set(set$title))
  expect_false(is.data_set(class(set)))
  expect_false(is.data_set(d))
  expect_false(is.data_set(NULL))
  expect_false(is.data_set(NA))
  expect_false(is.data_set(set$clone))
  expect_true(is.data_set(set$clone()))
})

test_that("make_testset", {
  expect_equal(c("data_set", "R6"), class(make_testset()))
  expect_error(make_testset(1))
  expect_error(make_testset(-2))
  expect_equal("TEST", make_testset(c(2, NULL))$title)
})

LDA(set)
QDA(set)

test_that("data_set", {
  expect_equal(c("data_set", "R6"), class(make_testset()))
  expect_error(set$func_names <- char_mat)
  expect_error(set$func_names <- NA_character_)
  expect_error(set$title <- NA_character_)
  expect_error(set$change_func_name("LDA_1",NA_character_))
  expect_error(set$change_func_name("LDA_1",char_mat))
  expect_error(set$change_func_name("LDA_1","QDA_2"))
  expect_error(set$classes <- "w")
  expect_error(set$expansion(NA_character_))
  expect_error(set$set_function(set$func["LDA_1"]))
  expect_error(set$set_function(set$func["LDA_1"][[1]],"QDA_2",parameter = list(c(0,-20),30,"w")))
})

