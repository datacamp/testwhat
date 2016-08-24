context("diff")

test_that("test diff general", {
  expect_error(build_diff())

  # no class-specific implementation
  expect_equal(build_diff(sol = list('a'), stud = 'blergh'), "")
  
  # stud is try-error
  expect_equal(build_diff(sol = 'anything', stud = tryerrorstring), "evaluating the expression you specified caused an error.")
  
  # if class-specific implementation: need more info!
  expect_error(build_diff(sol = "test"), stud = 'blergh')
  expect_error(build_diff(sol = "test", stud = TRUE))
  expect_error(build_diff(sol = "test", stud = TRUE, eq_condition = 'test'))
  expect_error(build_diff(sol = "test", stud = TRUE, eq_condition = 'equivalent'))
  res <- build_diff(sol = "test", stud = TRUE, eq_condition = 'equivalent', id = 'test')
  expect_equal(res, "test is a logical, while it should be a character string.")
})

test_that("test diff helpers", {
  expect_true(same_length(c(1, 2, 3), c(4, 5, 6)))
  expect_false(same_length(c(1, 2), c(5, 6, 6)))
  expect_equal(diff_length(c(1, 2), c(3, 5, 6), id = 'test'),
               "test has length 3, while it should have length 2.")

  expect_true(same_dim(data.frame(a = c(1, 2)), data.frame(a = c(1, 2))))
  expect_false(same_dim(data.frame(a = c(1, 2)), data.frame(a = c(1, 2, 3))))

  expect_equal(diff_dim(data.frame(a = c(1, 2)), data.frame(a = c(1, 2, 3)), id = 'test'),
               "test has 3 rows and 1 column, while it should have 2 rows and 1 column.")
  expect_equal(diff_dim(data.frame(a = 1, b = 2), data.frame(a = 1, b = 2, c = 3), id = 'test'),
               "test has 1 row and 3 columns, while it should have 1 row and 2 columns.")

  expect_equal(klass('x'), "character")
  expect_equal(klass(dplyr::tbl_df(mtcars)), "tbl_df/tbl/data.frame")

  # tests for same_type and diff_type
  expect_true(same_type(1, 2))
  expect_false(same_type("test", 2))
  expect_equal(diff_type("test", 2, 'test'), "test is a number, while it should be a character string.")
  expect_equal(diff_type(c("test", "test"), 2, 'test'), "test is a number, while it should be a character vector.")
  expect_equal(diff_type("test", c(1, 2), 'test'), "test is a numeric vector, while it should be a character string.")
  
  # test for same_class and diff_class
  expect_true(same_class(dplyr::tbl_df(mtcars), dplyr::tbl_df(mtcars)))
  expect_false(same_class(data.table::data.table(mtcars), dplyr::tbl_df(mtcars)))
  expect_equal(diff_class(data.table::data.table(mtcars), dplyr::tbl_df(mtcars), 'test'),
               "test is of class `tbl_df/tbl/data.frame`, while it should be of class `data.table/data.frame`.")

  # tests for same_attr and diff_attr
  expect_true(same_attr(c(a = 2), c(a = 3)))
  expect_false(same_attr(c(a = 2), c(b = 3)))
  expect_equal(diff_attr(c(a = 2, c(b = 3)), id = 'test'),
               "are you sure the attributes (names, class, etc.) of test are correct?")
})

test_that("test diff default", {
  expect_equal(get_diff(list(a = 2)), NULL)
})

test_that("test diff for logicals", {
  expect_equal(get_diff(TRUE, "x", "equivalent", "test"), "test is a character string, while it should be a logical.")
  expect_equal(get_diff(c(T, F), "x", "equivalent", "test"), "test is a character string, while it should be a logical vector.")
  expect_equal(get_diff(TRUE, c("x", "y"), "equivalent", "test"), "test is a character vector, while it should be a logical.")
  
  x <- TRUE
  class(x) <- c("anything", class(x))
  y <- FALSE
  class(y) <- c("anything2", class(y))
  expect_equal(get_diff(x, y, "equivalent", "test"),
               "test is of class `anything2/logical`, while it should be of class `anything/logical`.")
  expect_equal(get_diff(c(T, T), c(T, T, T), "equivalent", "test"),
               "test has length 3, while it should have length 2.")
  expect_equal(get_diff(c(a = T, b = T), c(a = T, c = T), "equivalent", "test"), NULL)
  expect_equal(get_diff(c(a = T, b = T), c(a = T, c = T), "equal", "test"),
               "are you sure the attributes (names, class, etc.) of test are correct?")
})


test_that("test diff for numerics", {
  expect_equal(get_diff(3, "x", "equivalent", "test"), "test is a character string, while it should be a number.")
  expect_equal(get_diff(c(1, 3), "x", "equivalent", "test"), "test is a character string, while it should be a numeric vector.")
  expect_equal(get_diff(3, c("x", "y"), "equivalent", "test"), "test is a character vector, while it should be a number.")
  
  x <- 3
  class(x) <- c("anything", class(x))
  y <- 3
  class(y) <- c("anything2", class(y))
  expect_equal(get_diff(x, y, "equivalent", "test"),
               "test is of class `anything2/numeric`, while it should be of class `anything/numeric`.")
  expect_equal(get_diff(c(2, 2), c(2, 2, 2), "equivalent", "test"),
               "test has length 3, while it should have length 2.")
  expect_equal(get_diff(c(a = 2, b = 2), c(a = 2, c = 2), "equivalent", "test"), NULL)
  expect_equal(get_diff(c(a = 2, b = 2), c(a = 2, c = 2), "equal", "test"),
               "are you sure the attributes (names, class, etc.) of test are correct?")
})


test_that("test diff for characters", {
  expect_equal(get_diff("x", 3, "equivalent", "test"), "test is a number, while it should be a character string.")
  expect_equal(get_diff(c("x", "y"), 3, "equivalent", "test"), "test is a number, while it should be a character vector.")
  expect_equal(get_diff("x", c(1, 3), "equivalent", "test"), "test is a numeric vector, while it should be a character string.")

  x <- 'x'
  class(x) <- c("anything", class(x))
  y <- 'y'
  class(y) <- c("anything2", class(y))
  expect_equal(get_diff(x, y, "equivalent", "test"),
               "test is of class `anything2/character`, while it should be of class `anything/character`.")
  expect_equal(get_diff(c('a', 'b'), c('a', 'b', 'c'), "equivalent", "test"),
               "test has length 3, while it should have length 2.")
  expect_equal(get_diff(c(a = 'x', b = 'x'), c(a = 'x', c = 'x'), "equivalent", "test"), NULL)
  expect_equal(get_diff(c(a = 'x', b = 'x'), c(a = 'x', c = 'x'), "equal", "test"),
               "are you sure the attributes (names, class, etc.) of test are correct?")

  expect_equal(get_diff("This is a test", "this is a test", "equivalent", "test"), "note that R is case-sensitive!")
  expect_equal(get_diff("This is a test", "This isatest", "equivalent", "test"), "make sure to use the correct spacing!")
  expect_equal(get_diff("This is a test!", "This is a test?", "equivalent", "test"), "make sure to use the correct punctuation marks!")
  expect_equal(get_diff("This is a test!", "This is a tester!", "equivalent", "test"), "there might be a typo in there.")
  expect_equal(get_diff("This is a test!", "Total randomness!", "equivalent", "test"), NULL)
})

test_that("test diff for data.frames", {
  expect_equal(get_diff(mtcars, 3, "equivalent", "test"), "test is a number, while it should be a data frame.")

  x <- mtcars
  class(x) <- c("anything", class(x))
  y <- mtcars[1:2, ]
  class(y) <- c("anything2", class(y))
  expect_equal(get_diff(x, y, "equivalent", "test"),
               "test is of class `anything2/data.frame`, while it should be of class `anything/data.frame`.")
  expect_equal(get_diff(mtcars, mtcars[1, ], "equivalent", "test"),
               "test has 1 row and 11 columns, while it should have 32 rows and 11 columns.")
  stud <- mtcars
  names(mtcars) <- paste0("__", names(mtcars))
  expect_equal(get_diff(mtcars, stud, "equivalent", "test"), NULL)
  expect_equal(get_diff(mtcars, stud, "equal", "test"),
               "are you sure the attributes (names, class, etc.) of test are correct?")
})


