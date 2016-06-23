context("test_or")
source("helpers.R")

test_that("test_or works", {
  lst <- list()
  lst$DC_CODE <- "b = 3; c = 5"
  lst$DC_SOLUTION <- "a = 2.5; b = 3; c = 29"
  
  lst$DC_SCT <- "test_or(test_object('a'), test_object('b'), test_object('c'))"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_or(test_object('a'), test_object('c'))"
  output <- test_it(lst)
  fails(output, mess_patt = ".*define .*a")
  
  lst$DC_SCT <- "test_or(test_object('a'), test_object('c'), incorrect_msg = 'incorrect, my man')"
  output <- test_it(lst)
  fails(output, mess_patt = "incorrect, my man")

})

test_that("test_or throws error if something is off", {
  lst <- list()
  lst$DC_CODE <- "summary(mtcars)"
  lst$DC_SOLUTION <- "summary(mtcars); str(mtcars)"
  
  lst$DC_SCT <- "test_or(test_function('summary', 'object'), test_function('str', 'object'))"
  capture.output(output <- test_it(lst))
  passes(output)

  # argument of second test function incorrect
  lst$DC_SCT <- "test_or(test_function('summary', 'object'), test_function('str', 'x'))"
  capture.output(output <- test_it(lst))
  error(output)
})


