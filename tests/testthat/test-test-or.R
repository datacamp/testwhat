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

test_that("test_or works in 'content_testing_mode'", {
  lst <- list()
  lst$DC_CODE <- "a = 2; print(a); b = 3"
  lst$DC_SOLUTION <- "a = 2; print(a); b = 3; print(b)"
  lst$DC_SCT <- "test_or(test_output_contains('a'), test_output_contains('b'))"
  
  lst$DC_TEST_MODE <- FALSE
  capture.output(output <- test_it(lst))
  passes(output)
  
  lst$DC_TEST_MODE <- TRUE
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "testing mode")
})


