context("test_output_regex")
source("helpers.R")

test_that("test_output_regex works", {
  lst <- list()
  lst$DC_ECHO <- TRUE
  lst$DC_CODE <- "'testing'\n123"
  
  lst$DC_SCT <- "test_output_regex('123')"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_output_regex('\\\\d{3,}')"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_output_regex('\\\\d{4,}')"
  output <- test_it(lst)
  fails(output, 'contain the pattern')
  
  lst$DC_SCT <- "test_output_regex('\\\\d{4,}', incorrect_msg = 'not correct')"
  output <- test_it(lst)
  fails(output, 'not correct')
  
  lst$DC_SCT <- "test_output_regex('123', fixed = TRUE)"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_output_regex('1234', fixed = TRUE)"
  output <- test_it(lst)
  fails(output, 'contain the pattern')
  
  lst$DC_SCT <- "test_output_regex('1234', fixed = TRUE, incorrect_msg = 'not correct')"
  output <- test_it(lst)
  fails(output, 'not correct')
  
  lst$DC_SCT <- "test_output_regex('testing', times = 2)"
  output <- test_it(lst)
  fails(output, 'contain the pattern')
  
  lst$DC_SCT <- "test_output_regex('\\\\[1\\\\] testing', fixed = TRUE, times = 2)"
  output <- test_it(lst)
  fails(output, 'contain the pattern')
  
  lst$DC_SCT <- "test_output_regex('testing', times = 2, incorrect_msg = 'not correct')"
  output <- test_it(lst)
  fails(output, 'not correct')
})

test_that("test_output_regex works 2", {
  lst <- list()
  lst$DC_ECHO <- TRUE
  
  # below code gives error: Error: non-numeric argument to binary operator
  lst$DC_CODE <- "'test' + 3" 
  
  lst$DC_SCT <- "test_output_regex('non-numeric')"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_output_regex('argument', times = 2)"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_output_regex('argument', times = 2, incorrect_msg = 'not correct')"
  output <- test_it(lst)
  fails(output, 'not correct')
})
