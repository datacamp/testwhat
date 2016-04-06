context("test_function_result")
source("helpers.R")
# test_dir('tests/testthat', filter = 'test-function-result')

test_that("test_function_result fails correctly", {
  lst <- list()
  lst$DC_PEC <- "library(dplyr)"
  lst$DC_SOLUTION <- "mtcars %>% summarise(avg = mean(mpg), max = max(mpg))"
  
  lst$DC_CODE <- "mtcars %>% filter(mpg > 20)"
  lst$DC_SCT <- "test_function_result('summarise')"
  output <- test_it(lst)
  fails(output)
  lst$DC_SCT <- "test_function_result('summarise', not_called_msg = 'not_found')"
  output <- test_it(lst)
  fails(output, mess_patt = "not_found")
  
  lst$DC_CODE <- "mtcars %>% summarise(min = min(mpg), max = max(non_existing))"
  lst$DC_SCT <- "test_function_result('summarise')"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1)
  lst$DC_SCT <- "test_function_result('summarise', eval_error_msg = 'gave_error')"
  output <- test_it(lst)
  fails(output, mess_patt = 'gave_error')
  line_info(output, 1, 1)
  
  lst$DC_CODE <- "mtcars %>% summarise(min = min(mpg), max = max(mpg))"
  lst$DC_SCT <- "test_function_result('summarise')"
  output <- test_it(lst)
  fails(output)
  line_info(output, 1, 1)
  lst$DC_SCT <- "test_function_result('summarise', incorrect_msg = 'not_correct')"
  output <- test_it(lst)
  fails(output, mess_patt = 'not_correct')
  line_info(output, 1, 1)
})

test_that("test_function_result passes with different orders", {
  lst <- list()
  lst$DC_PEC <- "library(dplyr)"
  lst$DC_SOLUTION <- "mtcars %>% summarise(avg = mean(mpg), max = max(mpg))"
  lst$DC_SCT <- "test_function_result('summarise', ordered = FALSE)"
  
  lst$DC_CODE <- "mtcars %>% summarise(avg = mean(mpg), max = max(mpg))"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "mtcars %>% summarise(max = max(mpg), avg = mean(mpg))"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "summarise(mtcars, max = max(mpg), avg = mean(mpg))"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_function_result('summarise', ordered = TRUE)"
  
  lst$DC_CODE <- "mtcars %>% summarise(max = max(mpg), avg = mean(mpg))"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "summarise(mtcars, max = max(mpg), avg = mean(mpg))"
  output <- test_it(lst)
  fails(output)
})

