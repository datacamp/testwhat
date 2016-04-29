context("test-predefined-objects")
source("helpers.R")

test_that("test_predefined_objects works", {
  lst <- list()
  lst$DC_SOLUTION <- "a <- 2\nb <- 3\nc <- c(x = 2)\nd <- 4"
  lst$DC_CODE <- "a <- 2\nb <- 3\nc <- 2"
  
  lst$DC_SCT <- "test_predefined_objects()"
  output <- test_it(lst)
  error(output)
  
  lst$DC_SCT <- "test_predefined_objects(c('a', 'b', 'c'))"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_predefined_objects(c('a', 'b', 'c'), eq_condition = 'equivalent')"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_predefined_objects(c('a', 'b', 'c'), eq_condition = c('equivalent', 'equivalent'))"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_predefined_objects(c('a', 'b', 'c'), eq_condition = c('equivalent', 'equivalent', 'equal'))"
  output <- test_it(lst)
  fails(output, "change the contents")
  
  lst$DC_SCT <- "test_predefined_objects(c('a', 'b', 'c', 'd'))"
  output <- test_it(lst)
  fails(output, "remove the predefined")
  
  lst$DC_SCT <- "test_predefined_objects(c('a', 'b', 'c'), eq_condition = c('equivalent', 'equivalent', 'equal'), eval = c(T, T, F))"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_predefined_objects(c('a', 'b', 'c'), eq_condition = 'equal', incorrect_msg = 'notok')"
  output <- test_it(lst)
  fails(output, 'notok')
  
  lst$DC_SCT <- "test_predefined_objects(c('a', 'b', 'c'), eq_condition = 'equal', incorrect_msg = c('anotok', 'bnotok', 'cnotok'))"
  output <- test_it(lst)
  fails(output, 'cnotok')
  
  lst$DC_SCT <- "test_predefined_objects(c('a', 'b', 'c', 'd'), undefined_msg = 'notok')"
  output <- test_it(lst)
  fails(output, 'notok')
  
  lst$DC_SCT <- "test_predefined_objects(c('a', 'b', 'c', 'd'), undefined_msg = c('anotok', 'bnotok', 'cnotok', 'dnotok'))"
  output <- test_it(lst)
  fails(output, 'dnotok')
  
})