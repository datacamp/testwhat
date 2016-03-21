context("test_mc")
source("helpers.R")

test_that("test_mc works", {
  lst <- list()
  lst$DC_TYPE <- "MultipleChoiceExercise"
  lst$DC_CODE <- "DM.result <- 2"
  
  lst$DC_SCT <- "test_mc(2, feedback_msgs = c('this is the WRONG answer', 'this is the CORRECT answer'))"
  output <- test_it(lst)
  passes(output, mess_patt = "this is the CORRECT answer")
  
  lst$DC_SCT <- "test_mc(2)"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_mc(1, feedback_msgs = c('this is the CORRECT answer', 'this is the WRONG answer'))"
  output <- test_it(lst)
  fails(output, mess_patt = "this is the WRONG answer")
  
  lst$DC_SCT <- "test_mc(1)"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_mc(1, feedback_msgs = c('not enugh messages'))"
  output <- test_it(lst)
  error(output)
  
  lst <- list()
  lst$DC_TYPE <- "MultipleChoiceExercise"
  lst$DC_CODE <- "not.DM.result <- 12"
  lst$DC_SCT <- "test_mc(1)"
  output <- test_it(lst)
  error(output)
  
  lst <- list()
  lst$DC_TYPE <- "MultipleChoiceExercise"
  lst$DC_CODE <- "DM.result <- 2"
  lst$DC_SCT <- "test_mc(c(1, 2))"
  output <- test_it(lst)
  passes(output)
})