context("test_mc")
source("helpers.R")

test_that("test_mc works works", {
  lst <- list()
  lst$DC_TYPE <- "MultipleChoiceExercise"
  lst$DC_CODE <- "DM.result <- 1"
  
  lst$DC_SCT <- "test_mc(1, feedback_msgs = c('this is the CORRECT answer', 'this is the WRONG answer'))"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_mc(2, feedback_msgs = c('this is the WRONG answer', 'this is the CORRECT answer'))"
  output <- test_it(lst)
  fails(output, mess_patt = "this is the WRONG answer")
})