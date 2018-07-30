context("test_mc")

test_that("check_mc works", {
  lst <- list()
  lst$DC_TYPE <- "MultipleChoiceExercise"
  lst$DC_CODE <- "DM.result <- 2"
  
  lst$DC_SCT <- "ex() %>% check_mc(2, feedback_msgs = c('this is the WRONG answer', 'this is the CORRECT answer'))"
  output <- test_it(lst)
  passes(output, mess_patt = "This is the CORRECT answer")
  
  lst$DC_SCT <- "ex() %>% check_mc(2)"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "ex() %>% check_mc(1, feedback_msgs = c('this is the CORRECT answer', 'this is the WRONG answer'))"
  output <- test_it(lst)
  fails(output, mess_patt = "This is the WRONG answer")
  
  lst$DC_SCT <- "ex() %>% check_mc(1)"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "ex() %>% check_mc(1, feedback_msgs = c('not enugh messages'))"
  expect_error(test_it(lst))
  
  lst <- list()
  lst$DC_TYPE <- "MultipleChoiceExercise"
  lst$DC_CODE <- "not.DM.result <- 12"
  lst$DC_SCT <- "ex() %>% check_mc(1)"
  output <- test_it(lst)
  fails(output, mess_patt = "Please select one of the options!")
  
  lst <- list()
  lst$DC_TYPE <- "MultipleChoiceExercise"
  lst$DC_CODE <- "DM.result <- 2"
  lst$DC_SCT <- "ex() %>% check_mc(c(1, 2))"
  output <- test_it(lst)
  passes(output)
})

test_that("test_mc works", {
  lst <- list()
  lst$DC_TYPE <- "MultipleChoiceExercise"
  lst$DC_CODE <- "DM.result <- 2"
  
  lst$DC_SCT <- "test_mc(2, feedback_msgs = c('this is the WRONG answer', 'this is the CORRECT answer'))"
  output <- test_it(lst)
  passes(output, mess_patt = "This is the CORRECT answer")
  
  lst$DC_SCT <- "test_mc(2)"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_mc(1, feedback_msgs = c('this is the CORRECT answer', 'this is the WRONG answer'))"
  output <- test_it(lst)
  fails(output, mess_patt = "This is the WRONG answer")
  
  lst$DC_SCT <- "test_mc(1)"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_mc(1, feedback_msgs = c('not enugh messages'))"
  expect_error(test_it(lst))
  
  lst <- list()
  lst$DC_TYPE <- "MultipleChoiceExercise"
  lst$DC_CODE <- "not.DM.result <- 12"
  lst$DC_SCT <- "test_mc(1)"
  output <- test_it(lst)
  fails(output, mess_patt = "Please select one of the options!")
  
  lst <- list()
  lst$DC_TYPE <- "MultipleChoiceExercise"
  lst$DC_CODE <- "DM.result <- 2"
  lst$DC_SCT <- "test_mc(c(1, 2))"
  output <- test_it(lst)
  passes(output)
})