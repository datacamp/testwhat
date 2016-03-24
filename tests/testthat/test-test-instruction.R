context("test_instruction")
source("helpers.R")

test_that("test_instruction works", {
  lst <- list()
  lst$DC_TYPE <- "ChallengeExercise"
  lst$DC_CODE <- "x <- c(1,2,3); y <- 5; m <- mean(x+y); diff(m)"
  lst$DC_SOLUTION <- "x <- c(1,2,3); y <- 5; m <- mean(x+y); d <- diff(m); d"
  
  lst$DC_SCT <- "test_instruction(1, test_object(\"x\"))"
  output <- test_it(lst)
  error(output)
  
  lst$DC_SCT <- "test_instruction(1, test_object(\"x\")); test_instruction(2, test_object(\"y\"))"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_instruction(1, test_object(\"d\")); test_instruction(2, test_output_contains(\"diff(m)\"))"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_instruction(1, test_object(\"x\")); test_instruction(2, test_object(\"d\"))"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_instruction(1, test_output_contains(\"'dansen'\")); test_instruction(2, test_object(\"d\"))"
  output <- test_it(lst)
  fails(output)
})