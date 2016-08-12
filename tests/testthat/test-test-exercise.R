context("test_exercise")

test_that("basic exercise", {
  lst <- list()
  lst$DC_CODE <- "x <- 4"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "test_object(\"x\")"
  output <- test_it(lst)
  passes(output)

  lst <- list()
  lst$DC_CODE <- "x <- 5"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "test_object(\"x\")"
  output <- test_it(lst)
  fails(output)
})

test_that("msg outside of testwhat call", {
  lst <- list()
  lst$DC_CODE <- "x <- 4"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "msg <- \"incorrect_obj\"\ntest_object(\"x\", incorrect_msg = msg)"
  output <- test_it(lst)
  passes(output)

  lst <- list()
  lst$DC_CODE <- "x <- 5"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "msg <- \"incorrect_obj\"\ntest_object(\"x\", incorrect_msg = msg)"
  output <- test_it(lst)
  fails(output)
})

test_that("msg outside subSCT using call", {
  lst <- list()
  lst$DC_CODE <- "x <- 4"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "msg <- \"incorrect_obj\"\ntest_correct(test_object(\"x\", incorrect_msg = msg), test_student_typed(\"4\"))"
  output <- test_it(lst)
  passes(output)
  
  lst <- list()
  lst$DC_CODE <- "x <- 5"
  lst$DC_SOLUTION <- "x <- 4"
  lst$DC_SCT <- "msg <- \"incorrect_obj\"\ntest_correct(test_object(\"x\", incorrect_msg = msg), test_student_typed(\"4\"))"
  output <- test_it(lst)
  fails(output)
})
