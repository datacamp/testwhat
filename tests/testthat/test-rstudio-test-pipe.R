context("test_pipe")
source("helpers.R")

test_that("test_pipe works", {
  lst <- list()
  lst$DC_CODE <- "mean(abs(-5:6))"
  lst$DC_SCT <- "test_pipe(1)"
  output <- test_it(lst)
  fails(output)
  lst$DC_SCT <- "test_pipe(1, absent_msg = 'missing!')"
  output <- test_it(lst)
  fails(output, mess_patt = "missing!")
  
  lst$DC_CODE <- "-5:6 %>% abs() %>% mean()"
  lst$DC_SCT <- "test_pipe(1)"
  output <- test_it(lst)
  passes(output)
  lst$DC_SCT <- "test_pipe(2)"
  output <- test_it(lst)
  passes(output)
  lst$DC_SCT <- "test_pipe(3)"
  output <- test_it(lst)
  fails(output)
  lst$DC_SCT <- "test_pipe(3, insuf_msg = 'not enough man!')"
  output <- test_it(lst)
  fails(output, mess_patt = "not enough man!")
})