context("test_expression_output")
source("helpers.R")

test_that("test_expression_output works", {
  lst <- list()
  lst$DC_CODE <- "func <- function(x) { print(x) }\nfunc_not_eq <- function(x) { print(x); x^2}"
  lst$DC_SOLUTION <- "func <- function(x) { print(x) }\nfunc_not_eq <- function(x) { print(paste(\"Output:\",x)) }"
  
  lst$DC_SCT <- "test_expression_output(\"func(3)\")"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_expression_output(\"func_not_eq(3)\")"
  output <- test_it(lst)
  fails(output, mess_patt = "Make sure that.*outputs.*\\[1\\] 3")
  
})
  

test_that("test_expression_output works 2", {
  lst <- list()
  lst$DC_CODE <- "func <- function(x) { print(\"Test: \"); print(x); return(invisible(x)) }"
  lst$DC_SOLUTION <- "func <- function(x) { print(\"Test: \"); print(x); return(invisible(x - 1)) }"
  
  lst$DC_SCT <- "test_expression_output(\"func(3)\")"
  output <- test_it(lst)
  passes(output)
  
})

test_that("test_expression_output works with NULL", {
  lst <- list()
  lst$DC_SOLUTION <- "func <- function(x) { print(NULL) }"
  lst$DC_SCT <- "test_expression_output(\"func(3)\")"
  
  lst$DC_CODE <- "func <- function(x) { print(NULL) }"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "func <- function(x) { print(\"NULL\") }"
  output <- test_it(lst)
  fails(output, mess_patt = "got:<br><code>\\[1\\] &quot;NULL")
})

test_that("test_expression_output works if broken", {
  lst <- list()
  lst$DC_CODE <- "func <- function(x) { print(\"Test: \"); y <- wrong_stuff }"
  lst$DC_SOLUTION <- "func <- function(x) { print(\"Test: \"); y <- wrong_stuff }"
  
  lst$DC_SCT <- "test_expression_output(\"func(3)\")"
  output <- test_it(lst)
  error(output)
  
  lst$DC_SOLUTION <- "func <- function(x) { print(\"Test: \"); y <- \"ok_stuff\" }"
  output <- test_it(lst)
  fails(output, mess_patt = "resulted in the following error")
})
