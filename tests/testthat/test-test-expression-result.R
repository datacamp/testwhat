context("test_expression_result")
source("helpers.R")

test_that("test_expression_result works", {
  lst <- list()
  lst$DC_CODE <- "func <- function(x) { x + 3 }\nfunc_not_eq <- function(x) { x + 4 }"
  lst$DC_SOLUTION <- "func <- function(x) { x + 3 }\nfunc_not_eq <- function(x) { x + 3 }"
  
  lst$DC_SCT <- "test_expression_result(\"func(3)\")"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_expression_result(\"func_not_eq(3)\")"
  output <- test_it(lst)
  fails(output, mess_patt = "Make sure that.*returns.*")
  
})

test_that("test_expression_result works 2", {
  lst <- list()
  lst$DC_CODE <- "func <- function(x) { x / 3 }"
  lst$DC_SOLUTION <- "func <- function(x) { x + 3 }"
  
  lst$DC_SCT <- "test_expression_result(\"func(3)\")"
  output <- test_it(lst)
  fails(output)

})

test_that("test_expression_result works with NULL", {
  lst <- list()
  lst$DC_SOLUTION <- "func <- function(x) { return(NULL) }"
  lst$DC_SCT <- "test_expression_result(\"func(3)\")"
  
  lst$DC_CODE <- "func <- function(x) { return(NULL) }"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_CODE <- "func <- function(x) { return(\"NULL\") }"
  output <- test_it(lst)
  fails(output, mess_patt = "Make sure that running.*returns <code>NULL</code>.*Instead, got: <code>")
})
