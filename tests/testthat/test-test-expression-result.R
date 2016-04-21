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

test_that("test_expression_result works with erroneous code", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- 5"
  lst$DC_CODE <- "x <- 5\nrm(x)"
  
  lst$DC_SCT <- "test_expression_result('class(non_existing)')"
  output <- test_it(lst)
  error(output)
  
  lst$DC_SCT <- "test_expression_result('class(x)')"
  output <- test_it(lst)
  fails(output)
})

test_that("test_expression_result works with eq_condition equal", {
  lst <- list()
  lst$DC_SOLUTION <- "func <- function(x) { return(list(a = 1, b = 2)) }"
  lst$DC_SCT <- "test_expression_result(\"func(3)\", eq_condition = \"equal\")"
  
  lst$DC_CODE <- "func <- function(x) { return(list(1, 2)) }"
  output <- test_it(lst)
  fails(output, "list\\(a = 1, b = 2\\)")
  fails(output, "Instead, got: <code>list\\(1, 2\\)")
  
  lst$DC_CODE <- "func <- function(x) { return(list(a = 1, b = 2)) }"
  output <- test_it(lst)
  passes(output)
})

test_that("test_expression_result works with eq_condition equal", {
  lst <- list()
  lst$DC_SOLUTION <- "func <- function(x) { return(data.frame(a = c(1,2), b = c(3,4))) }"
  lst$DC_SCT <- "test_expression_result(\"func(3)\", eq_condition = \"equal\")"
  
  lst$DC_CODE <- "func <- function(x) { return(data.frame(c(1, 2), c(3, 4))) }"
  output <- test_it(lst)
  fails(output, "data.frame\\(a = c\\(1, 2\\), b = c\\(3, 4\\)\\)")

  lst$DC_CODE <- "func <- function(x) { return(data.frame(a = c(1, 2), b = c(3, 4))) }"
  output <- test_it(lst)
  passes(output)
})