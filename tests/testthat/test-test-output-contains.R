context("test_output_contains")
source("helpers.R")

test_that("basic test_output_contains", {
  lst <- list()
  lst$DC_CODE <- "print('some crazy output')"
  
  lst$DC_SCT <- "test_output_contains(\"print('some crazy output')\")"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_output_contains(\"print('some wrong output')\")"
  output <- test_it(lst)
  fails(output)
})

test_that("test_output_contains works with times argument", {
  lst <- list()
  lst$DC_CODE <- "print('some crazy output'); print('some crazy output'); print('some crazy output')"
  lst$DC_SCT <- "test_output_contains(\"print('some crazy output')\", times = 3)"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_output_contains(\"print('some crazy output')\", times = 4, incorrect_msg = \"NOOOO\")"
  output <- test_it(lst)
  fails(output, mess_patt = "NOOOO")
})

test_that("test_output_contains works in other cases", {
  lst <- list()
  lst$DC_CODE <- "x <- mtcars\nsummary(x)\nrm(x)"
  lst$DC_SCT <- "test_output_contains(\'summary(x)\')"
  output <- test_it(lst)
  fails(output)
})
