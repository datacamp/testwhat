context("test_for_loop")
source("helpers.R")

test_that("test_for works in basic form", {
  lst <- list()
  lst$DC_CODE <- "for (i in 1:10) {\n    rpois(10,i)\n  }"
  lst$DC_SOLUTION <- "for (i in 1:3) {\n    rnorm(10,i)\n  }"
  lst$DC_SCT <- "test_for_loop()"
  output <- test_it(lst)
  passes(output)
  
  lst <- list()
  lst$DC_CODE <- ""
  lst$DC_SOLUTION <- "for (i in 1:3) {\n    rnorm(10,i)\n  }"
  lst$DC_SCT <- "test_for_loop()"
  output <- test_it(lst)
  fails(output)
})

test_that("test_for works with cond_test", {
  lst <- list()
  lst$DC_CODE <- "for (i in 1:10) {\n    rpois(10,i)\n  }"
  lst$DC_SOLUTION <- "for (i in 1:10) {\n    rpois(10,i)\n  }"
  lst$DC_SCT <- "test_for_loop(cond_test = test_student_typed('i in 1:10'))"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_for_loop(cond_test = test_student_typed('rettketet'))"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_for_loop(cond_test = test_student_typed('retteketet', not_typed_msg = 'retteketet not found'))"
  output <- test_it(lst)
  fails(output, mess_patt = "retteketet not found")
})

test_that("test_for works with expr_test", {
  lst <- list()
  lst$DC_CODE <- "for (i in 1:10) {\n    rpois(10,i)\n  }"
  lst$DC_SOLUTION <- "for (i in 1:10) {\n    rpois(10,i)\n  }"
  lst$DC_SCT = "test_for_loop(expr_test = test_function('rpois', args = c('n', 'lambda')))"
  output <- test_it(lst)
  passes(output)
  
  lst <- list()
  lst$DC_CODE <- "for (i in 1:10) {\n    rpois(3,i)\n  }"
  lst$DC_SOLUTION <- "for (i in 1:10) {\n    rpois(10,i)\n    rnorm(10,i)\n  }"
  lst$DC_SCT = "test_for_loop(expr_test = test_function('rpois', args = c('n', 'lambda')))"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT = "test_for_loop(expr_test = test_function('rpois', args = c('n', 'lambda'), incorrect_msg = 'wrong'))"
  output <- test_it(lst)
  fails(output, mess_patt = "wrong")
  
  lst$DC_SCT = "test_for_loop(expr_test = test_function(\"rnorm\"))"
  output <- test_it(lst)
  fails(output)
})

test_that("test_for works with indexes", {
  lst <- list()
  lst$DC_CODE <- "for (i in 3:8) {\n    rpois(2,i)\n  }\n  a <- \"some code here\"\n  for (n in 3:5) {\n    rnorm(5, n*n)\n  }"
  lst$DC_SOLUTION <- "for (i in 1:10) {\n    rpois(10,i)\n  }\n  for (n in 3:5) {\n    rnorm(5, n*n)\n  }\n\nfor(x in 1:5) print(x)"
  
  lst$DC_SCT <- "test_for_loop(2, cond_test = test_object(\"n\"), expr_test = test_function(\"rnorm\", c(\"n\")))"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "test_for_loop(1, expr_test = test_function(\"rpois\", c(\"n\", \"lambda\")))"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_SCT <- "test_for_loop(3, not_found_msg = \"Too much looooooops\")"
  output <- test_it(lst)
  fails(output, mess_patt = "Too much looooooops")
})
