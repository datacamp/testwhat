context("test_while_loop")
source("helpers.R")

test_that("test_while - step by step", {
  lst <- list()
  lst$DC_SOLUTION <- "i <- 1\nwhile (i < 10) { print('test'); i <- i + 1 }"
  lst$DC_SCT <- "whileloop <- ex() %>% test_while()
                 whileloop %>% test_cond() %>% test_code('10')
                 whileloop %>% test_body() %>% test_fun('print') %>% test_arg('x') %>% test_equal()"
  
  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Are you sure you coded one while statement")
  
  lst$DC_CODE <- "i <- 1\nwhile (i < 9) { i <- i + 1 }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the first while statement")
  fails(output, mess_patt = "Check the condition")
  
  lst$DC_CODE <- "i <- 1\nwhile (i < 10) { print('abc'); i <- i + 1 }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the first while statement")
  fails(output, mess_patt = "Check the body")
  fails(output, mess_patt = "Check your call of <code>print\\(\\)</code>")
  fails(output, mess_patt = "Did you correctly specify the argument <code>x</code>")
  
  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})


test_that("test_while - step by step - custom", {
  lst <- list()
  lst$DC_SOLUTION <- "i <- 1\nwhile (i < 10) { print('test'); i <- i + 1 }"
  lst$DC_SCT <- "whileloop <- ex() %>% test_while(not_found_msg = 'notfound')
                 whileloop %>% test_cond() %>% test_code('10', not_typed_msg = 'nottyped')
                 whileloop %>% test_body() %>% test_fun('print') %>% test_arg('x') %>% test_equal(incorrect_msg = 'incorr')"
                  
  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Notfound")
  
  lst$DC_CODE <- "i <- 1\nwhile (i < 9) { i <- i + 1 }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the first while statement")
  fails(output, mess_patt = "Check the condition")
  fails(output, mess_patt = "Nottyped")
  
  lst$DC_CODE <- "i <- 1\nwhile (i < 10) { print('abc'); i <- i + 1 }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the first while statement")
  fails(output, mess_patt = "Check the body")
  fails(output, mess_patt = "Check your call of <code>print\\(\\)</code>")
  fails(output, mess_patt = "Incorr")
  
  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})

test_that("test_while - backwards compatibility", {
  lst <- list()
  lst$DC_SOLUTION <- "i <- 1\nwhile (i < 10) { print('test'); i <- i + 1 }"
  lst$DC_SCT <- "test_while_loop(cond_test = test_student_typed('10'), expr_test = test_function('print', 'x'))"
  
  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Are you sure you coded one while statement")
  
  lst$DC_CODE <- "i <- 1\nwhile (i < 9) { i <- i + 1 }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the first while statement")
  fails(output, mess_patt = "Check the condition")
  
  lst$DC_CODE <- "i <- 1\nwhile (i < 10) { print('abc'); i <- i + 1 }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the first while statement")
  fails(output, mess_patt = "Check the body")
  fails(output, mess_patt = "Check your call of <code>print\\(\\)</code>")
  fails(output, mess_patt = "Did you correctly specify the argument <code>x</code>")
  
  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})


test_that("test_while - indexing", {
  # TODO
})


# TODO add messaging tests
test_that("test_while - nesting", {
  # TODO
})

test_that("test_while - test_ifelse inside", {
  # TODO  
})

test_that("test_while - highlighting", {
  # TODO
})

test_that("test_while - errs appropriately", {
  # TODO
})




# test_that("test_while works in basic form", {
#   lst <- list()
#   lst$DC_PEC <- "i <- 1"
#   lst$DC_CODE <- "while (i < 10) {\n    rpois(10,i)\n    i = i + 1\n  }"
#   lst$DC_SOLUTION <- "while (i < 3) {\n    rnorm(10,i)\n    i = i + 1\n  }"
#   lst$DC_SCT <- "test_while_loop()"
#   output <- test_it(lst)
#   passes(output)
#   
#   lst <- list()
#   lst$DC_PEC <- "i <- 1"
#   lst$DC_CODE <- ""
#   lst$DC_SOLUTION <- "while (i < 3) {\n    rnorm(10,i)\n    i = i + 1\n  }"
#   lst$DC_SCT <- "test_while_loop()"
#   output <- test_it(lst)
#   fails(output)
# })
# 
# test_that("test_while with cond_test works", {
#   lst <- list()
#   lst$DC_PEC <- "i <- 1"
#   lst$DC_CODE <- "while (i < 10) {\n    rpois(10,i)\n    i = i + 1\n  }"
#   lst$DC_SOLUTION <- "while (i < 10) {\n    rpois(10,i)\n    i = i + 1\n  }"
#   lst$DC_SCT <- "test_while_loop(cond_test = test_student_typed('i < 10'))"
#   output <- test_it(lst)
#   passes(output)
#   lst$DC_CODE <- "while (i < 9) {\n    rpois(10,i)\n    i = i + 1\n  }"
#   output <- test_it(lst)
#   fails(output)
# })
#   
# test_that("test_while_loop with expr_test works", {
#   lst <- list()
#   lst$DC_PEC <- "i <- 1"
#   lst$DC_CODE <- "while (i < 10) {\n    rpois(10,i)\n    i = i + 1\n  }"
#   lst$DC_SOLUTION <- "while (i < 10) {\n    rpois(10,i)\n    i = i + 1\n  }"
#   lst$DC_SCT <- "test_while_loop(expr_test = test_function('rpois', args = 'n'))"
#   output <- test_it(lst)
#   passes(output)
#   lst$DC_SCT <- "test_while_loop(expr_test = test_function('rpois', args = c('n', 'lambda')))"
#   output <- test_it(lst)
#   passes(output)
#   
#   lst <- list()
#   lst$DC_PEC <- "i <- 1"
#   lst$DC_CODE <- "while (i < 10) {\n    rpois(9,i)\n    i = i + 1\n  }"
#   lst$DC_SOLUTION <- "while (i < 10) {\n    rpois(10,i)\n    i = i + 1\n  }"
#   lst$DC_SCT <- "test_while_loop(expr_test = test_function('rpois', args = 'n'))"
#   output <- test_it(lst)
#   fails(output)
#   line_info(output, 2, 2)
#   
#   lst <- list()
#   lst$DC_PEC <- "i <- 1"
#   lst$DC_CODE <- "while (i < 10) {\n    rpois(10,i)\n    i = i + 1\n  }"
#   lst$DC_SOLUTION <- "while (i < 10) {\n    rpois(10,i)\n  rnorm(10, i)\n  i = i + 1\n  }"
#   lst$DC_SCT <- "test_while_loop(expr_test = test_function('rpois', args = 'n'))"
#   output <- test_it(lst)
#   passes(output)
#   lst$DC_CODE <- "while (i < 10) {\n    rpois(9,i)\n    i = i + 1\n  }"
#   output <- test_it(lst)
#   fails(output)
#   line_info(output, 2, 2)
# })
# 
# test_that("test_while_loop works with multiple while loops", {
#   lst <- list()
#   lst$DC_CODE <- "i = 3\n  n = 3\n  while (i < 8) {\n    rpois(2,i)\n    i = i + 1\n  }\n  a <- \"some code here\"\n  while (n < 5) {\n    rnorm(5, n*n)\n    n = n + 1\n  }"
#   lst$DC_SOLUTION <- "i = 1\n  n = 3\n  while (i < 10) {\n    rpois(10,i)\n    i = i + 1\n  }\n  while (n < 5) {\n    rnorm(5, n*n)\n    n = n + 1\n  }\nx <- 2\nwhile(x > 0) { print(x); x <- x - 1 } \n"
#   lst$DC_SCT <-  "test_while_loop(2, cond_test = test_student_typed('n < 5'), expr_test = test_function(\"rnorm\", c(\"n\")))"
#   output <- test_it(lst)
#   passes(output)
#   
#   lst$DC_SCT <- "test_while_loop(1, expr_test = test_function(\"rpois\", c(\"n\", \"lambda\")))"
#   output <- test_it(lst)
#   fails(output)
#   line_info(output, 4, 4)
#   
#   lst$DC_SCT <- "test_while_loop(3, not_found_msg = \"Too much\")"
#   output <- test_it(lst)
#   fails(output, mess_patt = "Too much")
# })
# 
# test_that("test_while works with if else inside", {
#   lst <- list()
#   lst$DC_SOLUTION <- paste0("speed <- 64\nwhile (speed > 30) {\n  print(paste(\"Your speed is\",speed))\n  if (speed > 48)",
#                             "{\n    print(\"Slow down big time!\")\n    speed <- speed - 11\n  } else {\n    print(\"Slow down!\")\n    speed <- speed - 6\n  }\n}")
#   lst$DC_CODE <- lst$DC_SOLUTION
#   lst$DC_SCT <- paste("test_while_loop(index = 1, ",
#                       "cond_test = test_student_typed(c(\"speed > 30\", \"30 < speed\")),",
#                       "expr_test = test_if_else(index = 1,",
#                       "if_cond_test = test_student_typed(c(\"speed > 48\", \"48 < speed\")),",
#                       "if_expr_test = test_function(\"print\", \"x\"),",
#                       "else_expr_test = test_function(\"print\", \"x\")",
#                       "))", sep = "\n")
#   
#   output <- test_it(lst)
#   passes(output)
# })
