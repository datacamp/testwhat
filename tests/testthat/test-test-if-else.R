context("test_if_else")
source("helpers.R")

test_that("test_if - step by step", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- 4\nif (x > 3) { print('x is big') } else { print('x is small') }"
  lst$DC_SCT <- "ifelse <- ex() %>% test_ifelse()
                 ifelse %>% test_cond() %>% test_code('>')
                 ifelse %>% test_if() %>% test_fun('print') %>% test_arg('x') %>% test_equal()
                 ifelse %>% test_else() %>% test_fun('print') %>% test_arg('x') %>% test_equal()"
  
  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "x <- 4\nif (x < 3) { }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the condition of the first if statement")
  
  lst$DC_CODE <- "x <- 4\nif (x > 3) { print('x is small') }"
  fails(output, mess_patt =  "Check the body of the first if statement")
  
  lst$DC_CODE <- "x <- 4\nif (x > 3) { print('x is big') }"
  output <- test_it(lst)
  fails(output, mess_patt = "The else part of the first if statement is missing")
  
  lst$DC_CODE <- "x <- 4\nif (x > 3) { print('x is big') } else { print('x is big') }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the else part of the first if statement")
  
  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})

test_that("test_if - step by step - custom", {
  
})

# test_that("test_if - step by step - backwards compatible", {
#   lst <- list()
#   lst$DC_SOLUTION <- "x <- 4\nif (x > 3) { print('x is big') } else { print('x is small') }"
#   lst$DC_SCT <- "test_if_else(if_cond_test = test_student_typed('>'), if_expr_test = test_function('print'), else_expr_test = test_function('print'))"
#   
#   lst$DC_CODE <- ""
#   output <- test_it(lst)
#   fails(output)
#   
#   lst$DC_CODE <- "x <- 4\nif (x < 3) { print('x is small') }"
#   output <- test_it(lst)
#   fails(output)
#   
#   lst$DC_CODE <- "x <- 4\nif (x > 3) { print('x is small') }"
#   output <- test_it(lst)
#   fails(output)
#   
#   lst$DC_CODE <- "x <- 4\nif (x > 3) { print('x is big') }"
#   output <- test_it(lst)
#   fails(output)
#   
#   lst$DC_CODE <- "x <- 4\nif (x > 3) { print('x is big') } else { print('x is big') }"
#   output <- test_it(lst)
#   fails(output)
#   
#   lst$DC_CODE <- lst$DC_SOLUTION
#   output <- test_it(lst)
#   passes(output)  
# })



















# test_that("test_if works in basic form", {
#   lst <- list()
#   lst$DC_CODE <- "a = 3\nif (a == 3) { print('equal') } else { print('not equal') }"
#   lst$DC_SOLUTION <- "a = 3\nif (a == 3) { print('equal') } else { print('not equal') }"
#   
#   lst$DC_SCT <- "test_if_else(if_cond_test = { test_student_typed(\"a == 3\") })"
#   output <- test_it(lst)
#   passes(output)
#   
#   lst$DC_SCT <- "test_if_else(if_expr_test = { test_function(\"print\", \"x\") })"
#   output <- test_it(lst)
#   passes(output)
#   
#   lst$DC_SCT <- "test_if_else(else_expr_test = { test_function(\"print\", \"x\") })"
#   output <- test_it(lst)
#   passes(output)
#   
#   lst$DC_SCT <- "test_if_else(if_cond_test = test_student_typed(\"a == 3\"), if_expr_test = test_function(\"print\", \"x\"), else_expr_test = test_function(\"print\", \"x\"))"
#   output <- test_it(lst)
#   passes(output)
#   
#   
#   lst <- list()
#   lst$DC_CODE <- "a = 4\nif (a == 4) { print('not equal') } else { \nprint('equal') }"
#   lst$DC_SOLUTION <- "a = 3\nif (a == 3) { print('equal') } else { \nprint('not equal') }"
#   
#   lst$DC_SCT <- "test_if_else(if_cond_test = { test_student_typed(\"a == 3\") })"
#   output <- test_it(lst)
#   fails(output)
#   
#   lst$DC_SCT <- "test_if_else(if_expr_test = { test_function(\"print\", \"x\") })"
#   output <- test_it(lst)
#   fails(output)
#   line_info(output, 2, 2)
#   
#   lst$DC_SCT <- "test_if_else(else_expr_test = { test_function(\"print\", \"x\") })"
#   output <- test_it(lst)
#   fails(output)
#   line_info(output, 3, 3)
#   
#   lst$DC_SCT <- "test_if_else(if_cond_test = { test_student_typed(\"a == 3\") },if_expr_test = { test_function(\"print\", \"x\") },else_expr_test = { test_function(\"print\", \"x\") })"
#   output <- test_it(lst)
#   fails(output)
# 
# })
# 
# 
# test_that("test_if_else handles missing ifs, elses and indexing correctly", {
#   lst <- list()
#   lst$DC_CODE <- "a = 4\nif (a == 4) { print('not equal') }"
#   lst$DC_SOLUTION <- "a = 3\nif (a == 3) { print('equal') } else { print('not equal') }\nif (3 == 3) { invisible() }"
#   
#   lst$DC_SCT <- "test_if_else(else_expr_test = { test_function(\"print\", \"x\") }, missing_else_msg = \"NO ELSE\" )"
#   output <- test_it(lst)
#   fails(output, mess_patt = "NO ELSE")
#   
#   lst$DC_SCT <- "test_if_else(2, not_found_msg = \"NO IF\")"
#   output <- test_it(lst)
#   fails(output, mess_patt = "NO IF")
#   
#   lst <- list()
#   lst$DC_CODE <- "a = 3\nif (3 == 1) { print('visible') }\nif (a == 3) { print('equal') }"
#   lst$DC_SOLUTION <- "a = 3\nif (3 == 3) { invisible() }\nif (a == 3) { print('equal') } else { print('not equal') }\nif(x <- 5) print('hustling')\n"
#   
#   lst$DC_SCT  <- "test_if_else(4,if_cond_test = test_student_typed(\"a == 3\"),if_expr_test = test_function(\"print\", \"x\"), else_expr_test = test_function(\"print\", \"x\"), not_found_msg = \"NO IF\")"
#   output <- test_it(lst)
#   error(output, mess_patt = "itself")
#   
#   lst$DC_SCT <- "test_if_else(3, if_cond_test = test_student_typed(\"a == 3\"), if_expr_test = test_function(\"print\", \"x\"), not_found_msg = \"NO IF\")"
#   output <- test_it(lst)
#   fails(output, mess_patt = "NO IF")
#   
#   lst$DC_SCT  <- "test_if_else(1, else_expr_test = { test_function(\"print\", \"x\") }, missing_else_msg = \"NO ELSE\" )"
#   output <- test_it(lst)
#   error(output, mess_patt = "itself")
#   
#   lst$DC_SCT  <- "test_if_else(2, else_expr_test = { test_function(\"print\", \"x\") }, missing_else_msg = \"NO ELSE\" )"
#   output <- test_it(lst)
#   fails(output, mess_patt = "NO ELSE")
# })
# 
# 
# test_that("test_if_else works with nesting", {
#   lst <- list()
#   lst$DC_CODE <- "a = 3\nif (a == 3) { print('equal') } else if (a == 4) { print('not equal') } else { print('zever, geen gezever') }"
#   lst$DC_SOLUTION <- "a = 3\nif (a == 3) { print('equal') } else if (a == 4) { print('not equal') } else { print('zever, gezever') }"
#   
#   lst$DC_SCT  <- "test_if_else(if_cond_test = { test_student_typed(\"a == 3\") },
#                             if_expr_test = { test_function(\"print\", \"x\") },
#                             else_expr_test = { 
#                             test_if_else(if_cond_test = { test_student_typed(\"a == 4\") },
#                             if_expr_test = { test_function(\"print\", \"x\") },
#                             else_expr_test = { test_function(\"print\")})})"
#   output <- test_it(lst)
#   passes(output)
#   
#   lst$DC_SCT  <- "test_if_else(if_cond_test = { test_student_typed(\"a == 3\") },
#                             if_expr_test = { test_function(\"print\", \"x\") },
#                             else_expr_test = { 
#                             test_if_else(if_cond_test = { test_student_typed(\"a == 3\") },
#                             if_expr_test = { test_function(\"print\", \"x\") },
#                             else_expr_test = { test_function(\"print\", \"x\")})})"
#   output <- test_it(lst)
#   fails(output)
#   
#   lst <- list()
#   lst$DC_CODE <- "a <- 3\nif(a == 3) { print('equal') } else { if (a == 4) { print('not equal') } else { print('zever, geen gezever') } }"
#   lst$DC_SOLUTION <- "a <- 3\nif(a == 3) { print('equal') } else { if (a == 4) { print('not equal') } else { print('zever, geen gezever') } }"
#   lst$DC_SCT  <- "test_if_else(if_cond_test = { test_student_typed(\"a == 3\") },
#                             if_expr_test = { test_function(\"print\", \"x\") },
#                             else_expr_test = { 
#                             test_if_else(if_cond_test = { test_student_typed(\"a == 4\") },
#                             if_expr_test = { test_function(\"print\", \"x\") },
#                             else_expr_test = { test_function(\"print\")})})"
#   output <- test_it(lst)
#   passes(output)
# })
# 
# test_that("test_if_else with diagnostics inside", {
#   lst <- list()
#   lst$DC_SOLUTION <- "if(TRUE) { print(\"test\") }"
#   lst$DC_CODE <- "if(TRUE) { print(123) }"
#   lst$DC_SCT <- "test_if_else(if_expr_test = test_function('print', 'x'))"
# 
#   output <- test_it(lst)
#   fails(output, mess_patt = "The object you specified is a number, while it should be a character string")
# 
#   lst$DC_SCT <- "test_if_else(if_expr_test = test_function('print', 'x', incorrect_msg = 'test'))"
#   output <- test_it(lst)
#   fails(output, mess_patt = 'test')
# })
