context("check_while_loop")

test_that("check_while - step by step", {
  lst <- list()
  lst$DC_SOLUTION <- "i <- 1\nwhile (i < 10) { print('test'); i <- i + 1 }"
  lst$DC_SCT <- "whileloop <- ex() %>% check_while()
                 whileloop %>% check_cond() %>% check_code('10')
                 whileloop %>% check_body() %>% check_function('print') %>% check_arg('x') %>% check_equal()"
  
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

test_that("check_while - step by step - custom", {
  lst <- list()
  lst$DC_SOLUTION <- "i <- 1\nwhile (i < 10) { print('test'); i <- i + 1 }"
  lst$DC_SCT <- "whileloop <- ex() %>% check_while(not_found_msg = 'notfound')
                 whileloop %>% check_cond() %>% check_code('10', missing_msg = 'nottyped')
                 whileloop %>% check_body() %>% check_function('print') %>% check_arg('x') %>% check_equal(incorrect_msg = 'incorr')"
                  
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

test_that("check_while - backwards compatibility", {
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


test_that("check_while - indexing", {
  # TODO
})


# TODO add messaging tests
test_that("check_while - nesting", {
  # TODO
})

test_that("check_while - test_ifelse inside", {
  # TODO  
})

test_that("check_while - highlighting", {
  # TODO
})

test_that("check_while - errs appropriately", {
  # TODO
})
