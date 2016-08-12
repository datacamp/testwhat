context("test_while_loop")

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
