context("test_for_loop")

test_that("check_for - step by step", {
  lst <- list()
  lst$DC_SOLUTION <- "for (i in 1:10) { print('test') }"
  lst$DC_SCT <- "forloop <- ex() %>% check_for()
                 forloop %>% check_cond() %>% check_code('10')
                 forloop %>% check_body() %>% check_function('print') %>% check_arg('x') %>% check_equal()"

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Are you sure you coded one for statement")

  lst$DC_CODE <- "for (i in 1:5) { print('abc') }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the first for statement")
  fails(output, mess_patt = "Check the condition")

  lst$DC_CODE <- "for (i in 1:10) { print('abc') }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the first for statement")
  fails(output, mess_patt = "Check the body")
  fails(output, mess_patt = "Check your call of <code>print\\(\\)</code>")
  fails(output, mess_patt = "Did you correctly specify the argument <code>x</code>")

  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})


test_that("check_for - step by step - custom", {
  lst <- list()
  lst$DC_SOLUTION <- "for (i in 1:10) { print('test') }"
  lst$DC_SCT <- "forloop <- ex() %>% check_for(not_found_msg = 'notfound')
                 forloop %>% check_cond() %>% check_code('10', missing_msg = 'nottyped')
                 forloop %>% check_body() %>% check_function('print') %>% check_arg('x') %>% check_equal(incorrect_msg = 'incorrect')"

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Notfound")

  lst$DC_CODE <- "for (i in 1:5) { print('abc') }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the first for statement")
  fails(output, mess_patt = "Check the condition")
  fails(output, mess_patt = "Nottyped")

  lst$DC_CODE <- "for (i in 1:10) { print('abc') }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the first for statement")
  fails(output, mess_patt = "Check the body")
  fails(output, mess_patt = "Check your call of <code>print\\(\\)</code>")
  fails(output, mess_patt = "Incorrect")

  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})

test_that("check_for - backwards compatibility", {
  lst <- list()
  lst$DC_SOLUTION <- "for (i in 1:10) { print('test') }"
  lst$DC_SCT <- "test_for_loop(index = 1, cond_test = test_student_typed('10'), expr_test = test_function('print', 'x'))"
  
  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Are you sure you coded one for statement")
  
  lst$DC_CODE <- "for (i in 1:5) { print('abc') }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the first for statement")
  fails(output, mess_patt = "Check the condition")

  lst$DC_CODE <- "for (i in 1:10) { print('abc') }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the first for statement")
  fails(output, mess_patt = "Check the body")
  fails(output, mess_patt = "Check your call of <code>print\\(\\)</code>")
  fails(output, mess_patt = "Did you correctly specify the argument <code>x</code>")

  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})


test_that("check_for - indexing", {
  lst <- list()
  lst$DC_SOLUTION <- "for (i in 1:10) { print('test') }\nfor (i in 1:5) { print('abc') }"
  lst$DC_SCT <- "forloop <- ex() %>% check_for(2)
                 forloop %>% check_cond() %>% check_code('5')
                 forloop %>% check_body() %>% check_function('print') %>% check_arg('x') %>% check_equal()"
  
  lst$DC_CODE <- "for (i in 1:10) {}"
  output <- test_it(lst)
  fails(output, mess_patt = "Are you sure you coded two for statements")
  
  lst$DC_CODE <- "for (i in 1:10) {}\nfor (i in 1:10) { print('test') }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the second for statement")
  fails(output, mess_patt = "Check the condition")
  
  lst$DC_CODE <- "for (i in 1:10) {}\nfor (i in 1:5) { print('test') }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the second for statement")
  fails(output, mess_patt = "Check the body")
  fails(output, mess_patt = "Check your call of <code>print\\(\\)</code>")
  fails(output, mess_patt = "Did you correctly specify the argument <code>x</code>")
  
  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})


# TODO add messaging tests
test_that("check_for - nesting", {
  lst <- list()
  lst$DC_SOLUTION <- "for (i in 1:10) { for (j in 1:5) { print('abcde') }}"
  lst$DC_SCT <- "forloop <- ex() %>% check_for()
                 forloop %>% check_cond() %>% check_code('10')
                 forloop2 <- forloop %>% check_body() %>% check_for()
                 forloop2 %>% check_cond() %>% check_code('5')
                 forloop2 %>% check_body() %>% check_function('print') %>% check_arg('x') %>% check_equal()"
  
  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "for (i in 1:5) { }"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "for (i in 1:10) { }"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "for (i in 1:10) { for (j in 1:11) { }}"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "for (i in 1:10) { for (j in 1:10) { }}"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "for (i in 1:10) { for (j in 1:10) { print('test') }}"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})

test_that("test_for - test_ifelse inside", {
  # TODO  
})

test_that("test_for - highlighting", {
  # TODO
})

test_that("test_for - errs appropriately", {
  # TODO
})