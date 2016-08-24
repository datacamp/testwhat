context("test_if_else")

test_that("check_if - step by step", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- 4\nif (x > 3) { print('x is big') } else { print('x is small') }"
  lst$DC_SCT <- "ifelse <- ex() %>% check_if_else()
  ifelse %>% check_cond() %>% check_code('>')
  ifelse %>% check_if() %>% check_function('print') %>% check_arg('x') %>% check_equal()
  ifelse %>% check_else() %>% check_function('print') %>% check_arg('x') %>% check_equal()"
  
  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Are you sure you coded one if statement")
  
  lst$DC_CODE <- "x <- 4\nif (x < 3) { }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the first if statement")
  fails(output, mess_patt = "Check the condition")
  
  lst$DC_CODE <- "x <- 4\nif (x > 3) { print('x is small') }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the first if statement")
  fails(output, mess_patt = "Check the if part")
  
  lst$DC_CODE <- "x <- 4\nif (x > 3) { print('x is big') }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the first if statement")
  fails(output, mess_patt = "The else part is missing")
  
  lst$DC_CODE <- "x <- 4\nif (x > 3) { print('x is big') } else { print('x is big') }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the first if statement")
  fails(output, mess_patt = "Check the else part")
  fails(output, mess_patt = "Check the else part")
  
  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})

test_that("check_if - step by step - custom", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- 4\nif (x > 3) { print('x is big') } else { print('x is small') }"
  lst$DC_SCT <- "ifelse <- ex() %>% check_if_else(not_found_msg = 'notfound')
  ifelse %>% check_cond() %>% check_code('>', missing_msg = 'nottyped')
  ifelse %>% check_if() %>% check_function('print') %>% check_arg('x') %>% check_equal(incorrect_msg = 'incorr')
  ifelse %>% check_else(not_found_msg = 'elsenotfound') %>% check_function('print') %>% check_arg('x') %>% check_equal('incorr2')"
  
  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Notfound")
  
  lst$DC_CODE <- "x <- 4\nif (x < 3) { }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the first if statement\\. Check the condition\\. Nottyped")
  
  lst$DC_CODE <- "x <- 4\nif (x > 3) { print('x is small') }"
  output <- test_it(lst)
  fails(output, mess_patt =  "Incorr")
  
  lst$DC_CODE <- "x <- 4\nif (x > 3) { print('x is big') }"
  output <- test_it(lst)
  fails(output, mess_patt = "Elsenotfound")
  
  lst$DC_CODE <- "x <- 4\nif (x > 3) { print('x is big') } else { print('x is big') }"
  output <- test_it(lst)
  fails(output, mess_patt = "Incorr2")
  
  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})

test_that("check_if_else - step by step - backwards compatible", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- 4\nif (x > 3) { print('x is big') } else { print('x is small') }"
  lst$DC_SCT <- "test_if_else(if_cond_test = test_student_typed('>'), if_expr_test = test_function('print', 'x'), else_expr_test = test_function('print', 'x'))"
  
  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Are you sure you coded one if statement")
  
  lst$DC_CODE <- "x <- 4\nif (x < 3) { }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the first if statement")
  fails(output, mess_patt = "Check the condition")
  
  lst$DC_CODE <- "x <- 4\nif (x > 3) { print('x is small') }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the first if statement.")
  fails(output, mess_patt = "Check the if part.")
  
  lst$DC_CODE <- "x <- 4\nif (x > 3) { print('x is big') }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the first if statement")
  fails(output, mess_patt = "The else part is missing")
  
  lst$DC_CODE <- "x <- 4\nif (x > 3) { print('x is big') } else { print('x is big') }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the first if statement")
  fails(output, mess_patt = "Check the else part")
  
  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})

test_that("check_if_else - indexing", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- 4\nif (x > 4) { print('a') } else { print('b')}\nif(x > 5) { print('c') } else { print('d') }"
  lst$DC_SCT <- "ifelse <- ex() %>% check_if_else(2)
  ifelse %>% check_cond() %>% check_code('>')
  ifelse %>% check_if() %>% check_function('print') %>% check_arg('x') %>% check_equal()
  ifelse %>% check_else() %>% check_function('print') %>% check_arg('x') %>% check_equal()"
  
  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Are you sure you coded two if statements")
  
  lst$DC_CODE <- "x <- 4\nif (x > 4) { print('a') }\nif (x < 3) { }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the second if statement")
  fails(output, mess_patt = "Check the condition")
  
  lst$DC_CODE <- "x <- 4\nif (x > 4) { print('a') }\nif (x > 3) { print('r') }"
  output <- test_it(lst)
  fails(output, mess_patt =  "Check the second if statement")
  fails(output, mess_patt =  "Check the if part")
  
  lst$DC_CODE <- "x <- 4\nif (x > 4) { print('a') }\nif (x > 3) { print('c') }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the second if statement")
  fails(output, mess_patt = "The else part is missing")
  
  lst$DC_CODE <- "x <- 4\nif (x > 4) { print('a') }\nif (x > 3) { print('c') } else { print('c') }"
  output <- test_it(lst)
  fails(output, mess_patt = "Check the second if statement")
  fails(output, mess_patt = "Check the else part")
  
  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})

# TODO add messaging tests
test_that("check_if_else - nesting", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- 4\nif (x > 4) { print('a') } else if (x > 5) { print('b') } else { print('c') }"
  lst$DC_SCT <- "ifelse <- ex() %>% check_if_else()
  ifelse %>% check_cond() %>% check_code('>')
  ifelse %>% check_if() %>% check_function('print') %>% check_arg('x') %>% check_equal()
  subifelse <- ifelse %>% check_else() %>% check_if_else()
  subifelse %>% check_cond() %>% check_code('>')
  subifelse %>% check_if() %>% check_function('print') %>% check_arg('x') %>% check_equal()
  subifelse %>% check_else() %>% check_function('print') %>% check_arg('x') %>% check_equal()"
  
  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "x <- 4\nif (x > 4) { print('a') } else if (x < 4) { print('aa') }"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "x <- 4\nif (x > 4) { print('a') } else if (x > 5) { print('aa') }"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "x <- 4\nif (x > 4) { print('a') } else if (x > 5) { print('b') }"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- "x <- 4\nif (x > 4) { print('a') } else if (x > 5) { print('b') } else { print('bb') }"
  output <- test_it(lst)
  fails(output)
  
  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})

test_that("test_ifelse - test_ifelse inside", {
  # TODO
})

test_that("test_ifelse - highlighting", {
  # TODO
})

test_that("test_ifelse - errs appropriately", {
  # TODO
})



context("check_for")

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


context("check_while")

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
