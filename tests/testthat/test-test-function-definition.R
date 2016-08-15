context("test_function_definition")

test_that("test_fundef - step by step", {
  lst <- list()
  lst$DC_SOLUTION <- "my_fun <- function(x, y) { stopifnot(is.double(x)); print(x + y); return(x + y) }"
  lst$DC_SCT <- "fundef <- ex() %>% test_fun_def('my_fun')
                 fundef %>% test_arguments()
                 fundef %>% test_body() %>% test_fun('print') %>% test_arg('x')
                 fundef %>% test_result(x = 2, y = 3) %>% test_equal()
                 fundef %>% test_output(x = 2, y = 3L) %>% test_equal()
                 fundef %>% test_error(x = 2L, y = 3L) %>% test_equal()"
  
  lst$DC_CODE <- ""
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you define the function")

  lst$DC_CODE <- "my_fun <- 123"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Are you sure that .*? is a function")

  lst$DC_CODE <- "my_fun <- function(x) { return(x) }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you correctly define the function <code>my_fun\\(\\)</code>")
  fails(output, mess_patt = "Did you specify the correct number of arguments")

  lst$DC_CODE <- "my_fun <- function(x, y) { return(x + y) }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you correctly define the function <code>my_fun\\(\\)</code>")
  fails(output, mess_patt = "Check the body.*?The system wants to check the first call of")

  lst$DC_CODE <- "my_fun <- function(x, y) { print('a'); stop('test') }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you correctly define the function <code>my_fun\\(\\)</code>")
  fails(output, mess_patt = "Running .*? generated an error")
  
  lst$DC_CODE <- "my_fun <- function(x, y) { print('a'); return(x + c(y, y)) }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you correctly define the function <code>my_fun\\(\\)</code>")
  fails(output, mess_patt = "Calling .*? correct result.*?The result has length 2, while it should have length 1")
  
  lst$DC_CODE <- "my_fun <- function(x, y) { print('a'); stopifnot(is.double(y)); return(x + y) }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you correctly define the function <code>my_fun\\(\\)</code>")
  fails(output, mess_patt = "Running .*? generated an error")
  
  lst$DC_CODE <- "my_fun <- function(x, y) { print('a'); return(x + y) }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you correctly define the function <code>my_fun\\(\\)</code>")
  fails(output, mess_patt = "Calling .*? correct output\\. Expected <code>\\[1\\] 5</code>, but got <code>\\[1\\] &quot;a&quot;</code>")
  
  lst$DC_CODE <- "my_fun <- function(x, y) { print(x + y); return(x + y) }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you correctly define the function <code>my_fun\\(\\)</code>")
  fails(output, mess_patt = "generate an error, but it should\\.")
  
  lst$DC_CODE <- "my_fun <- function(x, y) { print(x + y); if (!is.double(x)) { stop('blabla') }; return(x + y) }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you correctly define the function <code>my_fun\\(\\)</code>")
  fails(output, mess_patt = "generate the correct error\\. Expected the error <code>is.double\\(x\\) is not TRUE</code>, but instead got the error <code>blabla</code>")
  
  lst$DC_CODE <- "my_fun <- function(x, y) { stopifnot(is.double(x)); print(x + y); return(x + y) }"
  capture.output(output <- test_it(lst))
  passes(output)
})

test_that("test_fundef - step by step - custom", {
  lst <- list()
  lst$DC_SOLUTION <- "my_fun <- function(x, y) { stopifnot(is.double(x)); print(x + y); return(x + y) }"
  lst$DC_SCT <- "fundef <- ex() %>% test_fun_def('my_fun', undefined_msg = 'notdefined', no_fundef_msg = 'nofundef')
  fundef %>% test_arguments(incorrect_number_args_msg = 'incorrectnumargs')
  fundef %>% test_body() %>% test_fun('print') %>% test_arg('x', arg_not_specified_msg = 'test')
  fundef %>% test_result(x = 2, y = 3, error_msg = 'error1') %>% test_equal(incorrect_msg = 'incorr1')
  fundef %>% test_output(x = 2, y = 3L, error_msg = 'error2') %>% test_equal(incorrect_msg = 'incorr2')
  fundef %>% test_error(x = 2L, y = 3L, no_error_msg = 'error3') %>% test_equal(incorrect_msg = 'incorr3')"
  
  lst$DC_CODE <- ""
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Notdefined")
  
  lst$DC_CODE <- "my_fun <- 123"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Nofundef")
  
  lst$DC_CODE <- "my_fun <- function(x) { return(x) }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you correctly define the function <code>my_fun\\(\\)</code>")
  fails(output, mess_patt = "Incorrectnumargs")
  
  lst$DC_CODE <- "my_fun <- function(x, y) { return(x + y) }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you correctly define the function <code>my_fun\\(\\)</code>")
  fails(output, mess_patt = "Check the body")
  fails(output, mess_patt = "wants to check the first call of <code>print\\(\\)</code>")
  
  lst$DC_CODE <- "my_fun <- function(x, y) { print('a'); stop('test') }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you correctly define the function <code>my_fun\\(\\)</code>")
  fails(output, mess_patt = "Error1")
  
  lst$DC_CODE <- "my_fun <- function(x, y) { print('a'); return(x + c(y, y)) }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you correctly define the function <code>my_fun\\(\\)</code>")
  fails(output, mess_patt = "Calling my_fun\\(x = 2, y = 3\\)")
  fails(output, mess_patt = "Incorr1")

  lst$DC_CODE <- "my_fun <- function(x, y) { print('a'); stopifnot(is.double(y)); return(x + y) }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you correctly define the function <code>my_fun\\(\\)</code>")
  fails(output, mess_patt = "Error2")
  
  lst$DC_CODE <- "my_fun <- function(x, y) { print('a'); return(x + y) }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you correctly define the function <code>my_fun\\(\\)</code>")
  fails(output, mess_patt = "generate the correct output")
  fails(output, mess_patt = "Incorr2")
  
  lst$DC_CODE <- "my_fun <- function(x, y) { print(x + y); return(x + y) }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you correctly define the function <code>my_fun\\(\\)</code>")
  fails(output, mess_patt = "Error3")
  
  lst$DC_CODE <- "my_fun <- function(x, y) { print(x + y); if (!is.double(x)) { stop('blabla') }; return(x + y) }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you correctly define the function <code>my_fun\\(\\)</code>")
  fails(output, mess_patt = "generate the correct error")
  fails(output, mess_patt = "Incorr3")
  
  lst$DC_CODE <- "my_fun <- function(x, y) { stopifnot(is.double(x)); print(x + y); return(x + y) }"
  capture.output(output <- test_it(lst))
  passes(output)
})

test_that("test_fundef - backwards compatibility", {
  # TODO (test_correct system should work!)
})

test_that("test_fundef - test_output but no output", {
  # TODO
})

test_that("test_fundef - test_ifelse inside", {
  # TODO
})

test_that("test_fundef - highlighting", {
  # TODO
})

test_that("test_fundef - errs appropriately", {
  # TODO
})






# test_that("test_function_definition incorrect use", {
#   lst <- list()
#   lst$DC_SCT <- "test_function_definition('my_func')"
# 
#   lst$DC_SOLUTION <- ""
#   lst$DC_CODE <- ""
#   output <- test_it(lst)
#   error(output)
# 
#   lst$DC_SOLUTION <- "my_func <- 2"
#   lst$DC_CODE <- "my_func <- 2"
#   output <- test_it(lst)
#   error(output)
# })
# 
# test_that("test_function_definition works", {
#   lst <- list()
#   lst$DC_CODE <- "my_func <- function(x, y) {\n  print(sprintf('Calculating %d plus %d', x, y))\n  x + y\n}"
#   lst$DC_SOLUTION <- "my_func <- function(x, y) {\n  print(sprintf('Calculating %d plus %d', x, y))\n  x + y\n}\nmy_punk <- function () { FALSE }"
# 
#   lst$DC_SCT <- "test_function_definition(\"my_func\")"
#   capture.output(output <- test_it(lst))
#   passes(output)
# 
#   lst$DC_SCT <- paste("test_function_definition(\"my_func\", function_test = {",
#                       "  test_expression_result('my_func(3,3)')",
#                       "  test_expression_result('my_func(1,2)')",
#                       "})", sep = "\n")
#   capture.output(output <- test_it(lst))
#   passes(output)
# 
#   lst$DC_SCT <- paste("test_function_definition(\"my_func\", function_test = {",
#                       "  test_expression_output('my_func(3,3)')",
#                       "  test_expression_output('my_func(1,2)')",
#                       "})", sep = "\n")
#   capture.output(output <- test_it(lst))
#   passes(output)
# 
#   lst$DC_SCT <- "test_function_definition(\"my_punk\")"
#   capture.output(output <- test_it(lst))
#   fails(output, mess_patt = "Did you define")
# 
#   lst$DC_SCT <- "test_function_definition(\"my_punk\", undefined_msg = \"jajajaja\")"
#   capture.output(output <- test_it(lst))
#   fails(output, mess_patt = "jajajaja")
# 
# })
# 
# test_that("test_function_definition works 2", {
#   lst <- list()
#   lst$DC_CODE <- "my_func <- function(x, y) {\n  print(sprintf('Calculating %d plus %d', x, y))\n  x - y\n}"
#   lst$DC_SOLUTION <- "my_func <- function(x, y, z = 0) {\n  print(sprintf('Calculating %d plus %d', x, y))\n  x + y\n}"
# 
#   lst$DC_SCT <- "test_function_definition(\"my_func\")"
#   capture.output(output <- test_it(lst))
#   passes(output)
# 
#   lst$DC_SCT <- paste("test_function_definition(\"my_func\", function_test = {",
#                       "  test_expression_result('my_func(3,3)')",
#                       "  test_expression_result('my_func(1,2)')",
#                       "}, incorrect_number_arguments_msg = \"oeioeioei\")", sep = "\n")
#   capture.output(output <- test_it(lst))
#   fails(output, mess_patt = "oeioeioei")
# 
#   lst$DC_SCT <- paste("test_function_definition(\"my_func\", function_test = {",
#                       "  test_expression_result('my_func(3,3)')",
#                       "  test_expression_result('my_func(1,2)')",
#                       "})", sep = "\n")
#   capture.output(output <- test_it(lst))
#   fails(output, mess_patt = "number of arguments")
# 
# })
# 
# test_that("test_function_definition works 3", {
#   lst <- list()
#   lst$DC_CODE <- "my_func <- function(x, y) {\n  print(sprintf('Kalfculating %d plus %d', x, y))\n  x - y\n}"
#   lst$DC_SOLUTION <- "my_func <- function(x, y) {\n  print(sprintf('Calculating %d plus %d', x, y))\n  x + y\n}"
# 
#   lst$DC_SCT <- paste("test_function_definition(\"my_func\", function_test = {",
#                       "  test_expression_result('my_func(3,3)')",
#                       "  test_expression_result('my_func(1,2)')",
#                       "})", sep = "\n")
#   capture.output(output <- test_it(lst))
#   fails(output)
# 
#   lst$DC_SCT <- paste("test_function_definition(\"my_func\", function_test = {",
#                       "  test_expression_result('my_func(3,3)')",
#                       "  test_expression_result('my_func(1,2)')",
#                       "})", sep = "\n")
#   capture.output(output <- test_it(lst))
#   fails(output, mess_patt = "Make sure that running")
# 
#   lst$DC_SCT <- paste("test_function_definition(\"my_func\", function_test = {",
#                       "  test_expression_output('my_func(3,3)')",
#                       "  test_expression_output('my_func(1,2)')",
#                       "})", sep = "\n")
#   capture.output(output <- test_it(lst))
#   fails(output, mess_patt = "Make sure that .* outputs")
# 
# })
# 
# test_that("test_function_defintion works with body_test", {
#   lst <- list()
#   lst$DC_CODE <- "my_fun <- function() { print('hello') }"
#   lst$DC_SOLUTION <- "my_fun <- function() { print('hello') }"
#   lst$DC_SCT <- paste("test_function_definition('my_fun',\n",
#                       "function_test = test_expression_output('my_fun()'),\n",
#                       "body_test = test_function('print', 'x'))")
#   capture.output(output <- test_it(lst))
#   passes(output)
# 
#   lst <- list()
#   lst$DC_CODE <- "my_fun <- function() { print('helloooooo') }"
#   lst$DC_SOLUTION <- "my_fun <- function() { print('hello') }"
#   lst$DC_SCT <- paste("test_function_definition('my_fun',\n",
#                       "function_test = test_expression_output('my_fun()'),\n",
#                       "body_test = test_function('print', 'x'))")
#   capture.output(output <- test_it(lst))
#   fails(output)
# 
#   # If there's an incorrect SCST in function_test, should throw error
#   lst <- list()
#   lst$DC_CODE <- "my_fun <- function() { print('hello') }"
#   lst$DC_SOLUTION <- "my_fun <- function() { print('hello') }"
#   lst$DC_SCT <- paste("test_function_definition('my_fun',\n",
#                       "function_test = test_expression_output('my_fun(123)'),\n",
#                       "body_test = test_function('str', 'x'))")
#   capture.output(output <- test_it(lst))
#   error(output)
# 
#   # If there's an incorrect SCT in body_test, should throw error
#   lst <- list()
#   lst$DC_CODE <- "my_fun <- function() { print('hello') }"
#   lst$DC_SOLUTION <- "my_fun <- function() { print('hello') }"
#   lst$DC_SCT <- paste("test_function_definition('my_fun',\n",
#                       "function_test = test_expression_output('my_fun()'),\n",
#                       "body_test = test_function('str', 'x'))")
#   capture.output(output <- test_it(lst))
#   error(output)
# })
# 
# 
# test_that("test_function_definition works with control structure in there", {
#   lst <- list()
#   lst$DC_SOLUTION <- "my_filter <- function (x) {\n  if (x >= 0) {\n    print(x)\n  } else {\n    print(NULL)\n  }}"
#   lst$DC_SCT <- paste("test_function_definition('my_filter',",
#                       "function_test = {\ntest_expression_result('my_filter(5)')\ntest_expression_output('my_filter(-5)')\n},",
#                       "body_test = test_if_else(index = 1, if_cond_test = test_student_typed(c(\">= 0\",\"0 =<\")), else_expr_test = test_function('print', 'x')))", sep = "")
# 
#   lst$DC_CODE <- "my_filter <- function (x) {\n  if (x > 0) {\n    print(x)\n  } else {\n    print(\"NULL\")\n  }}"
#   capture.output(output <- test_it(lst))
#   fails(output, mess_patt = "appropriate location")
# 
#   lst$DC_CODE <- "my_filter <- function (x) {\n  if (x >= 0) {\n    print(x)\n  } else {\n    str(\"NULL\")\n  }}"
#   capture.output(output <- test_it(lst))
#   fails(output, mess_patt = "system wants to check")
# 
#   lst$DC_CODE <- "my_filter <- function (x) {\n  if (x >= 0) {\n    print(x)\n  } else {\n    print(\"NULL\")\n  }}"
#   capture.output(output <- test_it(lst))
#   fails(output, mess_patt = "argument <code>x</code>")
#   line_info(output, 5, 5)
# 
#   lst$DC_CODE <- lst$DC_SOLUTION
#   capture.output(output <- test_it(lst))
#   passes(output)
# })

