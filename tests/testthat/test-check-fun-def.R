context("check_fun_def")

test_that("check_fun_def - step by step", {
  lst <- list()
  lst$DC_SOLUTION <- "my_fun <- function(x, y) { stopifnot(is.double(x)); print(x + y); return(x + y) }"
  lst$DC_SCT <- "fundef <- ex() %>% check_fun_def('my_fun')
                 fundef %>% check_arguments()
                 fundef %>% check_body() %>% check_function('print') %>% check_arg('x')
                 fundef %>% check_call(x = 2, y = 3) %>% check_result() %>% check_equal()
                 fundef %>% check_call(x = 2, y = 3L) %>% check_output() %>% check_equal()
                 fundef %>% check_call(x = 2L, y = 3L) %>% check_error() %>% check_equal()"

  lst$DC_CODE <- ""
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you define the function")

  lst$DC_CODE <- "my_fun <- 123"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Are you sure that .*? is a function")

  lst$DC_CODE <- "my_fun <- function(x) { return(x) }"
  capture.output(output <- test_it(lst))
  fails(output)
  fb_contains(output, "Did you correctly define the function <code>my_fun()</code>")
  fb_contains(output, "Did you specify the correct number of arguments")

  lst$DC_CODE <- "my_fun <- function(x, y) { return(x + y) }"
  capture.output(output <- test_it(lst))
  fails(output)
  fb_contains(output, "Did you correctly define the function <code>my_fun()</code>?")
  fb_contains(output, "Check the body.")
  fb_contains(output, "Have you called <code>print()</code>?")

  lst$DC_CODE <- "my_fun <- function(x, y) { print('a'); stop('test') }"
  capture.output(output <- test_it(lst))
  fails(output)
  fb_contains(output, "Did you correctly define the function <code>my_fun()</code>")
  fb_contains(output, "Running <code>my_fun(x = 2, y = 3)</code> generated an error")

  lst$DC_CODE <- "my_fun <- function(x, y) { print('a'); return(x + c(y, y)) }"
  capture.output(output <- test_it(lst))
  fails(output)
  fb_contains(output, "Did you correctly define the function <code>my_fun()</code>")
  fb_contains(output, "Running <code>my_fun(x = 2, y = 3)</code> didn&#39;t give the correct result. ")
  fb_contains(output, "The result has length 2, while it should have length 1")

  lst$DC_CODE <- "my_fun <- function(x, y) { print('a'); stopifnot(is.double(y)); return(x + y) }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you correctly define the function <code>my_fun\\(\\)</code>")
  fails(output, mess_patt = "Running .*? generated an error")

  lst$DC_CODE <- "my_fun <- function(x, y) { print('a'); return(x + y) }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you correctly define the function <code>my_fun\\(\\)</code>")
  fails(output, mess_patt = "Running .*? correct output\\. Expected <code>\\[1\\] 5</code>, but got <code>\\[1\\] &quot;a&quot;</code>")

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

test_that("check_fun_def - step by step - custom", {
  lst <- list()
  lst$DC_SOLUTION <- "my_fun <- function(x, y) { stopifnot(is.double(x)); print(x + y); return(x + y) }"
  lst$DC_SCT <- "fundef <- ex() %>% check_fun_def('my_fun', undefined_msg = 'notdefined', no_fundef_msg = 'nofundef')
                 fundef %>% check_arguments(incorrect_number_arguments_msg = 'incorrectnumargs')
                 fundef %>% check_body() %>% check_function('print') %>% check_arg('x', arg_not_specified_msg = 'test')
                 fundef %>% check_call(x = 2, y = 3) %>% check_result(error_msg = 'error1') %>% check_equal(incorrect_msg = 'incorr1')
                 fundef %>% check_call(x = 2, y = 3L) %>% check_output(error_msg = 'error2') %>% check_equal(incorrect_msg = 'incorr2')
                 fundef %>% check_call(x = 2L, y = 3L) %>% check_error(no_error_msg = 'error3') %>% check_equal(incorrect_msg = 'incorr3')"

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
  fails(output, mess_patt = "Have you called <code>print\\(\\)</code>")

  lst$DC_CODE <- "my_fun <- function(x, y) { print('a'); stop('test') }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you correctly define the function <code>my_fun\\(\\)</code>")
  fails(output, mess_patt = "Error1")

  lst$DC_CODE <- "my_fun <- function(x, y) { print('a'); return(x + c(y, y)) }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you correctly define the function <code>my_fun\\(\\)</code>")
  fails(output, mess_patt = "Running <code>my_fun\\(x = 2, y = 3\\)</code>")
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

test_that("check_fun_def - backwards compatibility", {
  lst <- list()
  lst$DC_SOLUTION <- "my_fun <- function(x, y) { stopifnot(is.double(x)); print(x + y); return(x + y) }"
  lst$DC_SCT <- "test_function_definition('my_fun',
                                          function_test = {
                                            test_expression_result('my_fun(x = 2, y = 3)')
                                            test_expression_output('my_fun(x = 2, y = 3L)')
                                            test_expression_error('my_fun(x = 2L, y = 3L)')
                                          },
                                          body_test = {
                                            test_function('print', 'x', eval = FALSE)
                                          })"

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
  fails(output, mess_patt = "Check the body.*?Have you called <code>print\\(\\)</code>")

  lst$DC_CODE <- "my_fun <- function(x, y) { print('a'); stop('test') }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you correctly define the function <code>my_fun\\(\\)</code>")

  lst$DC_CODE <- "my_fun <- function(x, y) { print('a'); return(x + c(y, y)) }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you correctly define the function <code>my_fun\\(\\)</code>")

  lst$DC_CODE <- "my_fun <- function(x, y) { print('a'); stopifnot(is.double(y)); return(x + y) }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you correctly define the function <code>my_fun\\(\\)</code>")

  lst$DC_CODE <- "my_fun <- function(x, y) { print('a'); return(x + y) }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you correctly define the function <code>my_fun\\(\\)</code>")

  lst$DC_CODE <- "my_fun <- function(x, y) { print(x + y); return(x + y) }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "generate an error, but it should\\.")

  lst$DC_CODE <- "my_fun <- function(x, y) { print(x + y); if (!is.double(x)) { stop('blabla') }; return(x + y) }"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "generate the correct error\\. Expected the error <code>is.double\\(x\\) is not TRUE</code>, but instead got the error <code>blabla</code>")

  lst$DC_CODE <- "my_fun <- function(x, y) { stopifnot(is.double(x)); print(x + y); return(x + y) }"
  capture.output(output <- test_it(lst))
  passes(output)
})

test_that("check_fun_def - test_ifelse inside", {
  # TODO
})

test_that("check_fun_def - highlighting", {
  # TODO
})

test_that("check_fun_def - errs appropriately", {
  # TODO
})


context("test_function_definition (old)")

test_that("test_function_definition incorrect use", {
  lst <- list()
  lst$DC_SCT <- "test_function_definition('my_func')"

  lst$DC_SOLUTION <- ""
  lst$DC_CODE <- ""
  capture.output(output <- test_it(lst))
  error(output)

  lst$DC_SOLUTION <- "my_func <- 2"
  lst$DC_CODE <- "my_func <- 2"
  capture.output(output <- test_it(lst))
  error(output)
})

test_that("test_function_definition works", {
  lst <- list()
  lst$DC_CODE <- "my_func <- function(x, y) {\n  print(sprintf('Calculating %d plus %d', x, y))\n  x + y\n}"
  lst$DC_SOLUTION <- "my_func <- function(x, y) {\n  print(sprintf('Calculating %d plus %d', x, y))\n  x + y\n}\nmy_punk <- function () { FALSE }"

  lst$DC_SCT <- "test_function_definition(\"my_func\")"
  capture.output(output <- test_it(lst))
  passes(output)

  lst$DC_SCT <- paste("test_function_definition(\"my_func\", function_test = {",
                      "  test_expression_result('my_func(3,3)')",
                      "  test_expression_result('my_func(1,2)')",
                      "})", sep = "\n")
  capture.output(output <- test_it(lst))
  passes(output)

  lst$DC_SCT <- paste("test_function_definition(\"my_func\", function_test = {",
                      "  test_expression_output('my_func(3,3)')",
                      "  test_expression_output('my_func(1,2)')",
                      "})", sep = "\n")
  capture.output(output <- test_it(lst))
  passes(output)

  lst$DC_SCT <- "test_function_definition(\"my_punk\")"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Did you define")

  lst$DC_SCT <- "test_function_definition(\"my_punk\", undefined_msg = \"jajajaja\")"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Jajajaja")
})

test_that("test_function_definition works 2", {
  lst <- list()
  lst$DC_CODE <- "my_func <- function(x, y) {\n  print(sprintf('Calculating %d plus %d', x, y))\n  x - y\n}"
  lst$DC_SOLUTION <- "my_func <- function(x, y, z = 0) {\n  print(sprintf('Calculating %d plus %d', x, y))\n  x + y\n}"

  lst$DC_SCT <- "test_function_definition(\"my_func\")"
  capture.output(output <- test_it(lst))
  passes(output)

  lst$DC_SCT <- paste("test_function_definition(\"my_func\", function_test = {",
                      "  test_expression_result('my_func(3,3)')",
                      "  test_expression_result('my_func(1,2)')",
                      "}, incorrect_number_arguments_msg = \"oeioeioei\")", sep = "\n")
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Oeioeioei")

  lst$DC_SCT <- paste("test_function_definition(\"my_func\", function_test = {",
                      "  test_expression_result('my_func(3,3)')",
                      "  test_expression_result('my_func(1,2)')",
                      "})", sep = "\n")
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "number of arguments")

})

test_that("test_function_definition works 3", {
  lst <- list()
  lst$DC_CODE <- "my_func <- function(x, y) {\n  print(sprintf('Kalfculating %d plus %d', x, y))\n  x - y\n}"
  lst$DC_SOLUTION <- "my_func <- function(x, y) {\n  print(sprintf('Calculating %d plus %d', x, y))\n  x + y\n}"

  lst$DC_SCT <- paste("test_function_definition(\"my_func\", function_test = {",
                      "  test_expression_result('my_func(3,3)')",
                      "  test_expression_result('my_func(1,2)')",
                      "})", sep = "\n")
  capture.output(output <- test_it(lst))
  fails(output)

  lst$DC_SCT <- paste("test_function_definition(\"my_func\", function_test = {",
                      "  test_expression_result('my_func(3,3)')",
                      "  test_expression_result('my_func(1,2)')",
                      "})", sep = "\n")
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Running <code>my_func\\(3, 3\\)</code> didn&#39;t give the correct result")

  lst$DC_SCT <- paste("test_function_definition(\"my_func\", function_test = {",
                      "  test_expression_output('my_func(3,3)')",
                      "  test_expression_output('my_func(1,2)')",
                      "})", sep = "\n")
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Running <code>my_func\\(3, 3\\)</code> didn&#39;t generate the correct output")

})

test_that("test_function_defintion works with body_test", {
  lst <- list()
  lst$DC_CODE <- "my_fun <- function() { print('hello') }"
  lst$DC_SOLUTION <- "my_fun <- function() { print('hello') }"
  lst$DC_SCT <- paste("test_function_definition('my_fun',\n",
                      "function_test = test_expression_output('my_fun()'),\n",
                      "body_test = test_function('print', 'x'))")
  capture.output(output <- test_it(lst))
  passes(output)

  lst <- list()
  lst$DC_CODE <- "my_fun <- function() { print('helloooooo') }"
  lst$DC_SOLUTION <- "my_fun <- function() { print('hello') }"
  lst$DC_SCT <- paste("test_function_definition('my_fun',\n",
                      "function_test = test_expression_output('my_fun()'),\n",
                      "body_test = test_function('print', 'x'))")
  capture.output(output <- test_it(lst))
  fails(output)

  # If there's an incorrect SCST in function_test, should throw error
  lst <- list()
  lst$DC_CODE <- "my_fun <- function() { print('hello') }"
  lst$DC_SOLUTION <- "my_fun <- function() { print('hello') }"
  lst$DC_SCT <- paste("test_function_definition('my_fun',\n",
                      "function_test = test_expression_output('my_fun(123)'),\n",
                      "body_test = test_function('str', 'x'))")
  capture.output(output <- test_it(lst))
  error(output)

  # If there's an incorrect SCT in body_test, should throw error
  lst <- list()
  lst$DC_CODE <- "my_fun <- function() { print('hello') }"
  lst$DC_SOLUTION <- "my_fun <- function() { print('hello') }"
  lst$DC_SCT <- paste("test_function_definition('my_fun',\n",
                      "function_test = test_expression_output('my_fun()'),\n",
                      "body_test = test_function('str', 'x'))")
  capture.output(output <- test_it(lst))
  error(output)
})


test_that("test_function_definition works with control structure in there", {
  lst <- list()
  lst$DC_SOLUTION <- "my_filter <- function (x) {\n  if (x >= 0) {\n    print(x)\n  } else {\n    print(NULL)\n  }}"
  lst$DC_SCT <- paste("test_function_definition('my_filter',",
                      "function_test = {\ntest_expression_result('my_filter(5)')\ntest_expression_output('my_filter(-5)')\n},",
                      "body_test = test_if_else(index = 1, if_cond_test = test_student_typed(c(\">= 0\",\"0 =<\")), else_expr_test = test_function('print', 'x')))", sep = "")

  lst$DC_CODE <- "my_filter <- function (x) {\n  if (x > 0) {\n    print(x)\n  } else {\n    print(\"NULL\")\n  }}"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Have you typed")

  lst$DC_CODE <- "my_filter <- function (x) {\n  if (x >= 0) {\n    print(x)\n  } else {\n    str(\"NULL\")\n  }}"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "Have you called")

  lst$DC_CODE <- "my_filter <- function (x) {\n  if (x >= 0) {\n    print(x)\n  } else {\n    print(\"NULL\")\n  }}"
  capture.output(output <- test_it(lst))
  fails(output, mess_patt = "argument <code>x</code>")
  line_info(output, 5, 5)

  lst$DC_CODE <- lst$DC_SOLUTION
  capture.output(output <- test_it(lst))
  passes(output)
})

test_that("embedded check_fun_def - outer function", {
  expect_equal(TRUE, TRUE)
  lst <- list()
  lst$DC_SOLUTION <- "my_fun <- function(x) { twice <- function(a) { a * 2 }; twice(x) }"
  lst$DC_SCT <- "fundef <- ex() %>% check_fun_def('my_fun')
                 fundef %>% check_arguments()
                 innerfun_def <- fundef %>% check_body()"

  lst$DC_CODE <- lst$DC_SOLUTION
  capture.output(output <- test_it(lst))
  passes(output)
})

test_that("embedded check_fun_def - inner function", {
  # TODO not supported yet
})