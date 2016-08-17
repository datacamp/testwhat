context("test_expression")

test_that("text_expression - result", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))"
  lst$DC_SCT <- "ex() %>% test_expr('x$a') %>% test_result() %>% test_equal()"

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "generated an error")

  lst$DC_CODE <- "x <- data.frame(a = c(4, 5, 6, 7))"
  output <- test_it(lst)
  fails(output, mess_patt = "Running <code>x\\$a</code> .*? give the correct result")
  fails(output, mess_patt = "The result has length 4, while it should have length 3")

  lst$DC_CODE <- "x <- data.frame(a = c(1, 2, 3))"
  output <- test_it(lst)
  passes(output)
})

test_that("text_expression - result - custom", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))"
  lst$DC_SCT <- "ex() %>% test_expr('x$a') %>% test_result(error_msg = 'error') %>% test_equal(incorrect_msg = 'incorrect')"

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Error")

  lst$DC_CODE <- "x <- data.frame(a = c(4, 5, 6, 7))"
  output <- test_it(lst)
  fails(output, mess_patt = "Running <code>x\\$a</code> .*? give the correct result")
  fails(output, mess_patt = "Incorrect")

  lst$DC_CODE <- "x <- data.frame(a = c(1, 2, 3))"
  output <- test_it(lst)
  passes(output)
})

test_that("text_expression - output", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))"
  lst$DC_SCT <- "ex() %>% test_expr('x$a') %>% test_output() %>% test_equal()"
  
  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "generated an error")
  
  lst$DC_CODE <- "x <- data.frame(a = c(4, 5, 6, 7))"
  output <- test_it(lst)
  fails(output, mess_patt = "Running <code>x\\$a</code> .*? generate the correct output")
  fails(output, mess_patt = "Expected <code>\\[1\\] 1 2 3</code>, but got <code>\\[1\\] 4 5 6 7</code>")
  
  lst$DC_CODE <- "x <- data.frame(a = c(1, 2, 3))"
  output <- test_it(lst)
  passes(output)
})

test_that("test_expression - output - custom", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))"
  lst$DC_SCT <- "ex() %>% test_expr('x$a') %>% test_output(error_msg = 'error') %>% test_equal(incorrect_msg = 'incorrect')"
  
  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Error")
  
  lst$DC_CODE <- "x <- data.frame(a = c(4, 5, 6, 7))"
  output <- test_it(lst)
  fails(output, mess_patt = "Running <code>x\\$a</code> .*? generate the correct output")
  fails(output, mess_patt = "Incorrect")
  
  lst$DC_CODE <- "x <- data.frame(a = c(1, 2, 3))"
  output <- test_it(lst)
  passes(output)
})

test_that("test_expression - no output", {
  lst <- list()
  lst$DC_SOLUTION <- "my_fun <- function() { return(invisible(3)) }"
  lst$DC_CODE <- "my_fun <- function(x) { return(3) }"
  
  lst$DC_SCT <- "ex() %>% test_expr('my_fun()') %>% test_output() %>% test_equal()"
  output <- test_it(lst)
  fails(output, mess_patt = "Running <code>my_fun\\(\\)</code> .*? generate the correct output")
  fails(output, mess_patt = "Expected no output, but got <code>\\[1\\] 3</code>")
  
  lst$DC_SCT <- "ex() %>% test_expr('my_fun()') %>% test_result() %>% test_equal()"
  output <- test_it(lst)
  passes(output)
})


test_that("test_expression - error", {
  lst <- list()
  lst$DC_SOLUTION <- "my_fun <- function(x) { stopifnot(is.numeric(x)); return(x) }"
  lst$DC_SCT <- "ex() %>% test_expr('my_fun(NA)') %>% test_error() %>% test_equal()"
  
  lst$DC_CODE <- "my_fun <- function(x) { return(x) }"
  output <- test_it(lst)
  fails(output, mess_patt = "Running <code>my_fun\\(NA\\)</code> didn&#39;t generate an error, but it should")
  
  lst$DC_CODE <- "my_fun <- function(x) { stopifnot(is.double(x)); return(x) }"
  output <- test_it(lst)
  fails(output, mess_patt = "Running <code>my_fun\\(NA\\)</code> didn&#39;t generate the correct error")
  fails(output, mess_patt = "Expected the error <code>is.numeric\\(x\\) is not TRUE</code>, but instead got the error <code>is\\.double\\(x\\) is not TRUE</code>")
  
  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})

test_that("test_expression - error - custom", {
  lst <- list()
  lst$DC_SOLUTION <- "my_fun <- function(x) { stopifnot(is.numeric(x)); return(x) }"
  lst$DC_SCT <- "ex() %>% test_expr('my_fun(NA)') %>% test_error(no_error_msg = 'noerror') %>% test_equal(incorrect_msg = 'incorrect')"
  
  lst$DC_CODE <- "my_fun <- function(x) { return(x) }"
  output <- test_it(lst)
  fails(output, mess_patt = "Noerror")
  
  lst$DC_CODE <- "my_fun <- function(x) { stopifnot(is.double(x)); return(x) }"
  output <- test_it(lst)
  fails(output, mess_patt = "Running <code>my_fun\\(NA\\)</code> didn&#39;t generate the correct error")
  fails(output, mess_patt = "Incorrect")
  
  lst$DC_CODE <- lst$DC_SOLUTION
  output <- test_it(lst)
  passes(output)
})


context("test_expression (old) - result")

test_that("test_expression_result works", {
  lst <- list()
  lst$DC_CODE <- "func <- function(x) { x + 3 }\nfunc_not_eq <- function(x) { x + 4 }"
  lst$DC_SOLUTION <- "func <- function(x) { x + 3 }\nfunc_not_eq <- function(x) { x + 3 }"

  lst$DC_SCT <- "test_expression_result('func(3)')"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_expression_result('func_not_eq(3)')"
  output <- test_it(lst)
  fails(output, mess_patt = "Running <code>func_not_eq\\(3\\)</code> didn.*?give the correct result")

})

test_that("test_expression_result works 2", {
  lst <- list()
  lst$DC_CODE <- "func <- function(x) { x / 3 }"
  lst$DC_SOLUTION <- "func <- function(x) { x + 3 }"

  lst$DC_SCT <- "test_expression_result('func(3)')"
  output <- test_it(lst)
  fails(output)

})

test_that("test_expression_result works with NULL", {
  lst <- list()
  lst$DC_SOLUTION <- "func <- function(x) { return(NULL) }"
  lst$DC_SCT <- "test_expression_result('func(3)')"

  lst$DC_CODE <- "func <- function(x) { return(NULL) }"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "func <- function(x) { return('NULL') }"
  output <- test_it(lst)
  fails(output, mess_patt = "Running <code>func\\(3\\)</code> didn.*?give the correct result")
})

test_that("test_expression_result works with erroneous code", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- 5"
  lst$DC_CODE <- "x <- 5\nrm(x)"

  lst$DC_SCT <- "test_expression_result('class(non_existing)')"
  output <- test_it(lst)
  error(output)

  lst$DC_SCT <- "test_expression_result('class(x)')"
  output <- test_it(lst)
  fails(output)
})

test_that("test_expression_result works with eq_condition equal", {
  lst <- list()
  lst$DC_SOLUTION <- "func <- function(x) { return(list(a = 1, b = 2)) }"
  lst$DC_SCT <- "test_expression_result('func(3)', eq_condition = 'equal')"

  lst$DC_CODE <- "func <- function(x) { return(list(1, 2)) }"
  output <- test_it(lst)
  fails(output, "Running <code>func\\(3\\)</code>")

  lst$DC_CODE <- "func <- function(x) { return(list(a = 1, b = 2)) }"
  output <- test_it(lst)
  passes(output)
})

test_that("test_expression_result works with eq_condition equal", {
  lst <- list()
  lst$DC_SOLUTION <- "func <- function(x) { return(data.frame(a = c(1,2), b = c(3,4))) }"
  lst$DC_SCT <- "test_expression_result('func(3)', eq_condition = 'equal')"

  lst$DC_CODE <- "func <- function(x) { return(data.frame(c(1, 2), c(3, 4))) }"
  output <- test_it(lst)
  fails(output)

  lst$DC_CODE <- "func <- function(x) { return(data.frame(a = c(1, 2), b = c(3, 4))) }"
  output <- test_it(lst)
  passes(output)
})

context("test_expression (old) - output")

test_that("test_expression_output works", {
  lst <- list()
  lst$DC_CODE <- "func <- function(x) { print(x) }\nfunc_not_eq <- function(x) { print(x); x^2}"
  lst$DC_SOLUTION <- "func <- function(x) { print(x) }\nfunc_not_eq <- function(x) { print(paste('Output:',x)) }"

  lst$DC_SCT <- "test_expression_output('func(3)')"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_expression_output('func_not_eq(3)')"
  output <- test_it(lst)
  fails(output)
})


test_that("test_expression_output works 2", {
  lst <- list()
  lst$DC_CODE <- "func <- function(x) { print('Test: '); print(x); return(invisible(x)) }"
  lst$DC_SOLUTION <- "func <- function(x) { print('Test: '); print(x); return(invisible(x - 1)) }"

  lst$DC_SCT <- "test_expression_output('func(3)')"
  output <- test_it(lst)
  passes(output)

})

test_that("test_expression_output works with NULL", {
  lst <- list()
  lst$DC_SOLUTION <- "func <- function(x) { print(NULL) }"
  lst$DC_SCT <- "test_expression_output('func(3)')"

  lst$DC_CODE <- "func <- function(x) { print(NULL) }"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "func <- function(x) { print('NULL') }"
  output <- test_it(lst)
  fails(output)
})

test_that("test_expression_output works if broken", {
  lst <- list()
  lst$DC_CODE <- "func <- function(x) { print('Test: '); y <- wrong_stuff }"
  lst$DC_SOLUTION <- "func <- function(x) { print('Test: '); y <- wrong_stuff }"

  lst$DC_SCT <- "test_expression_output('func(3)')"
  output <- test_it(lst)
  error(output)

  lst$DC_SOLUTION <- "func <- function(x) { print('Test: '); y <- 'ok_stuff' }"
  output <- test_it(lst)
  fails(output)
})


context("test_expression (old) - error")

test_that("test_expression_error works", {
  lst <- list()
  lst$DC_SOLUTION <- paste("func <- function(x) { stop('error') }",
                           "func2 <- function(x) { stop('error') }",
                           "func3 <- function(x) { stop('error') }", sep = "\n")
  lst$DC_CODE <- paste("func <- function(x) { stop('error') }",
                       "func2 <- function(x) { message('noerror') }",
                       "func3 <- function(x) { stop('wrongerror') }", sep = "\n")

  lst$DC_SCT <- "test_expression_error('func(3)')"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_expression_error('func2(3)')"
  output <- test_it(lst)
  fails(output, mess_patt = "Running <code>func2\\(3\\)</code> didn.*? generate an error, but it should.")

  lst$DC_SCT <- "test_expression_error('func2(3)', no_error_msg = 'theresnoerror')"
  output <- test_it(lst)
  fails(output, mess_patt = "Theresnoerror")

  lst$DC_SCT <- "test_expression_error('func3(3)')"
  output <- test_it(lst)
  fails(output, mess_patt = "Running <code>func3\\(3\\)</code> .*? generate the correct error\\. Expected the error <code>error</code>, but instead got the error <code>wrongerror</code>")

  lst$DC_SCT <- "test_expression_error('func3(3)', incorrect_msg = 'notgood')"
  output <- test_it(lst)
  fails(output, mess_patt = "Notgood")
})
