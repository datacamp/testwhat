context("check_output")

test_that("check_output - basic", {
  lst <- list()
  lst$DC_ECHO <- TRUE
  lst$DC_CODE <- "'testing'\n123"

  lst$DC_SCT <- "ex() %>% check_output('123')"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "ex() %>% check_output('\\\\d{3,}')"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "ex() %>% check_output('\\\\d{4,}')"
  output <- test_it(lst)
  fails(output, 'contain the pattern')

  lst$DC_SCT <- "ex() %>% check_output('\\\\d{4,}', missing_msg = 'not correct')"
  output <- test_it(lst)
  fails(output, 'Not correct')

  lst$DC_SCT <- "ex() %>% check_output('123', fixed = TRUE)"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "ex() %>% check_output('1234', fixed = TRUE)"
  output <- test_it(lst)
  fails(output, 'contain the pattern')

  lst$DC_SCT <- "ex() %>% check_output('1234', fixed = TRUE, missing_msg = 'not correct')"
  output <- test_it(lst)
  fails(output, 'Not correct')

  lst$DC_SCT <- "ex() %>% check_output('testing', times = 2)"
  output <- test_it(lst)
  fails(output, 'contain the pattern')

  lst$DC_SCT <- "ex() %>% check_output('\\\\[1\\\\] testing', fixed = TRUE, times = 2)"
  output <- test_it(lst)
  fails(output, 'contain the pattern')

  lst$DC_SCT <- "ex() %>% check_output('testing', times = 2, missing_msg = 'not correct')"
  output <- test_it(lst)
  fails(output, 'Not correct')
})

test_that("check_output - errors", {
  lst <- list()
  lst$DC_ECHO <- TRUE

  # below code gives error: Error: non-numeric argument to binary operator
  lst$DC_CODE <- "'test' + 3"

  lst$DC_SCT <- "ex() %>% check_output('non-numeric')"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "ex() %>% check_output('argument', times = 2)"
  output <- test_it(lst)
  fails(output)

  lst$DC_SCT <- "ex() %>% check_output('argument', times = 2, missing_msg = 'not correct')"
  output <- test_it(lst)
  fails(output, 'Not correct')
})


context("check_output_expr")

test_that("check_output_expr - basic", {
  lst <- list()
  lst$DC_SCT <- "ex() %>% check_output_expr('print(123)')"

  lst$DC_CODE <- "print(12)"
  output <- test_it(lst)
  fails(output, mess_patt = "Did your code produce the same output as <code>print\\(123\\)</code>?")

  lst$DC_CODE <- "print(123)"
  output <- test_it(lst)
  passes(output)
})

test_that("check_output_expr - basic - custom", {
  lst <- list()
  lst$DC_SCT <- "ex() %>% check_output_expr('print(123)', missing_msg = 'missing')"

  lst$DC_CODE <- "print(12)"
  output <- test_it(lst)
  fails(output, mess_patt = "Missing")

  lst$DC_CODE <- "print(123)"
  output <- test_it(lst)
  passes(output)
})

test_that("check_output_expr - times", {
  lst <- list()
  lst$DC_SCT <- "ex() %>% check_output_expr('print(123)', times = 3)"

  lst$DC_CODE <- "print(123); print(123)"
  output <- test_it(lst)
  fails(output)

  lst$DC_CODE <- "print(123); print(123); print(123)"
  output <- test_it(lst)
  passes(output)
})

test_that("check_output_expr - error", {
  lst <- list()
  lst$DC_CODE <- "x <- mtcars\nsummary(x)\nrm(x)"
  lst$DC_SCT <- "ex() %>% check_output_expr(\'summary(x)\')"
  output <- test_it(lst)
  fails(output)
})

test_that("check_output_expr - backwards compatatibility", {
  lst <- list()
  lst$DC_SCT <- "test_output_contains('print(123)')"

  lst$DC_CODE <- "print(12)"
  output <- test_it(lst)
  fails(output, mess_patt = "Did your code produce the same output as <code>print\\(123\\)</code>?")

  lst$DC_CODE <- "print(123)"
  output <- test_it(lst)
  passes(output)
})

test_that("check_output_expr - use solution code", {
  code <- "print(12)"
  lst <- list(
    DC_CODE = code,
    DC_SOLUTION = code,
    DC_SCT = "test_output_contains(get_solution_code())"
  )
  passes(test_it(lst))
})

test_that("check_output - message output", {
  lst <- list()
  lst$DC_ECHO <- TRUE
  lst$DC_CODE <- 'message("I am a message")'
  
  lst$DC_SCT <- "ex() %>% check_output('I am a message')"
  output <- test_it(lst)
  passes(output)
  
  lst$DC_SCT <- "ex() %>% check_output('I am a message', output_only = FALSE)"
  output <- test_it(lst)
  passes(output)
})


