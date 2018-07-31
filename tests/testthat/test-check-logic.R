context("check_correct")

test_that("test_correct - 1", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- mean(1:11)"
  lst$DC_SCT <- "test_correct(ex() %>% check_object('x') %>% check_equal(),
                              ex() %>% check_function('mean') %>% check_arg('x') %>% check_equal())"

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Have you called <code>mean\\(\\)</code>")

  lst$DC_CODE <- "x <- 8"
  output <- test_it(lst)
  fails(output, mess_patt = "Have you called <code>mean\\(\\)</code>")

  lst$DC_CODE <- "x <- mean(1:123)"
  output <- test_it(lst)
  fails(output, mess_patt = "Check your call of <code>mean\\(\\)</code>")
  fails(output, mess_patt = "Did you correctly specify the argument <code>x</code>")
  fails(output, mess_patt = "It has length 123, while it should have length 11")

  lst$DC_CODE <- "x <- mean(1:11) + 2"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable <code>x</code>")

  lst$DC_CODE <- "x <- mean(1:11)"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "x <- 6"
  output <- test_it(lst)
  passes(output)
})

test_that("test_correct - 2", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- mean(1:11) + mean(1:123)"
  lst$DC_SCT <- "test_correct(ex() %>% check_object('x') %>% check_equal(), {
                                ex() %>% check_function('mean', index = 1) %>% check_arg('x') %>% check_equal()
                                ex() %>% check_function('mean', index = 2) %>% check_arg('x') %>% check_equal()
                              })"

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Have you called <code>mean\\(\\)</code>")

  lst$DC_CODE <- "x <- mean(1:11)"
  output <- test_it(lst)
  fails(output, mess_patt = "Have you called <code>mean\\(\\)</code> twice")

  lst$DC_CODE <- "x <- mean(1:11) + mean(1:12)"
  output <- test_it(lst)
  fails(output, mess_patt = "Check your call of <code>mean\\(\\)</code>")
  fails(output, mess_patt = "Did you correctly specify the argument <code>x</code>")
  fails(output, mess_patt = "It has length 12, while it should have length 123")

  lst$DC_CODE <- "x <- mean(1:11) + mean(1:123) + 2"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable <code>x</code>")

  lst$DC_CODE <- "x <- mean(c(68, 68))"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "x <- 68"
  output <- test_it(lst)
  passes(output)
})

test_that("test_correct throws errors if incorrect diagnose_code", {
  lst <- list()
  lst$DC_CODE <- "a <- 2; b <- 2; c <- a + b"
  lst$DC_SOLUTION <- "b <- 2; c <- 2 + b"

  lst$DC_SCT <- "test_correct(test_object('c'), test_object('b'))"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_correct(test_object('c'), test_object('a'))"
  expect_error(test_it(lst))

  lst <- list()
  lst$DC_CODE <- "x <- nchar('test')"
  lst$DC_SOLUTION <- "x <- nchar('test')"
  lst$DC_SCT <- "test_correct(test_object('x'), test_function('nchar', 'x'))"
  output <- test_it(lst)
  passes(output)

  lst <- list()
  lst$DC_CODE <- "x <- nchar('tester')"
  lst$DC_SOLUTION <- "x <- nchar('test')"
  lst$DC_SCT <- "test_correct(test_object('x'), test_function('nchar', 'x'))"
  output <- test_it(lst)
  fails(output)

  lst <- list()
  lst$DC_CODE <- "x <- nchar('tester')"
  lst$DC_SOLUTION <- "x <- nchar('test')"
  lst$DC_SCT <- "test_correct(test_object('x'), test_function('nchar', 'object'))"
  expect_error(test_it(lst))
  lst$DC_CODE <- "x <- nchar('test')"
  expect_error(test_it(lst))
})

test_that("test_correct - nested", {
  lst <- list()
  lst$DC_SOLUTION <- "a <- 1:5; b <- mean(a)"
  lst$DC_SCT <- "test_correct(test_object('b', incorrect_msg = 'b_incorrect'), test_correct(test_function('mean', args = 'x', incorrect_msg = 'mean_incorrect'), test_object('a', incorrect_msg = 'a_incorrect')))"

  lst$DC_CODE <- "a <- 1:5; b <- mean(a)"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "b <- 3"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "a <- 1:4; b <- mean(a)"
  output <- test_it(lst)
  fails(output, mess_patt = 'A_incorrect')

  lst$DC_CODE <- "a <- 1:5; b <- mean(a + 1)"
  output <- test_it(lst)
  fails(output, mess_patt = 'Mean_incorrect')

  lst$DC_CODE <- "a <- 1:5; b <- mean(a) + 1"
  output <- test_it(lst)
  fails(output, mess_patt = 'B_incorrect')
})

test_that("check_correct - old skool", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- mean(1:11)"
  lst$DC_SCT <- "check_correct(
                      ex() %>% check_object('x') %>% check_equal(),
                      ex() %>% check_function('mean') %>% check_arg('x') %>% check_equal()
                 )"

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Have you called <code>mean\\(\\)</code>")

  lst$DC_CODE <- "x <- 8"
  output <- test_it(lst)
  fails(output, mess_patt = "Have you called <code>mean\\(\\)</code>")

  lst$DC_CODE <- "x <- mean(1:123)"
  output <- test_it(lst)
  fails(output, mess_patt = "Check your call of <code>mean\\(\\)</code>")
  fails(output, mess_patt = "Did you correctly specify the argument <code>x</code>")
  fails(output, mess_patt = "It has length 123, while it should have length 11")

  lst$DC_CODE <- "x <- mean(1:11) + 2"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable <code>x</code>")

  lst$DC_CODE <- "x <- mean(1:11)"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "x <- 6"
  output <- test_it(lst)
  passes(output)
})

test_that("check_correct - new school", {
  lst <- list()
  lst$DC_SOLUTION <- "x <- mean(1:11)"
  lst$DC_SCT <- "ex() %>% check_correct(
                      check_object(., 'x') %>% check_equal(.),
                      check_function(., 'mean') %>% check_arg('x') %>% check_equal()
                 )"

  lst$DC_CODE <- ""
  output <- test_it(lst)
  fails(output, mess_patt = "Have you called <code>mean\\(\\)</code>")

  lst$DC_CODE <- "x <- 8"
  output <- test_it(lst)
  fails(output, mess_patt = "Have you called <code>mean\\(\\)</code>")

  lst$DC_CODE <- "x <- mean(1:123)"
  output <- test_it(lst)
  fails(output, mess_patt = "Check your call of <code>mean\\(\\)</code>")
  fails(output, mess_patt = "Did you correctly specify the argument <code>x</code>")
  fails(output, mess_patt = "It has length 123, while it should have length 11")

  lst$DC_CODE <- "x <- mean(1:11) + 2"
  output <- test_it(lst)
  fails(output, mess_patt = "The contents of the variable <code>x</code>")

  lst$DC_CODE <- "x <- mean(1:11)"
  output <- test_it(lst)
  passes(output)

  lst$DC_CODE <- "x <- 6"
  output <- test_it(lst)
  passes(output)
})

context("check_or")

test_that("test_or works", {
  lst <- list()
  lst$DC_CODE <- "b = 3; c = 5"
  lst$DC_SOLUTION <- "a = 2.5; b = 3; c = 29"

  lst$DC_SCT <- "test_or(test_object('a'), test_object('b'), test_object('c'))"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_or(test_object('a'), test_object('c'))"
  output <- test_it(lst)
  fails(output, mess_patt = ".*define .*a")

  lst$DC_SCT <- "test_or(test_object('a'), test_object('c'), incorrect_msg = 'incorrect')"
  output <- test_it(lst)
  fails(output, mess_patt = "Incorrect")
})

test_that("check_or works old skool", {
  lst <- list()
  lst$DC_CODE <- "b = 3; c = 5"
  lst$DC_SOLUTION <- "a = 2.5; b = 3; c = 29"

  lst$DC_SCT <- "check_or(ex() %>% check_object('a') %>% check_equal(),
                          ex() %>% check_object('b') %>% check_equal(),
                          ex() %>% check_object('c') %>% check_equal())"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_or(test_object('a'), test_object('c'))"
  output <- test_it(lst)
  fails(output, mess_patt = ".*define .*a")

  lst$DC_SCT <- "test_or(test_object('a'), test_object('c'), incorrect_msg = 'incorrect')"
  output <- test_it(lst)
  fails(output, mess_patt = "Incorrect")
})

test_that("check_or works new skool", {
  lst <- list()
  lst$DC_CODE <- "b = 3; c = 5"
  lst$DC_SOLUTION <- "a = 2.5; b = 3; c = 29"

  lst$DC_SCT <- "ex() %>% check_or(check_object(., 'a') %>% check_equal(.),
                                   check_object(., 'b') %>% check_equal(),
                                   check_object(., 'c') %>% check_equal())"
  output <- test_it(lst)
  passes(output)

  lst$DC_SCT <- "test_or(test_object('a'), test_object('c'))"
  output <- test_it(lst)
  fails(output, mess_patt = ".*define .*a")

  lst$DC_SCT <- "test_or(test_object('a'), test_object('c'), incorrect_msg = 'incorrect')"
  output <- test_it(lst)
  fails(output, mess_patt = "Incorrect")
})

test_that("test_or throws error if something is off", {
  lst <- list()
  lst$DC_CODE <- "summary(mtcars); str(mtcars[1, 2])"
  lst$DC_SOLUTION <- "summary(mtcars); str(mtcars)"

  lst$DC_SCT <- "test_or(test_function('summary', 'object'), test_function('str', 'object'))"
  capture.output(output <- test_it(lst))
  passes(output)

  # argument of second test function incorrect
  lst$DC_SCT <- "test_or(test_function('summary', 'object'), test_function('str', 'x'))"
  expect_error(test_it(lst))
})

test_that("check_or - new school - nested", {
  lst <- list()
  lst$DC_SOLUTION <- "a <- 3\nif(a > 3) { print('b') }"

  lst$DC_SCT <- "ex() %>% check_or(
                    check_code(., 'c'),
                    check_if_else(.) %>% check_cond() %>% check_or(
                      check_code(., 'b'),
                      check_code(., 'd')
                    )
                  )"

  # Shouldn't pass, because b is not in the condition part of testwhat, and c or d aren't anywhere
  lst$DC_SOLUTION <- "a <- 3\nif(a > 3) { print('b') }"
  output <- test_it(lst)
  fails(output, mess_patt = "The system wanted to find the pattern <code>c</code>")
})

context("check_logic v2 only")

test_that("test_or fails if ENV set", {
  setup_state(stu_code = 'x <- 5', sol_code = 'x <- 5')
  withr::with_envvar(c(TESTWHAT_V2_ONLY = ''), {
    is.null(test_or(test_student_typed('x'), test_student_typed('x')))
  })
  withr::with_envvar(c(TESTWHAT_V2_ONLY = '0'), {
    is.null(test_or(test_student_typed('x'), test_student_typed('x')))
  })
  withr::with_envvar(c(TESTWHAT_V2_ONLY = '1'), {
    expect_error(test_or(test_student_typed('x'), test_student_typed('x')), regexp = 'test_or() can no longer be used in SCTs', fixed = TRUE)
  })
  withr::with_envvar(c(TESTWHAT_V2_ONLY = '1'), {
    expect_error(check_or(ex() %>% check_code('x'), ex() %>% check_code('x')), regexp = 'test_or() can no longer be used in SCTs', fixed = TRUE)
  })
  withr::with_envvar(c(TESTWHAT_V2_ONLY = '1'), {
    is.null(ex() %>% check_or(check_code(., 'x'), check_code(., 'x')))
  })
})

test_that("test_correct fails if ENV set", {
  setup_state(stu_code = 'x <- 5', sol_code = 'x <- 5')
  withr::with_envvar(c(TESTWHAT_V2_ONLY = ''), {
    is.null(test_correct(test_student_typed('x'), test_student_typed('x')))
  })
  withr::with_envvar(c(TESTWHAT_V2_ONLY = '0'), {
    is.null(test_correct(test_student_typed('x'), test_student_typed('x')))
  })
  withr::with_envvar(c(TESTWHAT_V2_ONLY = '1'), {
    expect_error(test_correct(test_student_typed('x'), test_student_typed('x')), regexp = 'test_correct() can no longer be used in SCTs', fixed = TRUE)
  })
  withr::with_envvar(c(TESTWHAT_V2_ONLY = '1'), {
    expect_error(check_correct(ex() %>% check_code('x'), ex() %>% check_code('x')), regexp = 'test_correct() can no longer be used in SCTs', fixed = TRUE)
  })
  withr::with_envvar(c(TESTWHAT_V2_ONLY = '1'), {
    is.null(ex() %>% check_correct(check_code(., 'x'), check_code(., 'x')))
  })
})
