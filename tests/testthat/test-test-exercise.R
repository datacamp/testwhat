context("test_exercise")

test_that("test_exercise works properly", {
  
  run_test_exercise <- function(stu_code, stu_env, sct = '', output_list = list(), allow_errors=FALSE) {
    test_exercise(
      sct = sct,
      ex_type = "NormalExercise", 
      pec = "",
      student_code = stu_code,
      solution_code = "x <- 4",
      student_env = list2env(stu_env, parent = globalenv()),
      solution_env = list2env(list(x = 4), parent = globalenv()),
      output_list = output_list,
      allow_errors = allow_errors,
      seed = 42
    )  
  }
  
  # sct fails
  res <- run_test_exercise(stu_code = "x <- 5",
                           stu_env = list(x = 5),
                           sct = "ex() %>% check_object('x') %>% check_equal()")
  expect_false(res$correct)
  
  # sct fails with custom message
  res <- run_test_exercise(stu_code = "x <- 5",
                           stu_env = list(x = 5),
                           sct = "ex() %>% check_object('x') %>% check_equal(incorrect_msg='abc')")
  expect_false(res$correct)
  expect_true(grepl('Abc', res$message))
  
  # sct fails with custom message outside of function
  res <- run_test_exercise(stu_code = "x <- 5",
                           stu_env = list(x = 5),
                           sct = "msg <- 'abc'\nex() %>% check_object('x') %>% check_equal(incorrect_msg = msg)")
  expect_false(res$correct)
  expect_true(grepl('Abc', res$message))
  
  # sct passes, but contains an error
  res <- run_test_exercise(stu_code = "x <- y",
                           stu_env = list(),
                           sct = '',
                           output_list = list(list(type = 'r-error', payload = 'undefined variable')))
  expect_false(res$correct)
  expect_true(grepl('Your code contains an error that you should fix', res$message))

  # sct passes, contains an error, but errors are allowed: should be fine
  res <- run_test_exercise(stu_code = "x <- y",
                           stu_env = list(),
                           sct = '',
                           output_list = list(list(type = 'r-error', payload = 'undefined variable')),
                           allow_errors = TRUE)
  expect_true(res$correct)
  
  # sct passes
  res <- run_test_exercise(stu_code = "x <- 4",
                           stu_env = list(x = 4),
                           sct = "ex() %>% check_object('x') %>% check_equal()")
  expect_true(res$correct)
})

