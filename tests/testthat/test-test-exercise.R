context("test_exercise")

test_that("test_exercise works properly", {
  
  run_test_exercise <- function(stu_code, ..., sct) {
    test_exercise(
      sct = sct,
      ex_type = "NormalExercise", 
      pec = "",
      student_code = stu_code,
      solution_code = "x <- 4",
      student_env = list2env(list(...), parent = globalenv()),
      solution_env = list2env(list(x = 4), parent = globalenv()),
      output_list = list(),
      seed = 42
    )  
  }
  
  expect_true(run_test_exercise("x <- 4", x = 4, sct = "test_object('x')")$correct)
  
  expect_false(run_test_exercise("x <- 5", x = 5, sct = "test_object('x')")$correct)

  # message outside of function
  sct <- "msg <- 'incorrect_obj'\ntest_object('x', incorrect_msg = msg)"
  res <- run_test_exercise("x <- 5", x = 5, sct = sct)
  expect_false(res$correct)
  expect_true(grepl("Incorrect_obj", res$message))
  
  # message outside of construct
  sct <- "msg <- 'incorrect_obj'\ntest_correct(test_object('x', incorrect_msg = msg), test_student_typed('x'))"
  res <- run_test_exercise("x <- 5", x = 5, sct = sct)
  expect_false(res$correct)
  expect_true(grepl("Incorrect_obj", res$message))
})

