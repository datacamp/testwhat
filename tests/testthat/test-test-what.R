context("test_what")
source("helpers.R")

test_that("test_what works as it should", {
  reporter <- DataCampReporter$new(ex_type = "NormalExercise")
  expect_error(with_reporter(reporter, .test_exercise(parse(text = 'test_what(expect_true(TRUE), feedback = NULL)'), test_env())))
  reporter$end_reporter()
  
  reporter <- DataCampReporter$new(ex_type = "NormalExercise")
  expect_error(with_reporter(reporter, .test_exercise(parse(text = 'test_what(expect_true(TRUE), feedback = "")'), test_env())))
  reporter$end_reporter()
  
  reporter <- DataCampReporter$new(ex_type = "NormalExercise")
  expect_error(with_reporter(reporter, .test_exercise(parse(text = 'test_what(expect_true(TRUE), feedback_msg = NULL)'), test_env())))
  reporter$end_reporter()
  
  reporter <- DataCampReporter$new(ex_type = "NormalExercise")
  expect_error(with_reporter(reporter, .test_exercise(parse(text = 'test_what(expect_true(TRUE), feedback = list(message = NULL))'), test_env())))
  reporter$end_reporter()
})

