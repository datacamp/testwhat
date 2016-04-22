context("test_what")
source("helpers.R")

test_that("test_what works as it should", {
  reporter <- DataCampReporter$new()
  expect_error(with_reporter(reporter, run_until_fail(parse(text = 'test_what(expect_true(TRUE))'), test_env())))
  reporter$end_reporter()
  
  reporter <- DataCampReporter$new()
  expect_error(with_reporter(reporter, run_until_fail(parse(text = 'test_what(expect_true(TRUE), feedback = NULL)'), test_env())))
  reporter$end_reporter()
  
  reporter <- DataCampReporter$new()
  expect_error(with_reporter(reporter, run_until_fail(parse(text = 'test_what(expect_true(TRUE), feedback = "")'), test_env())))
  reporter$end_reporter()
  
  reporter <- DataCampReporter$new()
  expect_error(with_reporter(reporter, run_until_fail(parse(text = 'test_what(expect_true(TRUE), feedback = 1234)'), test_env())))
  reporter$end_reporter()
  
  reporter <- DataCampReporter$new()
  expect_error(with_reporter(reporter, run_until_fail(parse(text = 'test_what(expect_true(TRUE), feedback = list(not_message = \"test\"))'), test_env())))
  reporter$end_reporter()
  
  reporter <- DataCampReporter$new()
  expect_error(with_reporter(reporter, run_until_fail(parse(text = 'test_what(expect_true(TRUE), feedback = list(message = NULL))'), test_env())))
  reporter$end_reporter()
})

