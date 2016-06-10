context("test_what")
source("helpers.R")

test_that("test_what works as it should", {
  reporter <- DataCampReporter$new()
  expect_error(with_reporter(reporter, run_until_fail(parse(text = 'test_what(expect_true(TRUE))'))))
  reporter$end_reporter()
  
  reporter <- DataCampReporter$new()
  expect_error(with_reporter(reporter, run_until_fail(parse(text = 'test_what(expect_true(TRUE), feedback = NULL)'))))
  reporter$end_reporter()
  
  reporter <- DataCampReporter$new()
  expect_error(with_reporter(reporter, run_until_fail(parse(text = 'test_what(expect_true(TRUE), feedback = "")'))))
  reporter$end_reporter()
  
  reporter <- DataCampReporter$new()
  expect_error(with_reporter(reporter, run_until_fail(parse(text = 'test_what(expect_true(TRUE), feedback = 1234)'))))
  reporter$end_reporter()
  
  reporter <- DataCampReporter$new()
  expect_error(with_reporter(reporter, run_until_fail(parse(text = 'test_what(expect_true(TRUE), feedback = list(not_message = \"test\"))'))))
  reporter$end_reporter()
  
  reporter <- DataCampReporter$new()
  expect_error(with_reporter(reporter, run_until_fail(parse(text = 'test_what(expect_true(TRUE), feedback = list(message = NULL))'))))
  reporter$end_reporter()
})

