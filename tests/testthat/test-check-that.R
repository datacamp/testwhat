context("check_that")

test_that("check_that works as it should", {
  tw$set(reporter = DC_reporter$new())
  expect_error(run_until_fail(parse(text = 'check_that(is_true(TRUE))')))
  
  tw$set(reporter = DC_reporter$new())
  expect_error(run_until_fail(parse(text = 'check_that(is_true(TRUE), feedback = NULL)')))
  
  tw$set(reporter = DC_reporter$new())
  expect_error(run_until_fail(parse(text = 'check_that(is_true(TRUE), feedback = "")')))
  
  tw$set(reporter = DC_reporter$new())
  expect_error(run_until_fail(parse(text = 'check_that(is_true(TRUE), feedback = 1234)')))
  
  tw$set(reporter = DC_reporter$new())
  expect_error(run_until_fail(parse(text = 'check_that(is_true(TRUE), feedback = list(not_message = \"test\"))')))
  
  tw$set(reporter = DC_reporter$new())
  expect_error(run_until_fail(parse(text = 'check_that(is_true(TRUE), feedback = list(message = NULL))')))
})

