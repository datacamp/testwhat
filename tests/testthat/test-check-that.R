context("check_that")

test_that("check_that works as it should", {
  tw$set(reporter = DC_reporter$new())
  expect_error(run_until_fail(check_that(is_true(TRUE))))

  tw$set(reporter = DC_reporter$new())
  expect_error(run_until_fail(check_that(is_true(TRUE), feedback = NULL)))

  tw$set(reporter = DC_reporter$new())
  expect_error(run_until_fail(check_that(is_true(TRUE), feedback = list(not_message = "test"))))

  tw$set(reporter = DC_reporter$new())
  expect_error(run_until_fail(check_that(is_true(TRUE), feedback = list(message = NULL))))
})

test_that("backwards compatibility", {
  tw$set(reporter = DC_reporter$new())
  tw$set(state = RootState$new(test_env = environment()))
  expect_true(run_until_fail(test_what(expect_true(TRUE), feedback = 'testtest')))
  expect_false(run_until_fail(test_what(expect_true(FALSE), feedback = 'testtest')))
})