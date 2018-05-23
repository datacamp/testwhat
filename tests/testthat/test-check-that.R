context("check_that")

test_that("check_that works as it should", {
  expect_error(run_until_fail(check_that(is_true(TRUE))))
  expect_error(run_until_fail(check_that(is_true(TRUE), feedback = NULL)))
  expect_error(run_until_fail(check_that(is_true(TRUE), feedback = list(not_message = "test"))))
  expect_error(run_until_fail(check_that(is_true(TRUE), feedback = list(message = NULL))))
})

test_that("backwards compatibility", {
  tw$set(state = RootState$new(test_env = environment()))
  expect_true(run_until_fail(test_what(expect_true(TRUE), feedback = 'testtest'))$correct)
  expect_false(run_until_fail(test_what(expect_true(FALSE), feedback = 'testtest'))$correct)
})
