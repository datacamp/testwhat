context("messages")

test_that("trim works as expected", {
  x <- "hello. this? is! a. test?! ok? fine."
  res <- capitalize(x)
  expected <- "Hello. This? Is! A. Test?! Ok? Fine."
  expect_equal(res, expected)
})

test_that("language is deprecated", {
  expect_message(set_language("en"), "Different languages are no longer supported in testwhat")
})