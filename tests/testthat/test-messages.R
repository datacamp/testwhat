context("messages")

test_that("trim works as expected", {
  x <- "hello. this? is! a. test?! ok? fine."
  res <- capitalize(x)
  expected <- "Hello. This? Is! A. Test?! Ok? Fine."
  expect_equal(res, expected)
})