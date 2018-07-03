context("message-utils")

test_that("yaml_option_desc", {
  expect_equal(yaml_option_desc("a"), "`a`")
  expect_equal(yaml_option_desc(c("a", "b")), "`a:b`")
  expect_equal(yaml_option_desc(c("a", "b", "c")), "`a:b:c`")
})