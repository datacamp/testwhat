context("utils")

test_that("accessor works", {
  tw <- tw_accessors()
  tw$initialize(data = list(a = 2, b = 3, c = 4))
  expect_equal(tw$get("a"), 2)
  expect_equal(tw$get("b"), 3)
  expect_equal(tw$get("c"), 4)
  expect_equal(tw$get("d"), NULL)
  tw$set(d = 5)
  expect_equal(tw$get("a"), 2)
  expect_equal(tw$get("b"), 3)
  expect_equal(tw$get("c"), 4)
  expect_equal(tw$get("d"), 5)
  tw$clear()
  expect_equal(tw$get(), list())
  expect_equal(tw$get("a"), NULL)
})

test_that("get_num_hits works", {
  expect_equal(get_num_hits('a', 'abc', TRUE), 1)
  expect_equal(get_num_hits('a', 'aba', TRUE), 2)
})

test_that("check_defined works", {
  testenv <- new.env()
  assign("x", 5, envir = testenv)
  expect_silent(check_defined("x", testenv))
  expect_error(check_defined("y", testenv))
})

test_that("remove_comments works", {
  cases <- c("# c\n4", "#c\n4", "4\n#c", "4\n\n#c", "#c\n4\n# c\n")
  for (case in cases) {
    no_comments_stripped = stringr::str_trim(remove_comments(case))
    expect_equal(no_comments_stripped, "4")
  }
  cases2 <- c("#", "#\n")
  for (case in cases2) {
    expect_equal(remove_comments("#"), "")  
  }
})

test_that("fail_if_v2_only", {
  withr::with_envvar(c(TESTWHAT_V2_ONLY = ''), {
    expect_equal(fail_if_v2_only(), NULL)
  })
  withr::with_envvar(c(TESTWHAT_V2_ONLY = '0'), {
    expect_equal(fail_if_v2_only(), NULL)
  })
  withr::with_envvar(c(TESTWHAT_V2_ONLY = '1'), {
    expect_error(fail_if_v2_only())
  })
  withr::with_envvar(c(TESTWHAT_V2_ONLY = '1'), {
    expect_error(fail_if_v2_only('test'), regexp = 'test')
  })
})

test_that("unpipe works with a single pipe", {
  expected <- quote(select(cars, speed))
  actual <- unpipe(quote(cars %>% select(speed)))
  expect_identical(actual, expected)
})

test_that("unpipe works with a single pipe and dot", {
  expected <- quote(select(cars, speed))
  actual <- unpipe(quote(cars %>% select(., speed)))
  expect_identical(actual, expected)
})

test_that("unpipe works with two pipes", {
  expected <- quote(filter(select(cars, speed), speed > 20))
  actual <- unpipe(quote(cars %>% select(speed) %>% filter(speed > 20)))
  expect_identical(actual, expected)
})

test_that("unpipe works with two pipes and dot", {
  expected <- quote(filter(select(cars, speed), speed > 20))
  actual <- unpipe(quote(cars %>% select(., speed) %>% filter(., speed > 20)))
  expect_identical(actual, expected)
})

test_that("unpipe works with qualifed function names", {
  expected <- quote(dplyr::filter(dplyr::select(cars, speed), speed > 20))
  actual <- unpipe(quote(cars %>% dplyr::select(speed) %>% dplyr::filter(speed > 20)))
  expect_identical(actual, expected)
})

test_that("unpipe works with multiple dots", {
  expected <- quote(setNames(letters, letters))
  actual <- unpipe(quote(letters %>% setNames(., .)))
  expect_identical(actual, expected)
})
