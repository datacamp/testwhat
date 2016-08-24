context("utils")

test_that("backwards comp", {
  e <- new.env(globalenv())
  tw$set(state = RootState$new(solution_env = e,
                               student_code = "abc",
                               solution_code = "abc"))
  expect_equal(get_solution_env(), e)
  expect_equal(get_student_code(), "abc")
  expect_equal(get_solution_code(), "abc")
})


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
