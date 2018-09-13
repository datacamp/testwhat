context("override_solution")

test_that("override_solution", {
  s <- setup_state('x <- 2', 'x <- 2')
  expect_equal(get("x", envir = s$get("student_env")), 2)
  expect_equal(get("x", envir = s$get("solution_env")), 2)
  s2 <- override_solution(s, x = 3)
  expect_equal(get("x", envir = s2$get("student_env")), 2)
  expect_equal(get("x", envir = s2$get("solution_env")), 3)
  s3 <- override_solution_env(s, x = 3)
  expect_equal(get("x", envir = s3$get("student_env")), 2)
  expect_equal(get("x", envir = s3$get("solution_env")), 3)
  
  code <- 'mean(1:3)'
  s <- setup_state(code, code)
  expect_equal(s$get("student_code"), code)
  expect_equal(s$get("solution_code"), code)
  s2 <- override_solution(s, code = 'sum(1:3)')
  expect_equal(s2$get("student_code"), code)
  expect_equal(s2$get("solution_code"), 'sum(1:3)')
  s3 <- override_solution_code(s, code = 'sum(1:3)')
  expect_equal(s3$get("student_code"), code)
  expect_equal(s3$get("solution_code"), 'sum(1:3)')
  
  expect_error(override_solution(s, code = 'sum(1:3'))
})

