source("testing-framework.R")

# Scenarios for test-function

# Scenario 1: just check for a for loop
test_scenario(
  name = 'test_basic_check',
  student = '
  for (i in 1:10) {
    rpois(10,i)
  }',
  solution = '
  for (i in 1:3) {
    rnorm(10,i)
  }',
  msg = 'check whether basic test works',
  passes = function() {
    test_for_loop()
  }
)

# Scenario 2: just check for a for loop when there is none
test_scenario(
  name = 'test_basic_check_fails',
  solution = '
  for (i in 1:3) {
    rnorm(10,i)
  }',
  msg = 'check whether basic test fails if there is no for statement',
  passes = function() {
    expect_error(test_for_loop())
  }
)

# Scenario 3: check if cond_test is checked correctly
test_scenario(
  name = 'test_cond_test',
  student = '
  for (i in 1:10) {
    rpois(10,i)
  }',
  solution = '
  for (i in 1:10) {
    rpois(10,i)
  }',
  msg = 'check if cond_test is checked correctly',
  passes = function() {
    test_for_loop(
      cond_test = {
        test_object("i")
      }
    )
  }
)

# Scenario 4: check cond_test fail
test_scenario(
  name = 'test_cond_test_fails',
  student = '
  for (i in 1:10) {
    rpois(10,i)
  }',
  solution = '
  for (i in 1:3) {
    rpois(10,i)
  }',
  msg = 'check if cond_test fails correctly',
  passes = function() {
    expect_error(
      test_for_loop(
        cond_test = {
          test_object("i")
        }
      )
    )
    expect_error(
      test_for_loop(
        cond_test = {
          test_object("n")
        }
      )
    )
  }
)

# Scenario 5: check if expr_test is checked correctly
test_scenario(
  name = 'test_expr_test',
  student = '
  for (i in 1:10) {
    rpois(10,i)
  }',
  solution = '
  for (i in 1:10) {
    rpois(10,i)
  }',
  msg = 'check if cond_test is checked correctly',
  passes = function() {
    test_for_loop(
      expr_test = {
        test_function("rpois", c("n","lambda"))
      }
    )
    test_for_loop(
      expr_test = {
        test_function("rpois", c("n","lambda"))
        # You're actually just checking the value i after the last iteration
        test_object("i")
      }
    )
  }
)

# Scenario 6: check check_test fail
test_scenario(
  name = 'test_expr_test_fails',
  student = '
  for (i in 1:10) {
    rpois(3,i)
  }',
  solution = '
  for (i in 1:10) {
    rpois(10,i)
    rnorm(10,i)
  }',
  msg = 'check if expr_test fails correctly',
  passes = function() {
    expect_error(
      test_for_loop(
        expr_test = {
          test_function("rpois", c("n","lambda"))
        }
      )
    )
    expect_error(
      test_for_loop(
        expr_test = {
          test_function("rnorm")
        }
      )
    )
  }
)