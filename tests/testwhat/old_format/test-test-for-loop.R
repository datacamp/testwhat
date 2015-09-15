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
    expect_fail(test_for_loop())
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
  name = list(
    'test_cond_test_fails',
    'test_cond_test_msg'),
  student = '
  for (i in 1:10) {
    rpois(10,i)
  }',
  solution = '
  for (i in 1:3) {
    rpois(10,i)
  }',
  msg = list(
    'check if cond_test fails correctly',
    'check if cond_test fail message is displayed correctly'),
  passes = list(
    function() {
      expect_fail(
        test_for_loop(
          cond_test = {
            test_object("i")
          }
        )
      )
      expect_fail(
        test_for_loop(
          cond_test = {
            test_object("n")
          }
        )
      )
    },
    function() {
      expect_fail(
        test_for_loop(
          cond_test = {
            test_object("i",incorrect_msg = "Wrong condition")
          }
        ),
        "Wrong condition"
      )
    }
  )
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
  msg = 'check if expr_test is checked correctly',
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

# Scenario 6: check expr_test fail
test_scenario(
  name = list(
    'test_expr_test_fails',
    'test_expr_test_msg'),
  student = '
  for (i in 1:10) {
    rpois(3,i)
  }',
  solution = '
  for (i in 1:10) {
    rpois(10,i)
    rnorm(10,i)
  }',
  msg = list(
    'check if expr_test fails correctly',
    'check if cond_test fail message is displayed correctly'),
  passes = list(
    function() {
      expect_fail(
        test_for_loop(
          expr_test = {
            test_function("rpois", c("n","lambda"))
          }
        )
      )
      expect_fail(
        test_for_loop(
          expr_test = {
            test_function("rnorm")
          }
        )
      )
    },
    function() {
      expect_fail(
        test_for_loop(
          expr_test = {
            test_function("rpois", c("n","lambda"), incorrect_msg = "Wrong expression")
          }
        ),
        "Wrong expression"
      )
    }
  )
)

# Scenario 7: check if index is checked correctly
test_scenario(
  name = list(
    'test_index',
    'test_not_found_msg'),
  student = '
  for (i in 3:8) {
    rpois(2,i)
  }
  a <- "some code here"
  for (n in 3:5) {
    rnorm(5, n*n)
  }',
  solution = '
  for (i in 1:10) {
    rpois(10,i)
  }
  for (n in 3:5) {
    rnorm(5, n*n)
  }',
  msg = list(
    'check if index is checked correctly',
    'check if not_found_msg is displayed correctly'),
  passes = list(
    function() {
      test_for_loop(2,
        cond_test = {
          test_object("n")
        },
        expr_test = {
          test_function("rnorm", c("n","lambda"))
        }
      )
      expect_fail(
        test_for_loop(1,
          expr_test = {
            test_function("rpois", c("n","lambda"))
  
          }
        )
      )
    },
    function() {
      expect_fail(
        test_for_loop(3, not_found_msg = "Too much looooooops"),
        "Too much looooooops"
      )
    }
  )
)
