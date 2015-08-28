source("testing-framework.R")

# Scenarios for test-function

# Scenario 1: just check for a for loop
test_scenario(
  name = 'test_basic_check',
  pre_ex = 'i = 1',
  student = '
  while (i < 10) {
    rpois(10,i)
    i = i + 1
  }',
  solution = '
  while (i < 3) {
    rnorm(10,i)
    i = i + 1
  }',
  msg = 'check whether basic test works',
  passes = function() {
    test_while_loop()
  }
)

# Scenario 2: just check for a for loop when there is none
test_scenario(
  name = 'test_basic_check_fails',
  pre_ex = 'i = 1',
  solution = '
  while (i < 3) {
    rnorm(10,i)
    i = i + 1
  }',
  msg = 'check whether basic test fails if there is no for statement',
  passes = function() {
    expect_error(test_while_loop())
  }
)

# Scenario 3: check if cond_test is checked correctly
test_scenario(
  name = 'test_cond_test',
  pre_ex = 'i = 1',
  student = '
  while (i < 10) {
    rpois(10,i)
    i = i + 1
  }',
  solution = '
  while (i < 10) {
    rpois(10,i)
    i = i + 1
  }',
  msg = 'check if cond_test is checked correctly',
  passes = function() {
    test_while_loop(
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
  pre_ex = 'i = 1',
  student = '
  while (i < 10) {
    rpois(10,i)
    i = i + 1
  }',
  solution = '
  while (i < 3) {
    rpois(10,i)
    i = i + 1
  }',
  msg = list(
    'check if cond_test fails correctly',
    'check if cond_test fail message is displayed correctly'),
  passes = list(
    function() {
      expect_error(
        test_while_loop(
          cond_test = {
            test_object("i")
          }
        )
      )
      expect_error(
        test_while_loop(
          cond_test = {
            test_object("n")
          }
        )
      )
    },
    function() {
      expect_error(
        test_while_loop(
          cond_test = {
            test_object("i",incorrect_msg = "Wrong condition")
          }
        ),
        "Wrong condition"
      )
      expect_error(
        expect_error(
          test_while_loop(
            cond_test = {
              test_object("i",incorrect_msg = "Wrong condition")
            }
          ),
          "Wrong oops condition"
        )
      )
    }
  )
  )

# Scenario 5: check if expr_test is checked correctly
test_scenario(
  name = 'test_expr_test',
  pre_ex = 'i = 1',
  student = '
  while (i < 10) {
    rpois(10,i)
    i = i + 1
  }',
  solution = '
  while (i < 10) {
    rpois(10,i)
    i = i + 1
  }',
  msg = 'check if cond_test is checked correctly',
  passes = function() {
    test_while_loop(
      expr_test = {
        test_function("rpois", c("n","lambda"))
      }
    )
    test_while_loop(
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
  pre_ex = 'i = 1',
  student = '
  while (i < 10) {
    rpois(3,i)
    i = i + 1
  }',
  solution = '
  while (i < 10) {
    rpois(10,i)
    rnorm(10,i)
    i = i + 1
  }',
  msg = list(
    'check if expr_test fails correctly',
    'check if cond_test fail message is displayed correctly'),
  passes = list(
    function() {
      expect_error(
        test_while_loop(
          expr_test = {
            test_function("rpois", c("n","lambda"))
          }
        )
      )
      expect_error(
        test_while_loop(
          expr_test = {
            test_function("rnorm")
          }
        )
      )
    },
    function() {
      expect_error(
        test_while_loop(
          expr_test = {
            test_function("rpois", c("n","lambda"), incorrect_msg = "Wrong expression")
          }
        ),
        "Wrong expression"
      )
      expect_error(
        expect_error(
          test_while_loop(
            expr_test = {
              test_function("rpois", c("n","lambda"), incorrect_msg = "Wrong expression")
            }
          ),
          "Wrong oops expression"
        )
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
  i = 3
  n = 3
  while (i < 8) {
    rpois(2,i)
    i = i + 1
  }
  a <- "some code here"
  while (n < 5) {
    rnorm(5, n*n)
    n = n + 1
  }',
  solution = '
  i = 1
  n = 3
  while (i < 10) {
    rpois(10,i)
    i = i + 1
  }
  while (n < 5) {
    rnorm(5, n*n)
    n = n + 1
  }',
  msg = list(
    'check if index is checked correctly',
    'check if not_found_msg is displayed correctly'),
  passes = list(
    function() {
      test_while_loop(2,
                    cond_test = {
                      test_object("n")
                    },
                    expr_test = {
                      test_function("rnorm", c("n","lambda"))
                    }
      )
      expect_error(
        test_while_loop(1,
                      expr_test = {
                        test_function("rpois", c("n","lambda"))
                        
                      }
        )
      )
    },
    function() {
      expect_error(
        test_while_loop(3, not_found_msg = "Too much looooooops"),
        "Too much looooooops"
      )
      expect_error(
        expect_error(
          test_while_loop(3, not_found_msg = "Too much looooooops"),
          "Too much looooooooooooooooooooops"
        )
      )
    }
  )
)