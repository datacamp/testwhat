source("testing-framework.R")

# Call tests for test-function

test_call(
  name = 'test_no_name' , 
  msg = 'receives name argument',
  passes = function() {
    expect_error(test_function(), "argument \"name\" is missing")
  }
)

# Scenarios for test-function

# Scenario 1: just check for the use of function
test_scenario(
  name = list(
    'test_basic_check',
    'test_not_called_msg'),
  student = '
  summary(c(1,2,3,4))',
  solution = '
  summary(c(1,2,3,4),c(1,2,3,4))
  dim(c(1,2,3))',
  msg = list(
    'basic function check',
    'check if not_called_msg feedback is correct'),
  passes = list(
    function() {
      test_function("summary")
      expect_error(test_function("dim"))
    },
    function() {
      expect_error(
        test_function("dim", not_called_msg ="This is the not called message"), 
        "This is the not called message")
      expect_error(
        expect_error(
          test_function("dim", not_called_msg ="This is the not called message"), 
          "This is not the not called message")
      )
    })
)

# Scenario 2: check the use of certain arguments
test_scenario(
  name = list(
    'test_arguments',
    'test_incorrect_msg'),
  student = '
  summary(c(1,2,3,4))
  dim(c(1,2,3))
  rep(1, 4)
  dnorm(1, 10, 5)
  mean(c(1,2,3), na.rm = FALSE)',
  solution = '
  summary(c(1,2,3,4))
  dim(c(1,2,3,4))
  rep(1, 20)
  dnorm(1,10)
  mean(c(1,2,3), na.rm = TRUE)',
  msg = list(
    'checks on the function arguments',
    'check if incorrect_msg feedback is correct'),
  passes = list(
    function() {
      test_function("summary", "object")
      expect_error(test_function("dim", "x"))
      test_function("rep", "x")
      test_function("dnorm", c("x", "mean"))
      expect_error(test_function("dnorm", c("x", "mean"), allow_extra = FALSE))
      expect_error(test_function("mean", c("x", "na.rm")))
    },
    function() {
      expect_error(
        test_function("dim", "x", incorrect_msg = "This is the incorrect message"), 
        "This is the incorrect message")
      expect_error(
        expect_error(
          test_function("dim", "x", incorrect_msg = "This is the incorrect message"), 
          "This is not the incorrect message")
      )
    })
)

# Scenario 3: differences between equivalent and equal
test_scenario(
  name = 'test_equal',
  student = '
  df.equiv <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
  var(df.equiv)
  df.not_equiv <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
  lm(df.not_equiv)',
  solution = '
  df.equiv <- data.frame(c = c(1, 2, 3), d = c(4, 5, 6))
  var(df.equiv)
  df.not_equiv <- data.frame(c = c(7, 8, 9), d = c(4, 5, 6))
  lm(df.not_equiv)',
  msg = 'difference between equal and equivalent for arguments', 
  passes = function() {
    test_function("var", "x")
    expect_error(test_function("lm", "formula"))
    expect_error(test_function("var", "x", eq_condition = "equal"))
    expect_error(test_function("lm", "formula", eq_condition = "equal"))
  }
)

# Scenario 3: differences between equivalent and equal
test_scenario(
  name = 'test_equal',
  student = '
  df.equiv <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
  var(df.equiv)
  df.not_equiv <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
  lm(df.not_equiv)',
  solution = '
  df.equiv <- data.frame(c = c(1, 2, 3), d = c(4, 5, 6))
  var(df.equiv)
  df.not_equiv <- data.frame(c = c(7, 8, 9), d = c(4, 5, 6))
  lm(df.not_equiv)',
  msg = 'difference between equal and equivalent for arguments', 
  passes = function() {
    test_function("var", "x")
    expect_error(test_function("lm", "formula"))
    expect_error(test_function("var", "x", eq_condition = "equal"))
    expect_error(test_function("lm", "formula", eq_condition = "equal"))
    test_function("var", eval = FALSE)
    test_function("lm", eval = FALSE)
    test_function("var", eval = FALSE, eq_condition = "equal")
    test_function("lm", eval = FALSE, eq_condition = "equal")
  }
)

# Scenario 4: differences between identical and equal
test_scenario(
  name = 'test_equal',
  student = '
  var.iden <- 3
  var(var.iden)
  var.equal <- 4
  mean(var.equal)',
  solution = '
  var.iden <- 3 + 4.4e-8
  var(var.iden)
  var.equal <- 4
  mean(var.equal)',
  msg = 'difference between equal and equivalent for arguments', 
  passes = function() {
    test_function("var", "x", eq_condition = "equal")
    test_function("mean", "formula", eq_condition = "equal")
    expect_error(test_function("var", "x", eq_condition = "identical"))
    test_function("mean", "formula", eq_condition = "identical")
    test_function("var", eval = FALSE, eq_condition = "equal")
    test_function("mean", eval = FALSE, eq_condition = "equal")
    test_function("var", eval = FALSE, eq_condition = "identical")
    test_function("mean", eval = FALSE, eq_condition = "identical")
  }
)

# Scenario 5: check the use of eval
test_scenario(
  name = 'test_eval', 
  student = '
  var.a <- c(302, 305, 309)
  mean(var.a)
  var(var.a)',
  solution = '
  var.b <- c(302, 305, 309)
  mean(var.b)
  var(var.b)',
  msg = 'checks whether the eval argument works properly',
  passes = function() {
    test_function("mean", "x")
    expect_error(test_function(test_function("var", "x", eval = FALSE)))
  }
)

