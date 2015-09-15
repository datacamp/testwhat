source("testing-framework.R")

# Call tests for test-function

test_call(
  name = 'test_no_name' , 
  msg = 'receives name argument',
  passes = function() {
    expect_error(test_function_v2(), "argument \"name\" is missing")
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
      test_function_v2("summary")
      expect_fail(test_function_v2("dim"))
    },
    function() {
      expect_fail(
        test_function_v2("dim", not_called_msg ="This is the not called message"), 
        "This is the not called message")
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
      test_function_v2("summary", "object")
      expect_fail(test_function_v2("dim", "x"))
      test_function_v2("rep", "x")
      test_function_v2("dnorm", c("x", "mean"))
      expect_fail(test_function_v2("dnorm", c("x", "mean"), allow_extra = FALSE))
      expect_fail(test_function_v2("mean", c("x", "na.rm")))
    },
    function() {
      expect_fail(
        test_function_v2("dim", "x", incorrect_msg = "This is the incorrect message"), 
        "This is the incorrect message")
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
    test_function_v2("var", "x")
    expect_fail(test_function_v2("lm", "formula"))
    expect_fail(test_function_v2("var", "x", eq_condition = "equal"))
    expect_fail(test_function_v2("lm", "formula", eq_condition = "equal"))
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
    test_function_v2("var", "x")
    expect_fail(test_function_v2("lm", "formula"))
    expect_fail(test_function_v2("var", "x", eq_condition = "equal"))
    expect_fail(test_function_v2("lm", "formula", eq_condition = "equal"))
    test_function_v2("var", eval = FALSE)
    test_function_v2("lm", eval = FALSE)
    test_function_v2("var", eval = FALSE, eq_condition = "equal")
    test_function_v2("lm", eval = FALSE, eq_condition = "equal")
  }
)

# Scenario 4: differences between identical and equal
test_scenario(
  name = 'test_identical',
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
  msg = 'difference between identical and equal for arguments', 
  passes = function() {
    test_function_v2("var", "x", eq_condition = "equal")
    test_function_v2("mean", "formula", eq_condition = "equal")
    expect_fail(test_function_v2("var", "x", eq_condition = "identical"))
    test_function_v2("mean", "formula", eq_condition = "identical")
    test_function_v2("var", eval = FALSE, eq_condition = "equal")
    test_function_v2("mean", eval = FALSE, eq_condition = "equal")
    test_function_v2("var", eval = FALSE, eq_condition = "identical")
    test_function_v2("mean", eval = FALSE, eq_condition = "identical")
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
    test_function_v2("mean", "x")
    expect_fail(test_function_v2("var", "x", eval = FALSE))
  }
)

# Scenario 6: check the use of allow_extra
test_scenario(
  name = 'test_allow_extra', 
  student = '
  mean(1:10, trim = 0.9)
  var(1:5, 6:10)',
  solution = '
  mean(1:10)
  var(1:5, 6:10)',
  msg = 'checks whether the allow_extra argument works properly',
  passes = function() {
    test_function_v2("mean", "x")
    test_function_v2("mean", c("x", "trim"), allow_extra = FALSE)
    expect_fail(test_function_v2("mean", "x", allow_extra = FALSE))
    test_function_v2("var", c("x", "y"), allow_extra = FALSE)
  }
)

# Scenario 7: check the use of ignore
test_scenario(
  name = 'test_ignore', 
  student = '
  mean(1:10, trim = 0.9, na.rm = FALSE)
  var(1:5, 6:10)',
  solution = '
  mean(1:10)
  var(1:5, 11:15)',
  msg = 'checks whether the ignore argument works properly',
  passes = function() {
    test_function_v2("mean", "x", allow_extra = FALSE, ignore = c("trim", "na.rm"))
    expect_fail(test_function_v2("mean", "x", allow_extra = FALSE, ignore = "na.rm"))
    expect_fail(test_function_v2("mean", "x", allow_extra = FALSE, ignore = "na.rm"))
    test_function_v2("var", "x", allow_extra = FALSE, ignore = "y")
  }
)

# Scenario 8: check the use of new index 
test_scenario(
  name = 'test_index_new', 
  student = '
  a <- "test"
  mean(1:10, trim = 0.9, na.rm = FALSE)
  mean(1:5, trim = 0.8)',
  solution = '
  a <- "test"
  mean(1:10, trim = 0.9)
  mean(1:9)',
  msg = 'checks whether the index argument works properly',
  passes = function() {
    test_function_v2("mean", "x", index = 1)
    expect_fail(test_function_v2("mean", "x", index = 2))
    expect_fail(test_function_v2("mean", c("x", "trim"), allow_extra = FALSE, index = 1))
    expect_fail(test_function_v2("mean", "x", allow_extra = FALSE, index = 1))
    test_function_v2("mean", c("x", "trim"))
  }
)

