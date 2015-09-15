source("testing-framework.R")

# Call tests for test-object

test_call(
  name = 'test_no_name' , 
  msg = 'receives name argument',
  passes = function() {
    expect_error(test_object(), "argument \"name\" is missing")
  }
)

# Scenarios for test-object

# Scenario 1: one object is equivalent, the other isn't
test_scenario(
  name = list(
    'test_equivalence',
    'test_undefined_msg',
    'test_incorrect_msg'),
  student = '
    var.equiv <- 3
    var.not_equiv <- 4',
  solution = '
    var.equiv <- 3
    var.not_equiv <- 3
    var.not_here <- 2',
  msg = list(
    'handle equivalence correctly',
    'test on undefined message',
    'test on incorrect message'),
  passes = list(
    function() {
      expect_fail(test_object("var.not_equiv"))
      test_object("var.equiv")
    },
    function() {
      expect_fail(
        test_object("var.not_here", undefined_msg="This is the undefined message"), 
        "This is the undefined message")
    },
    function() {
      expect_fail(
        test_object("var.not_equiv", incorrect_msg = "This is the incorrect message"), 
        "This is the incorrect message")
    })
)

# Scenario 2: test difference equal and equivalent
test_scenario(
  name = 'test_equal',
  student = '
    df.equiv <- data.frame(a=c(1,2,3), b=c(4,5,6))
    df.not_equiv <- data.frame(a=c(1,2,3), b=c(4,5,6))',
  solution = '
    df.equiv <- data.frame(c=c(1,2,3), d=c(4,5,6))
    df.not_equiv <- data.frame(c=c(7,8,9), d=c(4,5,6))',
  msg = 'difference between equals and equivalent',
  passes = function() {
    expect_fail(test_object("df.equiv", eq_condition = "equal"))
    test_object("df.equiv")
    expect_fail(test_object("df.not_equiv", eq_condition = "equal"))
    expect_fail(test_object("df.not_equiv"))
  }
)

# Scenario 3: test difference between identical and equal
test_scenario(
  name = 'test_identical',
  student = '
    var.iden <- 3
    var.equal <- 3 + 4.4e-8',
  solution = '
    var.iden <- 3
    var.equal <- 3',
  msg = 'difference between equals and identical',
  passes = function() {
    expect_fail(test_object("var.equal", eq_condition = "identical"))
    test_object("var.iden", eq_condition = "identical")
    test_object("var.equal", eq_condition = "equal")
  }
)

# Scenario 4: test on eval argument
test_scenario(
  name = 'test_eval',
  student = '
    var <- 3
    var.other <- 4',
  solution = '
    var <- 5
    var.other <- 3',
  msg = 'test on eval parameter',
  passes = function() {
    expect_fail(test_object("var"))
    test_object("var", eval = FALSE)
    expect_fail(test_object("var.not_here", eval=FALSE))
  }
)
