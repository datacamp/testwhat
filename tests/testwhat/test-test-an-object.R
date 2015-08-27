source("testing-framework.R")

# Call tests for test-an-object

test_call(
  name = 'test_no_name' , 
  msg = 'receives name argument',
  passes = function() {
    expect_error(test_an_object(), "argument \"undefined_msg\" is missing")
  }
)

# Scenarios for test-an-object

# Scenario 1: one object is equivalent, the other isn't
test_scenario(
  name = list(
    'test_equivalence',
    'test_undefined_msg'),
  student = '
  var.equiv <- 3
  var.not_equiv <- 4',
  solution = '
  var.equiv <- 3
  var.other_equiv <- 4
  var.not_here <- 2
  var.not_equiv <- 5',
  msg = list(
    'handle equivalence correctly',
    'test on undefined message'),
  passes = list(
    function() {
      expect_error(test_an_object("var.not_equiv", "Testing"))
      test_an_object("var.equiv", "Testing")
      test_an_object("var.other_equiv", "Testing")
    },
    function() {
      expect_error(
        test_an_object("var.not_here", undefined_msg="This is the undefined message"), 
        "This is the undefined message")
      expect_error(
        expect_error(
          test_an_object("var.not_here", undefined_msg="This is the undefined message"), 
          "This is not the undefined message")
      )
    })
)

# Scenario 2: test difference equal and equivalent
test_scenario(
  name = 'test_equal',
  student = '
  df.equiv <- data.frame(a=c(10,11,12), b=c(4,5,6))
  df.not_equiv <- data.frame(a=c(1,2,3), b=c(4,5,6))',
  solution = '
  df.equiv <- data.frame(c=c(1,2,3), d=c(4,5,6))
  df.not_equiv <- data.frame(c=c(7,8,9), d=c(4,5,6))',
  msg = 'difference between equals and equivalent',
  passes = function() {
    expect_error(test_an_object("df.equiv", "Testing", eq_condition = "equal"))
    test_an_object("df.equiv", "Testing")
    expect_error(test_an_object("df.not_equiv", "Testing", eq_condition = "equal"))
    expect_error(test_an_object("df.not_equiv", "Testing"))
  }
)

# Scenario 3: test difference between identical and equal
test_scenario(
  name = 'test_identical',
  student = '
  var.iden <- 3 + 4.4e-8
  var.equal <- 4',
  solution = '
  var.iden <- 4
  var.equal <- 3',
  msg = 'difference between equals and identical',
  passes = function() {
    expect_error(test_an_object("var.equal", "Testing", eq_condition = "identical"))
    test_an_object("var.iden", "Testing", eq_condition = "identical")
    test_an_object("var.equal", "Testing")
  }
)


