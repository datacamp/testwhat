source("testing-framework.R")

# Call tests for test-correct

test_call(
  name = 'test_no_check_code' , 
  msg = 'receives check_code argument',
  passes = function() {
    expect_error(test_correct(), "argument \"check_code\" is missing")
  }
)

test_call(
  name = 'test_no_diagnose_code' , 
  msg = 'receives diagnose_code argument',
  passes = function() {
    expect_error(test_correct(NULL), "argument \"diagnose_code\" is missing")
  }
)

# Scenarios for test-correct

# Scenario 1: success if check is correct and diagnose is correct
test_scenario(
  name = 'test_check_correct',
  student = '
  var.equiv <- 3
  var.not_equiv <- 4
  mean(var.equiv)',
  solution = '
  var.equiv <- 3
  var.not_equiv <- 3
  var.not_here <- 2
  mean(var.equiv)',
  msg = 'test succeeds if check is correct and diagnose is correct',
  passes = function() {
    test_correct({
      test_object("var.equiv")
    }, {
      test_function("mean")
    })
  }
)

# Scenario 2: success if check is correct and diagnose is false
test_scenario(
  name = 'test_diagnose_false',
  student = '
  var.equiv <- 3
  var.not_equiv <- 4
  mean(var.not_equiv)',
  solution = '
  var.equiv <- 3
  var.not_equiv <- 3
  var.not_here <- 2
  mean(var.not_equiv)',
  msg = 'test succeeds if check is correct and diagnose is false',
  passes = function() {
    test_correct({
      test_object("var.equiv")
      test_function("mean")
    }, {
      test_function("mean", "x")
    })
  }
)

# Scenario 3: fail if check is false and diagnose is correct
test_scenario(
  name = 'test_check_false',
  student = '
  var.equiv <- 3
  var.not_equiv <- 4
  mean(var.not_equiv)',
  solution = '
  var.equiv <- 3
  var.not_equiv <- 3
  var.not_here <- 2
  mean(var.not_equiv)',
  msg = 'test fails if check is false and diagnose is correct',
  passes = function() {
    expect_fail(
      test_correct({
        test_object("var.equiv")
        test_function("mean", "x", incorrect_msg = "This is the one")
      }, {
        test_function("mean")
      }),
      "This is the one"
    )
    
    expect_fail(
      test_correct({
        test_object("var.not_equiv", incorrect_msg = "Wrong object!")
        test_function("mean", "x", incorrect_msg = "This is the one")
      }, {
        test_object("var.equiv")
      }),
      "Wrong object!"
    )
  }
)

# Scenario 4: right message if check is false and diagnose is false
test_scenario(
  name = 'test_check_diagnose_msg',
  student = '
  var.equiv <- 3
  var.not_equiv <- 4
  mean(var.not_equiv)',
  solution = '
  var.equiv <- 3
  var.not_equiv <- 3
  var.not_here <- 2
  mean(var.not_equiv)',
  msg = 'tests if the correct feedback is send for diangose',
  passes = function() {
    expect_fail(
      test_correct({
        test_object("var.not_equiv", incorrect_msg = "Don't send this")
      }, {
        test_function("mean", "x", incorrect_msg = "Diagnose of the problem")
      }),
      "Diagnose of the problem"
    )
  }
)

# Scenario 5: right message if check is false and diagnose is false 2
test_scenario(
  name = 'test_check_diagnose_msg_2',
  student = '
  var.equiv <- 3
  var.not_equiv <- 4
  mean(var.not_equiv)',
  solution = '
  var.equiv <- 3
  var.not_equiv <- 3
  var.not_here <- 2
  mean(var.not_equiv)',
  msg = 'tests if the correct feedback is send for diangose 2',
  passes = function() {
    expect_fail(
      test_correct({
        test_object("var.not_equiv", incorrect_msg = "Don't send this")
        test_object("var.equiv")
        test_function("mean", "x", incorrect_msg = "Wrong message")
      }, {
        test_function("mean", not_called_msg = "Diagnose of the problem")
        test_function("mean", "x", incorrect_msg = "This is the one!")
      }),
      "This is the one!"
    )
  }
)

# Scenario 6: right message if check is false and diagnose is true 
test_scenario(
  name = 'test_check_msg',
  student = '
  var.equiv <- 3
  var.not_equiv <- 4
  mean(var.not_equiv)',
  solution = '
  var.equiv <- 3
  var.not_equiv <- 3
  var.not_here <- 2
  mean(var.not_equiv)',
  msg = 'tests if the correct feedback is send for check',
  passes = function() {
    expect_fail(
      test_correct({
        test_object("var.not_equiv", incorrect_msg = "It will send this oneeee")
        test_object("var.equiv")
        test_function("mean", "x", incorrect_msg = "Wrong message")
      }, {
        test_function("mean", not_called_msg = "Diagnose of the problem")
      }),
      "It will send this oneeee"
    )
  }
)
