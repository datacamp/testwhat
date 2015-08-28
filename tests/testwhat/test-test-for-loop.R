source("testing-framework.R")

# Call tests for test-function

test_call(
  name = 'test_no_name' , 
  msg = 'receives name argument',
  passes = function() {
    expect_error(test_for_loop(), "argument \"name\" is missing")
  }
)