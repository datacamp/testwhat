scen <- list(
  list(
    type = "NormalExercise", 
    student = "b = 3; c = 5", 
    solution = "a = 2.5; b = 3; c = 29", 
    pass = list(
      test_standard_pass = list(
        long = "test succeeds if the first check is true", 
        sct = "test_or(test_object('a'), test_object('b'), test_object('c'))"
      )
    ),
    fail = list(
      test_standard_fail = list(
        long = "test fails if all the checks are false",
        sct = "test_or(test_object('a'), test_object('c'))",
        message = ".*define .*a"
      ),
      test_fail_with_msg = list(
        long = "test fails if all the checks are false with feedback msg",
        sct = "test_or(test_object('a'), test_object('c'), incorrect_msg = 'incorrect, my man')",
        message = "incorrect, my man"
      )
    )
  ),
  list(
    type = "NormalExercise", 
    student = "temp <- 3 + 5", 
    solution = "var <- 3 + 5", 
    pass = list(
      test_standard_pass = list(
        long = "test succeeds if the first check is true", 
        sct = "test_or(test_an_object('var', 'Define 3 + 5 to a variable'), test_output_contains('3+5'))"
      )
    )
  )
)