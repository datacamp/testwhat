scen <- list(
  list(
    type = "NormalExercise", 
    student = "\n  3 + 3", 
    solution = "\n  3 + 3", 
    pass = list(
      test_fail_pass = list(
        long = "test succeeds if code contains no error", 
        sct = "test_error()"
      )
    )
  ),
  list(
    type = "NormalExercise", 
    student = "\n  \"a\" + 3", 
    solution = "\n  3 + 3", 
    fail = list(
      test_fail_fail = list(
        long = "test fails if code contains an error", 
        sct = "test_error()"
      ),
      test_incorrect_msg = list(
        long = "test fails if code contains an error", 
        sct = "test_error(\"this is the incorrect msg\")",
        message = "this is the incorrect msg"
      )
    )
  )
)