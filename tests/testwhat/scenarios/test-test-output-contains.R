scen <- list(
  list(
    type = "NormalExercise",
    student = "print('some crazy output')",
    pass = list(
      test_output_contains_pass = list(
        long = "test succeeds if output contains the string",
        sct = "test_output_contains(\"print('some crazy output')\")"
      )
    ),
    fail = list(
      test_output_contains_fail = list(
        long = "test fails if output doesn't contain the string",
        sct = "test_output_contains(\"print('some wrong output')\")"
      )
    )
  ),
  list(
    type = "NormalExercise",
    student = "print('some crazy output'); print('some crazy output'); print('some crazy output')",
    pass = list(
      test_output_contains_pass_with_times = list(
        long = "test succeeds if output contains the string 3 times",
        sct = "test_output_contains(\"print('some crazy output')\", times = 3)"
      )
    ),
    fail = list(
      test_output_contains_fails_with_times = list(
        long = "test fails if output contains the string 3 times (need 4)",
        sct = "test_output_contains(\"print('some crazy output')\", times = 4, incorrect_msg = \"NOOOO\")",
        message = "NOOOO"
      )
    )
  )
)